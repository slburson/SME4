;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: greedy-merge.lsp
;;;;    System: SME v4
;;;;    Author: Ken Forbus
;;;;   Created: June 19, 2002 09:48:23
;;;;   Purpose: Greedy merge algorithm
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2013-06-03 18:38:43 -0500 (Mon, 03 Jun 2013) $
;;;;  $LastChangedBy: aml758 $
;;;; ---------------------------------------------------------------------------

(in-package :sme)  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Greedy merge algorithm
;;
;; In a greedy algorithm one starts with the largest partial solution
;; and carries on, adding in all the other partial solutions that will
;; fit, until there are no more.
;;
;; When getting multiple interpretations there are several strategies which
;; can be followed.  For instance,
;; (1) Start with the largest that was not consistent with the initial choice.
;; (2) Start with the last consistent choice, backtrack.
;; Doing (2) recursively backwards from the smallest interpretation would
;; generate an exhaustive merge of all gmaps which could be built using
;; just the first entry as a basis.  Then start again with the second, and
;; so forth...but exhaustive isn't what we want.  Method #1 should typically
;; yield the largest DIFFERENT interpretation from the largest

(defgeneric merge-consistent-kernels (sme &optional filter)
  (:documentation
   "Constructs consistent global mappings from kernels."))

;;; Primary method.
(defmethod merge-consistent-kernels ((sme sme) &optional (filter #'always-false))
  "Constructs consistent global mappings from kernels."
  (greedy-merge sme filter))

(defun greedy-merge (sme &optional (filter #'always-false))
  ;; filter is a procedure.  always-false is basically a no-op.
  ;; Start by doing greedy merge within the root mappings that share
  ;; common base structure.  Then do greedy merge on these results.
  ;; The base greedy merges are exhaustive, which is okay because
  ;; the maximum size is simply n.
  (let ((candidates nil)
        (result nil) 
        (sought-solutions 
         (find-mh-constraint-solutions sme))
        (excluded-mhs (find-mh-exclusions sme)))
    (with-slots (greedy-cutoff greedy-max-# greedy-merge-overlap-only?) 
        (mapping-parameters sme)
      (dolist (entry (partition-mappings-via-roots sme filter))
        (let ((filtered (remove-if #'(lambda (mapping) 
                                       (filtered-or-excluded?
                                        mapping filter excluded-mhs))
                                   (cdr entry))))
          (cond
           (*use-legacy-greedy-merge?*
            (setq candidates
                  (merge 'list 
                         (remove-if #'(lambda (candidate) (find candidate candidates))
                                    (greedy-cream sme filtered 0.0 (length filtered) 
                                      :final-merge? nil))
                         candidates
                         #'> :key #'score)))
           (t
            (dolist (new-candidate (greedy-cream sme filtered 0.0 (length filtered) 
                                      :final-merge? nil))
                (unless (member new-candidate candidates)
                  (setq candidates
                        (merge 'list (list new-candidate) candidates
                               #'> :key #'score))))))))
      (check-for-kernel-plateau sme) ;; check for plataeu effect.
      (setq result 
            (greedy-merge-with-sought sought-solutions
                                      sme (remove-if filter candidates)
                                      greedy-cutoff greedy-max-#
                                      :final-merge? t))
       (when *warn-on-greedy-inversion?*
         (check-for-greedy-inversion candidates result)))
    result))

(defun greedy-merge-with-sought (sought-solutions sme candidates
                                                  greedy-cutoff greedy-max-# 
                                                  &key (final-merge?))
  ;;Sought solutions is now always null or a single mapping representing
  ;; the mapping made of the required correspondences.
  (cond ((null sought-solutions)
         (greedy-cream sme candidates greedy-cutoff greedy-max-# 
                       :final-merge? final-merge?))
        (t (let ((best-solutions nil))
             (dolist (solution sought-solutions best-solutions)
               (let ((these-solutions 
                      (greedy-within-constraints
                       sme solution candidates greedy-cutoff greedy-max-# 
                       :final-merge? final-merge?)))
                 (setq best-solutions (weave-solutions these-solutions best-solutions
                                                       greedy-cutoff greedy-max-#))))))))

(defun greedy-within-constraints (sme solution candidates greedy-cutoff greedy-max-#
                                      &key (final-merge?))
  ;; Ensures that we construct mappings that include the required solution.
  (mapcar #'(lambda (result)
              (merge-mapping-pair sme solution result))
    (greedy-cream sme (remove-if-not #'(lambda (candidate)
                                         (mutually-consistent? candidate solution))
                                     candidates) 
      greedy-cutoff greedy-max-# :final-merge? final-merge?)))
  
(defun weave-solutions (these-solutions best-solutions cutoff max-#)
  (dolist (new-solution these-solutions)
    (setq best-solutions (merge 'list (list new-solution) best-solutions '> :key 'score)))
  ;; Chop off all that are beyond the max-#
  (when (> (length best-solutions) max-#)
    (setq best-solutions (butlast best-solutions (- (length best-solutions) max-#))))
  (when best-solutions
    (let ((threshold (* cutoff (score (car best-solutions)))))
      (setq best-solutions (remove-if #'(lambda (soln) (< (score soln) threshold))
                                      best-solutions))))
  best-solutions)
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helpers for greedy-merge

(defun check-for-kernel-plateau (sme)
  "For the given SME, check all the kernel mappings and see if a
   plateau condition exists.  If so, don't error out, but set the 
   kernel plateau flag to the number of equal kernels  A kernel plateau 
   is present when three or more of the kernels have an equal score."
  (unless (null (kernel-mappings sme))
     (let ((num-equal 1) 
           (top-score (score (car (kernel-mappings sme)))))
       (dolist (kernel (cdr (kernel-mappings sme)))
         (cond
          ((> (score kernel) top-score)
           (setq top-score (score kernel))
           (setq num-equal 1))
          ((= (score kernel) top-score)
           (incf num-equal))
          (t nil)))
       (when (> num-equal 1)
         (setf (kernel-plateau? sme) num-equal))
       (when (and (> num-equal *default-greedy-max-#*)
                  *warn-on-kernel-plateaus?*)
             (warn "Kernel plateau of ~D found for ~A!!" num-equal sme))
       num-equal)))

(defun add-consistent-kernels-to-results (current-mappings &optional filtered-kernels)
  "For the given set of candidates, add all consistent kernels
   that have not already been included in the set. Return result
   in order of score." 
  ;; Q: Should this use filters?  It doesn't currently.
  ;; A: Now it does (AML, 1/10)
   (when current-mappings
     (let* ((unused-candidates nil)
            (sme (sme (car current-mappings)))
            (kernels (or filtered-kernels (kernel-mappings sme)))
            (result nil))
         (setq result
           (mapcar #'(lambda (current-mapping)
                       ;; For unused candidate kernels, check if addable.
                       (setq unused-candidates (set-difference
                                                kernels 
                                                (submappings current-mapping) :test #'eq))
                       (dolist (candidate unused-candidates)
                          (when (mappings-mutually-consistent? candidate
                                 current-mapping)
                             (setq current-mapping 
                               (merge-mapping-pair sme current-mapping candidate))))
                       current-mapping)
             current-mappings))
         (sort (copy-list result) #'> :key #'score))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manipulating kernels

(defmethod find-kernel-mappings ((sme sme))
  (dolist (mh-root (root-mhs sme))
     (when (> (timestamp mh-root) (last-mapped sme))
       (find-kernel-mappings-for mh-root sme)))
  (kernel-mappings sme))

(defun find-kernel-mappings-for (mh sme)
  (if (or (incomplete? mh)
          (inconsistent? mh))
      (dolist (arg-mh (arguments mh))
              (find-kernel-mappings-for arg-mh sme))
    (make-kernel-mapping mh sme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Partitioning via projection to common base roots
;;;
;;; What needs to happen is a pre-merge of mappings with a common
;;; base.  This prevents such nonsense as >diam <-> >temp & wflow <->
;;; hflow going into the same mapping, since >press <-> >temp & &
;;; wflow <-> hflow share common base structure while >diam <-> >temp
;;; does not.  In SME 2E and earlier, merging mappings with common
;;; base structure was done exhaustively.  Obviously we don't want to
;;; do this.  Instead, sort the root mappings into equivalence
;;; classes, according to overlap in base roots.  Use greedy merge
;;; within each equivalence class, and carry on from there.  (Notice
;;; this means splitting the indexing of the merged mapping from
;;; building it, in create-merged-mapping above)
;;;
;;; Actually, many root mappings have more than one base root,
;;;  so put each root mapping into as many equivalence classes
;;;  as roots.

(defparameter *fold-in-attributes* t "Experimental")

(defun partition-mappings-via-roots (sme &optional (filter #'always-false))
   "Takes SME, and partitions the kernel mapping list by base root 
   expressions.  Returns alist where CAR is a dgroup root
   expression, and the CDR is a list of kernels that map some part of
   that root expression."
  (let ((table nil)
        (attribute-table nil)
        (entry nil))
      (dolist (kernel (consistent-kernel-mappings sme) table)
         (unless (funcall filter kernel)
            (dolist (root-mh (root-mhs kernel))
              (dolist (root-expr (roots (base-item root-mh)))
                (cond ((and *fold-in-attributes*
                            (null sme::*minimal-ascend-attributes*)
                            (attribute? root-expr))
                       (setq entry (assoc (first (arguments root-mh)) attribute-table
                                          :test #'eq))
                       (if entry
                         (setf (second entry) (merge-mapping-pair sme kernel (second entry)))
                         (push (list (first (arguments root-mh)) kernel) attribute-table)))
                      (t
                       (setq entry (assoc root-expr table :test #'eq))
                       (cond ((null entry)
                              (push (list root-expr kernel) table))
                             ((member kernel (cdr entry) :test #'eq)
                              nil)
                             (t
                              (setf (cdr entry)
                                (merge 'list (list kernel) (cdr entry) #'> :key #'score)))))
                      )))))
    (fold-in-attributes sme table attribute-table)))

(defmethod consistent-kernel-mappings ((sme sme))
  (remove-if 'filter-match-via-filter-list  (kernel-mappings sme)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attribute folding
;;;
;;; The idea is to incorporate attribute matches into kernels when
;;; consistent.

(defparameter *exclude-attribute-only-matches* nil
  "Set flag to only include objects in the final mapping that have relational MHs")

(defun fold-in-attributes (sme partitions attribute-partitions)
  "Takes the output of partition-mappings-via-roots and returns a modified
   partition that has attribute kernels merged with the relational kernels
   that mention the same object pairings.  It drops all attribute partitions, 
   but adds partitions for object pairs"
  ;;; the nreverse in setting the cdr of partitions is to make the sme-shakedown
  ;;; come out the same when kernels are the same score
  (when *fold-in-attributes*
    (let ((new-kernel-mappings nil))
      (dolist (partition partitions)
        (do ((kernels (rest partition) (rest kernels))
             (updated-kernels nil
                              (cons 
                               (merge-in-attributes sme (first kernels) attribute-partitions)
                               updated-kernels)))
            ((null kernels) 
             (setf (cdr partition) (careful-candidate-sort
                                    (nreverse updated-kernels) #'> #'score))
             (setq new-kernel-mappings 
                   (update-new-kernel-mappings (cdr partition) new-kernel-mappings)))))))
  (if *exclude-attribute-only-matches*
      partitions
    (append partitions attribute-partitions)))

(defun update-new-kernel-mappings (partition new-kernel-mappings)
  (dolist (element partition new-kernel-mappings)
    (pushnew element new-kernel-mappings)))

(defun merge-in-attributes (sme base-kernel attribute-partitions)
  (let ((new-kernel base-kernel)
        (entry nil))
    (dolist (mh (mhs base-kernel) new-kernel)
      (when (and (entity? mh)
                 (setq entry (assoc mh attribute-partitions :test #'eq)))
        (setq new-kernel (merge-mapping-pair sme new-kernel (second entry)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Greedy cream

;; general greedy utility check = procedure which returns non-nil if
;; two candidates are compatible.  scorer = returns a positive integer
;; score for the weight of a candidate.  It is assumed that scores of
;; a combination of candidates is simply the sum of their individual
;; scores.  merger = given two candidates, produces a new candidate
;; which is the union of the two.

;; cutoff = only accept solutions within cutoff of the best one.
;; max-# = absolute limit on number of interpretations produced.
;; After we find a second mapping, we give it a chance to gobble up any
;; consistent kernels from the previous mapping (so the mappings may share
;; kernels).  Similarly, a third mapping gets a chance at the kernels in the 
;; previous two mappings.

(defgeneric greedy-cream (sme candidates cutoff max-# &key (final-merge?))
  (:documentation
   "For the given list of candidate mappings, return a set
of global mappings constructed out of compatible
sets of candidates.  Returns list of mappings sorted by score."))
   
(defmethod greedy-cream ((sme sme) (candidates list)
                         (cutoff number) (max-# integer) &key (final-merge?))
   "For the given list of candidate mappings, return a set
of global mappings constructed out of compatible
sets of candidates.  Returns list of mappings sorted by score."
   (cond 
    ((null candidates) nil) ;; No candidates, no mappings
    ((null (cdr candidates)) ;; Only one, but is it consistent
     (list (car candidates)))
    (t ;; multiple candidates.
     (let* ((solutions nil)
            (solution nil)
            (remaining (careful-candidate-sort candidates #'> #'score))
            (used)
            (best-score -1)
            (done? nil))  
         (loop 
           (when done? (return))
           (multiple-value-setq (solution remaining)
             (greedy-gatherer sme remaining :final-merge? final-merge?))
           
           ;;Give any previous used kernels a chance to be added.
           (when solution
             (let ((old-used used))
               (setf used (append used (submappings solution)))
               (setf solution (add-consistent-kernels-to-mapping solution old-used))))
           
           (cond 
            ((null solution) (setq done? t)) ;; no solution left
            
            ((>= (score solution) (* cutoff best-score)) ;; solution good enough
             (setq best-score (max best-score (score solution)))
             (push solution solutions))
            
            (t                                           ;;solution isn't good enough
             (setq done? t)))
           
           (decf max-#)
           
           (if (<= max-# 0)
             (setq done? t)))
       (setf solutions (careful-candidate-sort solutions #'> #'score))
       (remove-if #'(lambda (mapping)
                      (< (score mapping) (* cutoff best-score)))
         solutions)))))
           
(defun add-consistent-kernels-to-mapping (mapping kernels)
  "Greedily add all consistent kernels to the mapping."
  
  (let ((sme (sme mapping)))
    (dolist (kernel kernels mapping)
      (when (mappings-mutually-consistent? kernel mapping)
        (setq mapping (merge-mapping-pair sme mapping kernel))))))

(defun careful-candidate-sort (candidates predicate key)
   (let ((result nil))
      (dolist (candidate candidates result)
         (setq result (merge  'list (list candidate) result predicate
                        :key key)))))

(defun add-candidate-scores (candidates)
   (let ((result 0))
      (dolist (candidate candidates result)
         (incf result (score candidate)))))

(defgeneric greedy-gatherer (sme candidate-mappings &key (final-merge?))
  (:documentation 
   "Assumes candidates are non-nil and sorted by score.  Produces a new
solution and the list of the candidates not merged, in sorted
order, for the next round of using it, if needed."))
   
(defmethod greedy-gatherer ((sme sme) (candidates list) &key (final-merge?))
  (if (and final-merge? (greedy-merge-overlap-only? (mapping-parameters sme)))
    (greedy-gatherer-overlap-only sme candidates)
    (basic-greedy-gatherer sme candidates)))
    
(defun basic-greedy-gatherer (sme candidates)
   "Assumes candidates are non-nil and sorted by score.  Produces a new
   solution and the list of the candidates not merged, in sorted
   order, for the next round of using it, if needed."
  (cond ((null candidates) nil)
        ((null (cdr candidates)) (car candidates))
        (t (let ((remaining nil)
                 (solution (car candidates)))
             (dolist (candidate (cdr candidates)
                                (values solution (nreverse remaining)))
               (cond ((mappings-mutually-consistent? solution candidate)     
                      (setq solution (merge-mapping-pair sme solution candidate)))
                     (t (push candidate remaining))))))))

;;This gatherer will only merge kernels that have mh's in common.
;;Either entity mh's or relation mh's (but not functor mh's) are sufficient.
(defun greedy-gatherer-overlap-only (sme candidates)
   "Assumes candidates are non-nil and sorted by score.  Produces a new
   solution and the list of the candidates not merged, in sorted
   order, for the next round of using it, if needed."
  (cond ((null candidates) nil)
        ((null (cdr candidates)) (car candidates))
        (t (let ((solution (car candidates)))
             (do* ((found? t found?)
                   (remaining0 (cdr candidates) (nreverse remaining1))
                   (remaining1 nil nil))
                  ((null found?) (values solution remaining0))
               (setf found? nil)
               (dolist (candidate remaining0)
                 (cond ((and (mappings-mutually-consistent? solution candidate)
                             (mapping-mhs-overlap? candidate solution))
                        (setf found? t)
                        (setq solution (merge-mapping-pair sme solution candidate)))
                       (t (push candidate remaining1)))))))))

(defun mhs-consistent? (mh1 mh2)
   (and (not (intersection (descendants mh1) (nogoods mh2)))
        (not (intersection (descendants mh2) (nogoods mh1)))))

(defun make-temp-mapping (mhs sme &aux new-mapping)
   "Makes a mapping that is used for temporary computations"
   (setq new-mapping
        (make-mapping-instance *default-mapping-type*
                       :root-mhs (copy-list mhs)
                       :mhs (copy-list mhs)
          :sme sme))
  (setf (submappings new-mapping) (list new-mapping))
  (dolist (mh mhs)
    (initialize-mapping-set-properties new-mapping mh))
  new-mapping)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
