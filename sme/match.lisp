;;;; -*- Mode: Lisp; Mode: Outline; -*-
;;;; ------------------------------------------------------------
;;;; File name: match.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;    Author: Ron Ferguson & Ken Forbus
;;;;  $LastChangedDate: 2014-08-27 19:55:12 -0500 (Wed, 27 Aug 2014) $
;;;;  $LastChangedBy: forbus $
;;;;
;;;;  Entry points for comparison
;;;; ------------------------------------------------------------
(in-package :sme)

;; Incremental matching requires keeping track of what has been
;; added to the base and target since the last time a match was
;; created.  We assume the following:

;; * Each description contains a local timestamp, which is incremented
;; whenever an item is added.  (For now deletions are forbidden.)
;; * An empty description has a timestamp of 0.
;; * The timestamp of a description is equal to the sum of the
;; expressions and entities which comprise it.
;; * The operation (items-since <description> <old time>) returns
;; a list of items added to <description> since <old time>.
;; * Each SME has a timestamp, based on the number of match hypotheses.
;; * The operation (mhs-since <sme> <old time>) returns a list of the
;; match hypotheses created since <old time>.

;; items-since supports the incremental creation of match hypotheses,
;; mhs-since supports the incremental extension of global interpretations.

(defmethod match-extendable? ((sme t)) nil)

(defmethod match-extendable? ((sme sme))
  ;; Has anything been added since the last incremental match?
  (or (> (timeclock (base sme)) (base-timestamp sme))
      (> (timeclock (target sme)) (target-timestamp sme))))
         
(defgeneric incremental-match (sme &optional filter)
  (:documentation 
   "Create a mapping between the base and target descriptions of
    the given SME object.  Map incrementally by extending the
    mapping using any new information found in the base or 
    target descriptions.  Previous global mappings are extended
    rather than discarded (as in match function).  Filter is used for 
    pragmatic marking.  It should be an argument that takes
    a kernel mapping and returns true if the mapping should be
    excluded.  Default is for this to always be false."))

(defmethod incremental-match (sme &optional (filter #'always-false))
  "Incrementally extend the mapping for the given SME object,
   using the given filters, if any."
   (multiple-value-bind (new-mhs new-bitems new-titems) 
       (extend-mhs sme)
      (when new-mhs ;; new local matches?
         (find-kernel-mappings sme) ;; find new root mappings
         (evaluate sme)
         ;; This change is to allow pragmatic filtering
         ;; criteria at the level of kernel-mappings
         (extend-all-mappings
          (remove-if filter (new-kernel-mappings sme))
          sme)
         (setf (last-mapped sme) (timeclock sme)))
      (when (or new-bitems new-titems new-mhs)
        ;; If anything has changed, need to reevaluate
        ;; the roots of the mappings and whether there
        ;; are any new candidate inferences.  
        (dolist (mapping (mappings sme))
          (aggregate-top-level-mhs mapping)
          (calculate-mapping-inferences mapping)))
      (setf (new-mhs sme) nil)  ;; clear out accumulator
     sme))

(defmethod incremental-match-with-appropriate-filters ((sme sme) filter-input)
  "Handles filters of various types, passes on to incremental-match to do real work."
  (incremental-match sme (construct-filter sme filter-input)))

;; Non-incremental entry point
(defgeneric match (sme &optional filter)
  (:documentation 
   "Create a mapping between the base and target descriptions of
    the given SME object.  Map non-incrementally -- if any 
    previous global mappings exist, remove them and redo
    global mapping merge from scratch (note: does not generate
    kernel mappings from scratch).  Filter must be a procedure
    of one argument that returns true if a mapping should be
    excluded. "))

(defmethod match ((sme sme) &optional (filter #'always-false))
  "Create a mapping between the target and base descriptions
   of the given SME object."
   (if (member :instrumented *sme-debug-flags*)
      (instrumented-match sme filter)
      (let ((*sme* sme))
         (setf (unused-score sme) 0.0)
         (extend-mhs sme)
         (find-kernel-mappings sme)
         (evaluate sme)
         (setf (mappings sme) 
               (merge-consistent-kernels sme filter))
        (dolist (mapping (mappings sme))
          (aggregate-top-level-mhs mapping)
          (calculate-mapping-inferences mapping))
        ;;recompute the global mapping score if
        ;;allow-out-of-mapping-score-contributions? is nil
        (when (block-most-out-of-mapping-contributions? (mapping-parameters sme))
          (dolist (mapping (mappings sme))
            (mapping-structural-evaluation mapping (mapping-parameters sme) t))
          (setf (mappings sme) (sort (copy-list (mappings sme)) #'> :key #'score)))
        ;; Update object parameters concerning incrementality        
         (setf (last-mapped sme) (timeclock sme))
         (setf (new-mhs sme) nil)  ;; clear out accumulator
         sme)))

(defun filter-match-via-filter-list (mapping)
  ;; Used with handling generic match constraints.  Can be combined
  ;; in closures that support pragmatic marking and other task-specific
  ;; filter techniques.
  (check-sme-filters mapping (match-filters (sme mapping))))

(defun construct-filter (sme filter-input)
  (cond ((or (functionp filter-input)
             (and (symbolp filter-input)
                  (fboundp filter-input)))
         ;; Trust caller knows what they are doing
         ;; But clear the match-filters field to avoid mishap
         (setf (match-filters sme) nil)
         (setf (match-constraints sme) nil)                               
         filter-input)
        ((and (consp filter-input)                                        
              (every #'sme-filter? filter-input))
         ;; Someone has precomputed the filters for us, so just use them.
         (setf (match-filters sme) filter-input)
         (setf (match-constraints sme) (filter-list-form (match-filters sme)))   
         'filter-match-via-filter-list)
        ((and (consp filter-input)                                        
              (every 'listp filter-input))
         ;; Assume it's a list of assertions defining filters
         (setf (match-filters sme) (create-filters-from-assertions sme filter-input))
         (setf (match-constraints sme) (filter-list-form (match-filters sme)))
         'filter-match-via-filter-list)
        (t (setf (match-filters sme) nil)
           (setf (match-constraints sme) nil)                             
           #'always-false)))

(defmethod match-with-appropriate-filters ((sme sme) filter-input &key (instrumented? nil))
  "Handles filters of various types, passes on to match to do real work."
  (let ((filter (construct-filter sme filter-input)))
    (if instrumented? (instrumented-match sme filter)
      (match sme filter))))

(defun instrumented-match-with-appropriate-filters (sme filter-input)
  (match-with-appropriate-filters sme filter-input :instrumented? t))

;;;; Incremental extension of matches

(defmethod extend-mhs ((sme sme))
  (let* ((btime (base-timestamp sme)) ;; last time base was examined.
         (ttime (target-timestamp sme)) ;; last point target was examined.
         (bitems (items-since (base sme) btime))
         (titems (items-since (target sme) ttime)))
    (setf (new-mappings sme) nil)
    ;; New base items against ALL target itmes
    (match-all-expressions bitems (expressions (target sme)) sme)
    ;; New target items against ALL base itmes
    (match-all-expressions (expressions (base sme)) titems sme)
    (calculate-structural-relations sme)
    (calculate-mh-roots sme)
    (setf (base-timestamp sme) (timeclock (base sme)))
    (setf (target-timestamp sme) (timeclock (target sme)))
    (values (new-mhs sme) bitems titems)))

(defmethod match-all-expressions (b-exprs t-exprs (sme sme))
  ;; expressions are stored in a-list bins - just match within each bin
  (dolist (b-bin b-exprs) 
    (unless (ubiquitous-predicate-in? (first b-bin)
              (vocabulary (base sme)))
      (match-items-in-bin (rest b-bin) 
                          (list-like-expressions (first b-bin) t-exprs) sme)))
  (new-mhs sme))

(defun match-items-in-bin (base-exprs target-exprs sme)
  (dolist (bitem base-exprs)
    (dolist (titem target-exprs)
      (multiple-value-bind (alignable? rationale)
          (locally-alignable? bitem titem sme)
        (when alignable?
          (create-local-match bitem titem rationale sme) sme)
        ))))

(defun list-like-expressions (bin-name exprs)
   ;; check for possible matches due to minimal ascension
   (if (null (parents bin-name))
      (rest (assoc bin-name exprs :test #'eq))
      (list-like-expressions-for-ids (parents bin-name) exprs)))

(defun list-like-expressions-for-ids (ids exprs)
  ;; append together all the expressions for each group indexed by the ids
  (when exprs
    (let ((v (vocabulary (description (cadar exprs)))))
      (do ((id-lst ids (rest id-lst))
           (result nil (if (ubiquitous-predicate-in? (first id-lst) v)
                         result
                         (append (rest (assoc (first id-lst) exprs
                                         :key #'id :test #'eq)) result))))
          ((null id-lst) result)))))

;;;; Extending mappings
;;
;; Possible criterion for merging a new root mapping into an existing
;; mapping:
;; 1. Not structurally inconsistent.
;; 2. In addition to #1, the intersection of the correspondences must be
;;    non-empty.
;; 3. In addition to #2, the new root mapping must have overlap beyond entities
;;    with the existing mapping.
;; The second criterion is more conservative, but it means that if we accumulate
;; several independent pieces of the match we won't merge them w/o remapping.

(defmethod extend-all-mappings (new-rmaps (sme sme))
  ;; Tries to extend existing mappings via new root maps.  Two
  ;; desirable properties of this algorithm are: 1. Conservatism: If
  ;; there are extensions of the current best mapping that work, they
  ;; should be found in preference to elevating something else.
  ;; 2. Signaling: The degree to which the root maps cannot be integrated
  ;; should be made clear, by updating the unused-score for the SME.
  ;; This version only uses a root map once.  The idea is to ensure that
  ;; alternative interpretations remain distinctive, as a way of reducing
  ;; order bias.
  ;;
  (when (member :instrumented *sme-debug-flags* :test #'eq)
    (format *sme-debug-stream*
        "~%Number of new mappings being merged = ~A~%" (length new-rmaps)))
  (do ((old-mapping (car (mappings sme))
                    (car rest-of-mappings))
       (rest-of-mappings (cdr (mappings sme))
                         (cdr rest-of-mappings))
       (candidates (sort (copy-list new-rmaps)
                         (lambda (x y) (> (score x) (score y)))))
       (new-mappings nil)
       (best-extension nil)
       (remaining-root-maps new-rmaps))
      ((null old-mapping)
       (dolist (leftover remaining-root-maps)
         (incf (unused-score sme) (score leftover)))
       (dolist (mapping new-mappings) ;; update ci's
         (calculate-mapping-inferences mapping))
       (setf (mappings sme)
         (sort new-mappings #'score>)))
    (multiple-value-setq (best-extension remaining-root-maps)
      ;;  (find-best-extended-mapping old-mapping new-rmaps sme)
      (greedy-find-good-extended-mapping old-mapping candidates sme))
    (push best-extension new-mappings)))

(defun greedy-find-good-extended-mapping (old-mapping new-rmaps sme)
  ;; Returns extended mapping plus remaining unused new root mappings
  (let ((mapping old-mapping)
        (remainders nil))
    (dolist (rmap new-rmaps (values mapping
                                    (nreverse remainders)))
      (cond ((mappings-mutually-consistent? mapping rmap)
             (setq mapping (merge-mapping-pair sme mapping rmap)))
            (t (push rmap remainders))))))

(defun find-best-extended-mapping (seed candidates sme)
  ;; Claim: Generally the number of candidates will be small.
  ;; Therefore it is reasonable to do an exhaustive merge.  If this
  ;; turns out to be problematic, can do some cross between exhaustive
  ;; when number of candidates is small and greedy when they are large.
  (cond ((null candidates) (values seed nil))
        ((subsumed-by? (car candidates) seed)
         ;; Skip ahead
         (find-best-extended-mapping seed (cdr candidates) sme))
        ((mutually-consistent? (car candidates) seed)
         ;; Try extending, but also try extending without it to see if
         ;; other alternatives are better
         (multiple-value-bind (with-first first-leftovers)
             (find-best-extended-mapping
              (find-or-create-merged-mapping sme (list (car candidates) seed))
              (cdr candidates) sme)
           (multiple-value-bind (without-first without-leftovers)
               (find-best-extended-mapping seed (cdr candidates) sme)
             (if (>= (score with-first) (score without-first))
               (values with-first first-leftovers)
               (values without-first
                       (cons (car candidates) without-leftovers)))
             )))
        ;; O.W. conflicts, so check out rest of choices.
        (t (find-best-extended-mapping seed (cdr candidates) sme))))

(defun subsumed-by? (mapping1 mapping2)
  (null (set-difference (mhs mapping1) (mhs mapping2) :test #'eq)))

(defun trim-via-max-# (list max-len)
  (cond ((null list) nil)
        ((not (> max-len 0)) nil)
        (t (cons (car list)
                 (trim-via-max-# (cdr list) (1- max-len))))))

(defmethod extend-mapping ((old-mapping mapping) (new-rmap mapping) (sme sme))
  (cond ((mutually-consistent? old-mapping new-rmap)
         (find-or-create-merged-mapping sme (list new-rmap old-mapping)))
        (t old-mapping)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remapping
;;;
;;; Incremental matching means that information may not arrive
;;; in optimal orders, so that the existing mappings are no longer
;;; the best interpretation of the comparison.  The threshold at which
;;; this should occur is task-specific, so remap-info returns the 
;;; amount of unused score and the score provided by the current mappings,
;;; so that such decisions could be made.  

(defun remap-info (sme)
  (values (unused-score sme) 
          (reduce #'(lambda (sum mapping)
                      (+ sum (score mapping)))
                  (mappings sme) :initial-value 0.0)))

;;;; Remap itself assumes incremental updates of the match have
;;;; already been done.  It recalculates the mappings from scratch,
;;;; based on the existing kernels, 
;;;; giving it a chance to find newer, better solutions.

(defmethod remap ((sme sme))
  (setf (mappings sme) 
    (merge-consistent-kernels sme 'filter-match-via-filter-list))
  (dolist (mapping (mappings sme))
    (aggregate-top-level-mhs mapping)
    (calculate-mapping-inferences mapping))
  sme)

(defmethod remap-with-appropriate-filters ((sme sme) filter-input &key (instrumented? nil))
  ;; useful in case filters have changed.
  (declare (ignore instrumented?))
  (construct-filter sme filter-input) ;; This changes the SME object
  (remap sme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Match entry point for benchmarking

(defmethod instrumented-match ((sme sme) &optional (filter #'always-false))
  "Create a mapping between the target and base descriptions
   of the given SME object, but prints timing information"
  (let ((*sme* sme))
    (setf (unused-score sme) 0.0)
    (with-recorded-real-time (format nil "~A MHs" sme)
      (extend-mhs sme))
    (with-recorded-real-time (format nil "~A Kernels" sme)
      (find-kernel-mappings sme))
    (with-recorded-real-time (format nil "~A SE" sme)
                             (evaluate sme))
    (setf (mappings sme) 
      (with-recorded-real-time (format nil "~A Merge" sme)
                               (merge-consistent-kernels sme filter)))
    (with-recorded-real-time (format nil "~A CIs" sme)
      (let ((number-of-cis 0))
         (dolist (mapping (mappings sme) number-of-cis)
            (incf number-of-cis 
              (length (calculate-mapping-inferences mapping))))))
     (setf (last-mapped sme) (timeclock sme))
     (setf (new-mhs sme) nil)  ;; clear out accumulator
     sme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code