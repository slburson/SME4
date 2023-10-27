;;;; -*- Mode: Lisp; -*-
;;;; ------------------------------------------------------------
;;;; File name: mapping.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;  $LastChangedDate: 2014-08-06 12:34:13 -0500 (Wed, 06 Aug 2014) $
;;;;  $LastChangedBy: forbus $
;;;; ------------------------------------------------------------
;;;;  This file contains the mapping structure and various methods
;;;;   for creating, checking and adding to mappings.
;;;;
;;;;  Match routines are now all in match.lisp.
;;;;
(in-package :sme)

(defparameter *default-mapping-type* 'mapping
  "Provided for experimental versions, integration with external systems.
   Rarely used, left in for backward compatibility with some experimental
   systems.")

;;; MAPPING CLASS DEFINITION
;;; ----------------------------------------------------------------------------
(defclass mapping ()
  ((id
    :documentation "Unique ID for mapping."
    :accessor id  :initform 0  :initarg :id)
   (timestamp
    :documentation "When this mapping was last modified."
    :initform 0  :initarg :timestamp  :accessor timestamp)
   (ci-counter :initform -1 :accessor ci-counter
               :documentation "ID counter for candidate inferences")
   (mhs
    :documentation "The match hypotheses in mapping"
    :type list  :initarg :mhs  :initform nil  :accessor mhs)
   (root-mhs
    :documentation "The root match-hypotheses.  Subset of above."
    :type list  :initarg :root-mhs  :initform nil  :accessor root-mhs)
   (top-level-mhs
    :documentation "Match hypotheses that are top-level expressions."
    :type list  :initarg :top-level-mhs  :initform nil  :accessor top-level-mhs)   
   (descendants
    :documentation "Union of the descendants of its match hypotheses."
    :type
    bitset
    :initarg :descendants  :accessor descendants)
   (nogoods
    :documentation "Union of the nogoods of its match hypotheses."
    :accessor nogoods  :initarg :nogoods  :type 
    bitset
    )
   (inferences
    :documentation "The base-to-target candidate inferences for this mapping."
    :initform nil  :initarg :inferences :accessor inferences)
   (reverse-inferences
    :documentation "The target-to-base candidate inferences for this mapping."
    :initform nil  :initarg :reverse-inferences :accessor reverse-inferences)
   (submappings
    :documentation "Mappings from which this one was built."
    :initform nil  :initarg :submappings  :accessor submappings)
   (sme
    :documentation "The SME to which I belong."
    :initform nil  :initarg :sme  :accessor sme)
   (score
    :documentation "The structural raw score of this mapping."
    :type float  :accessor score  :initform 0.0  :initarg :score)
   (inconsistent?
    :documentation "Is this mapping itself inconsistent?"
    :accessor inconsistent? :initform nil)
   (commutatives
    :documentation "Consistant subset of arg mh's of commutative
       predicates, given in comm-table format."  
    :initform nil  :initarg :commutatives  :accessor commutatives))
   (:documentation 
    "A mapping is a set of correspondences between the entities and
    expressions of two descriptions.  Individual alignments are 
    given as match hypotheses.  Mappings with a single pair of 
    aligned root expressions are called Kernel Mappings."))
  
(defmethod make-mapping-instance ((mapping-type t) &rest initargs)
  (let* ((instance (apply 'make-instance (cons mapping-type initargs)))
         (sme (sme instance)))
    (if (not (slot-boundp instance 'descendants))
        (setf (descendants instance) nil))
    (if (not (slot-boundp instance 'nogoods))
        (setf (nogoods instance) nil))
     (setf (id instance) (incf (mapping-counter sme)))
    (setf (timestamp instance) (incf (timeclock sme)))
    instance))

(defmethod print-object ((mapping mapping) stream)
  (format stream "<Mapping ~A>" (id mapping)))

(defmethod root-score ((mh mapping)) 0.0) 

;;; Creating New Mappings

(defmethod make-kernel-mapping ((mh match-hypothesis) sme &aux new-mapping)
  "Given a match hypothesis (and the SME) create a new kernel
   mapping based on that match hypothesis.  Returns the kernel
   mapping."
  (setq new-mapping
        (make-mapping-instance *default-mapping-type*
         :root-mhs (list mh)
         :mhs (descendants mh)
         :sme sme))
  (initialize-mapping-set-properties new-mapping mh)
  (setf (submappings new-mapping) (list new-mapping)) ;; starting point
  (dolist (mh (mhs new-mapping))
    (if (commutative-mh? mh)
        (push (make-ct-entry mh new-mapping) (commutatives new-mapping))))
  (check-commutative-mapping-constraints new-mapping)
  (push new-mapping (kernel-mappings sme)))

(defmethod initialize-mapping-set-properties ((new-mapping mapping) mh)
  (dolist (mh (nogoods mh))
    (setf (nogoods new-mapping)
      (set-add mh (nogoods new-mapping))))
  (dolist (mh (mhs new-mapping))
    (setf (descendants new-mapping)
      (set-add mh (descendants new-mapping)))))

(defun new-kernel-mappings (sme)
  "Return a list of all kernel mappings created since the last
   mapping was completed."
  ;; new mappings are pushed on, so stop after first mapping is 
  ;; before timestamp
  (do ((mhs (kernel-mappings sme) (rest mhs))
       (result nil (cons (first mhs) result)))
      ((or (null mhs) (> (last-mapped sme) (timestamp (first mhs))))
       result)))

;;; Consistency checking

(defmethod mutually-consistent? ((m1 mapping) (m2 mapping))
  (and (not (set-common-member? (descendants m1) (nogoods m2)))
       (not (set-common-member? (descendants m2) (nogoods m1)))))

(defmethod mutually-consistent? ((m1 match-hypothesis) (m2 mapping))
  (and (not (set-common-member? (descendants m1) (nogoods m2)))
       (not (set-common-member? (nogoods m1) (descendants m2)))))

(defmethod mutually-consistent? ((m1 mapping) (m2 match-hypothesis))
  (and (not (set-common-member? (descendants m2) (nogoods m1)))
       (not (set-common-member? (nogoods m2) (descendants m1)))))

(defmethod mutually-consistent? ((m1 match-hypothesis) (m2 match-hypothesis))
  (and (null (intersection (descendants m1) (nogoods m2)))
       (null (intersection (descendants m2) (nogoods m1)))))

(defmethod mutually-consistent2? ((mh1 match-hypothesis) (m2 mapping))
   (and (not (set-common-member? (descendants mh1) (nogoods m2)))
        (not (set-common-member? (nogoods mh1) (descendants m2)))))


(defun mappings-mutually-consistent? (mapping1 mapping2)
  "Returns t if the two mappings are consistent with 
   one another."
   (declare (type mapping mapping1 mapping2))
   (let ((commutatives1 (commutatives mapping1))
         (commutatives2 (commutatives mapping2)))
      (cond ((null (mutually-consistent? mapping1 mapping2))
             nil)
            ;; If neither has commutatives, then consistent.
            ((not (or commutatives1 commutatives2))
             t)
            ;; Check if commutatives are minimally consistent.
            ((and (not (comm-table-inconsistent? mapping1 commutatives2))
                  (not (comm-table-inconsistent? mapping2 commutatives1))
                  (comm-tables-interconsistent? commutatives1 commutatives2))
             t)
       (t nil))))

(defun mapping-mhs-overlap? (mapping1 mapping2)
  (dolist (mh1 (mhs mapping1))
    (dolist (mh2 (mhs mapping2))
      (if (and (eq mh1 mh2) (not (eq (rationale mh1) :functor-match)))
        (return-from mapping-mhs-overlap? t))))) 


;;; Consistency Checking for Commutatives In Mappings

(defun comm-table-inconsistent? (mapping comm-table)
   "Is this commutatives table inconsistent with the mapping?"
   (some #'(lambda (ct-entry) (ct-entry-inconsistent? mapping ct-entry))
     comm-table))

(defun ct-entry-inconsistent? (mapping ct-entry)
   "Is the following ct-entry inconsistent with the mapping?"
   ;; Entry is inconsistent if any choice set is inconsistent.
   (unless (integrated-ct-entry? ct-entry) ;; if integrated, consistent.
      (dolist (choice-set (ct-entry-choice-sets ct-entry))
         (when (choice-set-inconsistent? mapping choice-set)
            (return-from ct-entry-inconsistent? t))))
   nil)

(defmethod choice-set-inconsistent? ((mapping mapping) choice-set)
   "A choice set is inconsistent if all the choices overlap with the
    mappings nogoods."
   ;; Meaning that if any choice does not overlap the nogoods, it is 
   ;; consistent.
   (dolist (mh (ct-choice-set-mhs choice-set))
      (when (not (set-member? mh (nogoods mapping)))
         (return-from choice-set-inconsistent? nil)))
   t)

;;; Interconsistency between commutative tables and entries.

(defun comm-tables-interconsistent? (ctable1 ctable2)
  "Are the commutatives in the two commutatives table
   interconsistent?"
  (comm-tables-consistent? ctable1 ctable2))

(defun comm-tables-consistent? (ctable1 ctable2)
   "Are the two commutative tables consistent with one another?
    Returns t or nil, and returns as soon as first inconsistency 
    is detected."
   (multiple-value-bind (entry-pairs leftover-entries)
       (common-pairs ctable1 ctable2 :key 'ct-entry-mh-key :test #'equal)
      (declare (ignore leftover-entries))
      ;; Now, check to make sure that each paired ct-entry is 
      ;;  consistent.  If not, return NIL.
      (dolist (entry-pair entry-pairs)
         (unless (ct-entries-consistent? (car entry-pair) (cdr entry-pair))
            (return-from comm-tables-consistent? nil)))
      t))

(defun ct-entries-consistent? (ct-entry1 ct-entry2)
   "Are the two commutative entries consistent with one another?"
  ;; Each ct entry consists of a key and a set of choice sets.
  ;; UPDATE: Integrated commutative entries don't have choice-sets.  Those 
  ;; need to be handled specially, thus the additions I made below.  
  ;;  [JMU 4/8/2011]
  (when (not (eq (ct-entry-mh-key ct-entry1) (ct-entry-mh-key ct-entry2)))
    (return-from ct-entries-consistent? nil))
  (cond ((and (integrated-ct-entry? ct-entry1)
              (integrated-ct-entry? ct-entry2))
         (let ((args1 (cddr ct-entry1))
               (args2 (cddr ct-entry2)))
           (and (= (length args1) (length args2))
                (every #'(lambda (arg1)
                           (member arg1 args2 :test #'equal))
                       args1))))
        ((integrated-ct-entry? ct-entry1) nil)
        ((integrated-ct-entry? ct-entry2) nil)
        (t
         (let ((choice-sets1 (ct-entry-choice-sets ct-entry1))
               (choice-sets2 (ct-entry-choice-sets ct-entry2)))
           (multiple-value-bind (paired-choice-sets leftovers)
               (common-pairs choice-sets1 choice-sets2 
                             :key 'ct-choice-set-key :test #'equal)
             (declare (ignore leftovers))
             ;; For the entries to be consistent, intersection 
             ;;  of each paired choice set must be non-empty.
             (dolist (pair paired-choice-sets)
               (when (null (intersection (ct-choice-set-mhs (car pair))
                                         (ct-choice-set-mhs (cdr pair))
                                         :test #'eq))
                 (return-from ct-entries-consistent? nil)))))
         ;; Now that all tests have been passed, they are consistent.
         t)))


;;; Merging Consistent Mappings

(defun merge-mapping-pair (sme mapping1 mapping2)
  "Stand-in function for now, will go away."
  (find-or-create-merged-mapping sme (list mapping1 mapping2)))

(defmethod find-or-create-merged-mapping ((sme sme) mappings-list)
  (let ((key (calculate-mappings-key mappings-list)))
    (cond ((find-merged-mapping sme key))
          (t (create-merged-mapping sme mappings-list key)))))

(defun calculate-mappings-key (mappings-list)
  (when (null mappings-list) (return-from calculate-mappings-key nil))
  (sort (reduce #'union (mapcar #'(lambda (m)
                                    (copy-list (submappings m)))
                                mappings-list))
                #'id<))

(defmethod find-merged-mapping ((sme sme) sorted-mappings-list)
  (find sorted-mappings-list (mappings-cache sme)
        :test #'equal :key #'submappings))

(defmethod create-merged-mapping ((sme sme) mappings-list key)
  (let ((new (make-mapping-instance *default-mapping-type*
                            :sme sme
                            :submappings key)))
    (setf (root-mhs new) ;; in case of previous merges 
          (reduce #'union (mapcar #'root-mhs mappings-list)))
    (setf (mhs new)
      (reduce #'union (mapcar #'mhs mappings-list)))
    (mapping-structural-evaluation new (mapping-parameters sme))
    (merge-consistency-check-fields new mappings-list)
    (push new (mappings-cache sme))
    ;; Finally, process commutative subexpressions
    (combine-mapping-commutatives mappings-list new)
    (check-commutative-mapping-constraints new)
    new))

(defmethod merge-consistency-check-fields ((new-mapping mapping) mappings-list)
  ;; merge the decendents and nogood lists
    (dolist (mapping mappings-list)
      (setf (descendants new-mapping)
        (set-union! (descendants new-mapping) (descendants mapping)))
      (setf (nogoods new-mapping)
        (set-union! (nogoods new-mapping) (nogoods mapping)))))

;;; Merging commutative tables and table entries.

(defun combine-mapping-commutatives (mapping-list new-mapping)
   "For the set of mappings, combine prune the commutatives based
    on the nogoods in the new-mapping.  Then combine the commutatives."
   (let ((new-ctable nil))
      (dolist (old-mapping mapping-list)
         (setf new-ctable (combine-comm-tables new-ctable 
                            (commutatives old-mapping))))
      (setf (commutatives new-mapping)
            (prune-comm-table new-ctable new-mapping))
      (check-commutative-mapping-constraints new-mapping)
      new-mapping))

(defun combine-comm-tables (ctable1 ctable2)
   "Combine the pair of comm tables.  Does not check for consistency."
   (multiple-value-bind (entry-pairs leftover-entries)
       (common-pairs ctable1 ctable2)
      ;; Now, combine each pair of entries.
      (let ((new-ctable-entries
             (mapcar #'(lambda (entry-pair)
                         (combine-ct-entries (car entry-pair) 
                                             (cdr entry-pair)))
                entry-pairs)))
         (append new-ctable-entries leftover-entries))))

(defun combine-ct-entries (ct-entry1 ct-entry2)
   "Combine the two commutative table entries into a consistent set.
    Does not check for consistency."
   ;; Double-check MH for each--must be identical.
   (when (not (eq (ct-entry-mh-key ct-entry1) (ct-entry-mh-key ct-entry2)))
      (return-from combine-ct-entries nil))
   ;; Now, find choice sets that refer to the same key, and combine them.
   (multiple-value-bind (choice-set-pairs leftovers)
       (common-pairs ct-entry1 ct-entry2
          :key 'ct-choice-set-key :test #'equal)
      (let ((combined-choice-sets
              (mapcar #'(lambda (pair)
                          (combine-choice-sets (car pair) (cdr pair)))
                choice-set-pairs)))
         (append combined-choice-sets leftovers))))

(defun combine-choice-sets (choice-set1 choice-set2)
   "Combine two choice sets.  Doesn't work for integrated 
    choice sets."
   (cons (ct-choice-set-key choice-set1)
     (intersection (ct-choice-set-mhs choice-set1)
       (ct-choice-set-mhs choice-set2) :test #'eq)))

(defmethod prune-comm-table (comm-table (mapping mapping))
   "Use the nogoods of the mapping to prune the contents of the 
    commutatives table."
   (mapcar #'(lambda (ct-entry)
               (prune-ct-entry ct-entry mapping))
     (copy-tree comm-table)))

(defun prune-ct-entry (ct-entry mapping)
   "Use the nogoods of the mapping to prune the contents of the 
     commutatives table entry. Destructive function."
   (unless (integrated-ct-entry? ct-entry) ;; if integrated, don't prune.
      (setf (ct-entry-choice-sets ct-entry)
            (mapcar #'(lambda (choice-set)
                        (prune-choice-set choice-set mapping))
              (ct-entry-choice-sets ct-entry))))
   ct-entry)

(defmethod prune-choice-set (choice-set (mapping mapping))
   "Use nogoods in mapping to prune choice set.  Destructive function."
   (let ((nogoods (nogoods mapping)))
      (setf (ct-choice-set-mhs choice-set)
            (remove-if #'(lambda (mh)
                           (set-member? mh nogoods))
              (ct-choice-set-mhs choice-set))))
   choice-set)

;;;; Commutative mapping constraints

(defun check-commutative-mapping-constraints (mapping)
  "Check for commutatives in the mapping that are either
   overconstrained (meaning that the mapping is inconsistent), or
   contain a single choice per ct entry (meaning that they can be
   integrated)."
  (dolist (ct-entry (commutatives mapping)) ;; for each commtable entry.
     (let ((cmh (ct-entry-mh-key ct-entry)))
        (cond ((integrated-ct-entry? ct-entry) nil) ;; already integrated.
              ((inconsistent-ct-entry? ct-entry)    ;; inconsistent.
               (push cmh (inconsistent? mapping)))
              ((integratable-ct-entry? ct-entry)    ;; can integrate--do so.
               (integrate-ct-entry ct-entry mapping))))))

(defmethod integrate-ct-entry (entry (mapping mapping))
   "Take the given ct-entry, which is guaranteed to be integratable,
    and turn it into an integrated ct-entry.  Also add the appropriate
    nogoods and descendents to the mapping.  Returns entry."
   (let* ((choice-sets (ct-entry-choice-sets entry))
          (subs (remove-duplicates (mapcar #'cadr choice-sets))))
      (dolist (sub subs)
        (mapc #'(lambda(sub-mh) 
                  (unless (member sub-mh (mhs mapping) :test #'eq)
                    (push sub-mh (mhs mapping))
                    (setf (descendants mapping)
                      (set-union! (descendants mapping) (descendants sub-mh)))
                    (setf (nogoods mapping)
                      (set-union! (nogoods mapping) (nogoods sub-mh)))
                      (incf (score mapping) (score sub-mh))))
                   (descendants sub)))
      (setf (cdr entry) (cons :integrated subs)))
   entry)

;;; Check the Completeness of a Mapping in terms of Entitites

;; Given a mapping, determine if the mapping uses all of either the
;; base or the target entities that are available.

(defun complete-mapping? (mapping sme)
  "Given a mapping, determine if the mapping uses all of either the
   base or the target entities that are available."
  ;; True if the number of entity-mhs is equal to the number
  ;;  of either the base or target entities.
  (let ((num-entity-mhs (length (entity-mhs mapping))))
    (or (= num-entity-mhs (length (entities (base sme))))
        (= num-entity-mhs (length (entities (target sme)))))))

;;; Top-Level Expression Handling for Mappings

;; Given a mapping, go through the MHs and determine which are top-level.
;; Save the top-level MHs in the designated slot, for use during CI creation.

(defun aggregate-top-level-mhs (mapping)
  (setf (top-level-mhs mapping)
    (remove-if-not #'(lambda (mh)
                       (and (expression? (base-item mh))
                            (or (top-level? (base-item mh))
                                (top-level? (target-item mh)))))
                   (mhs mapping))))


;;; Utilities 

(defmethod mh-containing-base-item ((item item)(mapping mapping))
  (dolist (mh (mhs mapping))
    (if (eq (base-item mh) item) (return-from mh-containing-base-item mh))))

(defmethod mh-containing-target-item ((item item)(mapping mapping))
  (dolist (mh (mhs mapping))
    (if (eq (target-item mh) item)
        (return-from mh-containing-target-item mh))))

(defmethod mh-containing-base-item ((p predicate)(mapping mapping))
   (dolist (mh (mhs mapping))
      (if (eq (base-item mh) p)
         (return-from mh-containing-base-item mh))))

(defmethod mh-containing-target-item ((p predicate)(mapping mapping))
   (dolist (mh (mhs mapping))
      (if (eq (target-item mh) p)
         (return-from mh-containing-target-item mh))))

;;; Next two make the candidate inference calculation simpler
(defmethod mh-containing-base-item ((arbitrary t)(mapping mapping))
   nil)

(defmethod mh-containing-target-item ((arbitrary t)(mapping mapping))
   nil)

(defun map# (id)
  (dolist (root (kernel-mappings sme::*sme*))
   (if (= id (id root)) (return-from map# root)))
  (dolist (m (mappings-cache sme::*sme*))
   (if (= id (id m)) (return-from map# m))))

(defun kernels-containing-mh (mh &optional (*sme* *sme*))
  (remove-if-not #'(lambda (mapping)
                     (member mh (mhs mapping) :test #'eq))
                 (kernel-mappings *sme*)))

(defmacro mapping? (possible-mapping)
  `(typep ,possible-mapping 'mapping))

(defmethod entity-mhs ((mapping mapping))
  (remove-if-not #'entity? (mhs mapping)))

(defun submap-of? (mapping1 mapping2)
  "True if mapping1 is a submapping of mapping2."
  (null (set-difference (root-mhs mapping1) (root-mhs mapping2) :test #'eq)))

(defun mh-intersects-mapping? (mh mapping)
  (some (lambda (root-mh)
          (intersection (descendants root-mh) (descendants mh)))
        (root-mhs mapping)))

(defun mh-consistent-with-mapping? (mh mapping)
  (every (lambda (root-mh)
           (and (not (intersection (descendants root-mh) (nogoods mh)))
                (not (intersection (nogoods root-mh) (descendants mh)))))
         (root-mhs mapping)))

;;; Return the current mappings in their order pulled in the greedy
;;;  merge, rather than by score.  Requires that you have the list
;;;  of candidate kernel mappings that were used in the greedy merge.
(defun mappings-ordered-by-greed (candidates produced-mappings)
  "Return the produced mappings in the order of the candidates used
    to produce them."
  (let ((greed-order nil))
    (dolist (candidate candidates)
      (dolist (produced-mapping produced-mappings)
	(when (submap-of? candidate produced-mapping)
	  (push produced-mapping greed-order)
	  (return)))
      (setq produced-mappings (remove (car greed-order)
				      produced-mappings))
      (when (null produced-mappings) (return)))
    (reverse greed-order)))

(defun check-for-greedy-inversion (candidates mappings)
  "Check to see if the greedy merge has produced a mapping order
   that is different from the order that kernels were plucked
   from the list."
  (let ((greedy-order (mappings-ordered-by-greed candidates mappings)))
    (when (and (not (equal mappings greedy-order))
	       *warn-on-greedy-inversion?*)
      (warn "Greedy inversion occurred.")
      (format *error-output* "~%==PRODUCED ORDERING: ~A~%" mappings)
      (format *error-output* "==ORIGINAL ORDER: ~A~%" greedy-order))))


;;;; Commutative Table Data Structures
;;;;------------------------------------------------------------
;;;  Commutative tables are the things in the commutatives and 
;;;   possible-children slots that give a set of arguments
;;;   and how they might be resolved.
;;;
;;; Commutative tables provide, for each element in the 
;;; base and target, the mh's involving it that are still 
;;; consistent with the mapping. (The reason for storing 
;;; both base and target is because the nary case will need this.)
;;; If any entry is null for a non-nary commutative predicate, 
;;; then that cmh isn't consistent with the mapping (and the 
;;; mapping should be tossed).  If exactly one entry exists for 
;;; each, then the union of them should suffice as arguments.
;;;
;;; A commutative table is a list of entries, where each entry is of
;;; the following format:
;;;   (<MH> 
;;;     ((:base . item1) <mh> <mh> <mh>)
;;;     ((:base . item2) ...)
;;;     ((:target . item1) <mh> ...)
;;;      ...)
;;;
;;;   Instead of :base ... etc. the cdr of the entry can also be 
;;;     (<mh> :integrated <argmh1> <argmh2> ...)
;;;   Where <argmh..> are the argument MHs that have been
;;;     decided on.
;;;
;;;  The top level is called a commutative table.  Each entry
;;;  in the table is called a commutative-table-entry (or ct-entry)  Each of 
;;;  entries within the entry is called a choice-set.

(defun make-comm-table ()
  "Constructor for Commutative table object."
  nil)

(defun find-comm-table-entry (mh comm-table)
  "Find the entry for a given match hypothesis in the command table."
  (assoc mh comm-table))

(defun comm-table-all-choice-sets (comm-table)
  "Return a list of all the choice sets available in the given
   commutatives table, without the integrated entries."
  (setq comm-table (remove-if 'integrated-ct-entry? comm-table))
  (mapcan #'(lambda (ct-entry)
	      (copy-list (ct-entry-choice-sets ct-entry)))
	  comm-table))

;;; Constructor for Commutative Table Entries

(defun make-ct-entry (cmh mapping)
  "Make a single entry for a commutative table."
   (cons cmh ;; commutative match hypothesis
         ;; Entries are (<base or target> . <children mh's>)
         (let ((table nil))
           (dolist (child (possible-children cmh) table)
             (let ((entry (assoc (cons :base (base-item child)) table
                                 :test #'equal)))
              (unless entry
                (push (setq entry 
                            (cons (cons :base (base-item child)) nil))
                            table))
               (when (mutually-consistent? child mapping)
                  (push child (cdr entry)))
              ;; Must create all entries
              (setq entry (assoc (cons :target (target-item child))
                                 table :test #'equal))
              (unless entry
                (push (setq entry (cons (cons :target (target-item child))
                                        nil))
                            table))
               (when (mutually-consistent2? child mapping)
                  (push child (cdr entry)))
               )))))

(defun integrated-ct-entry? (entry)
  "Is the given commutative table entry integrated?"
  (eql (car (ct-entry-choice-sets entry)) ':integrated))

(defun integratable-ct-entry? (entry)
  "Is the ct-entry integratable.  In other words, is there only
    one entry for each base and target argument?  Does not
    check to see if the entry is already integrated."
  (every 'ct-choice-set-single-choice? (ct-entry-choice-sets entry)))

(defun inconsistent-ct-entry? (entry)
   "An entry is inconsistent if any of its choice sets is empty.
   If an n-ary MH is the index, then all of the choice sets must
   be empty."
   (let ((choice-sets (ct-entry-choice-sets entry))
         (mh (ct-entry-mh-key entry)))
      (if (n-ary? mh)
         (every #'ct-choice-set-empty? choice-sets)
         (some #'ct-choice-set-empty? choice-sets))))

;;; Selectors
(defun ct-entry-mh-key (entry) (car entry))

(defun ct-entry-choice-sets (entry) (cdr entry))

(defun ct-entry-base-choice-set (ct-entry base-item)
  "Return the set of possible MHs for a given base item."
  (assoc (cons ':base base-item) ct-entry :test #'equal))

(defun (setf ct-entry-choice-sets) (new-value ct-entry)
   "Set the value of the choice sets in a ct-entry."
   (setf (cdr ct-entry) new-value))

(defun ct-entry-target-choice-set (ct-entry target-item)
  (assoc (cons ':target target-item) ct-entry :test  #'equal))

;;; Commutative Constraint Data Object

(defun ct-choice-set-side (cset)
  "Side of the choice set--either :base or :target"
  (caar cset))

(defun ct-choice-set-item (cset)
  "The item this is a choice set for."
  (cdar cset))

(defun ct-choice-set-key (cset)
  "Key for choice set (consists of side and item."
  (car cset))

(defun ct-choice-set-mhs (cset)
  "The MHs we can choose from."
  (cdr cset))

(defun (setf ct-choice-set-mhs) (new-value choice-set)
   "Set the value of the mhs in the choice set."
   (setf (cdr choice-set) new-value))

;; Predicates
(defun ct-choice-set-empty? (choice-set)
  "A choice-set is empty if is not integrated and if it has no mhs left."
  (null (ct-choice-set-mhs choice-set)))

(defun ct-choice-set-single-choice? (choice-set)
  "A choice set is constrained if it has only a single mh entry left."
  (let ((mhs (ct-choice-set-mhs choice-set)))
    (and (not (null mhs))
         (null (cdr mhs)))))
