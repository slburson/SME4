;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: filters.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ken Forbus
;;;;   Created: January 17, 1999 00:18:39
;;;;   Purpose: Implementation of match filters
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2012-04-19 06:19:43 -0500 (Thu, 19 Apr 2012) $
;;;;  $LastChangedBy: friedman $
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;; This version implements filters via objects, so that methods can dispatch
;;; on the type of filter.  Each filter object has an sme-filter-violated? method
;;; to figure out whether a mapping violates it or not.

(defclass sme-filter () ())

(defmethod sme-filter? ((thing t)) nil)
(defmethod sme-filter? ((thing sme-filter)) t)

(defmethod sme-filter-violated? ((mapping t) (filter t)) nil)

;; You can always ask, but if it isn't there, it isn't there.
(defmethod base-item ((thing sme-filter)) nil)
(defmethod target-item ((thing sme-filter)) nil)

;; Identical functions filter.  When in force, this
;; forbids non-identical function mappings.  Used only in certain
;; kinds of tasks, like textbook problem solving, where functions
;; that denote quantity types aren't the kinds of things that one typically
;; wants to allow substitutions for, when solving within-domain analogous problems.

(defclass identical-functions-filter (sme-filter) ())

(defun identical-functions-exp? (exp)
  ;; The comma versus camelCase distinctions are for backward
  ;; compatibility with older external knowledge and reasoning systems
  (or (eq (car exp) 'data::identical-functions)
      (eq (car exp) 'data::identicalFunctions)))

(defmethod sme-filter-violated? ((mapping mapping) (filter identical-functions-filter))
   (some #'(lambda (mh)
             (and (functor? (base-item mh))
                  (functor? (target-item mh))
                  (function? (base-item mh))
                  (function? (target-item mh))
                  (not (equal (base-item mh) (target-item mh)))))
     (mhs mapping)))

;; Required correspondence filter.  When in force, requires that every mapping
;; place the specified items in correspondence.  Useful when task constraints place
;; items in correspondence, e.g., someone tells you that X goes with Y, or when
;; extending a match based on a previous match.
;; For learning new domains, required correspondences enable persistent mappings 
;;   across domains to be learned incrementally, e.g. Klenk & Forbus, 2013,
;; Exploiting persistent mappings in cross-domain analogical learning of physical
;; domains, Artificial Intelligence, 195:398:417.

(defclass required-correspondence-filter (sme-filter)
   ((base-item :reader base-item :initarg :base-item)
    (target-item :reader target-item :initarg :target-item)))

(defun required-correspondence-exp? (exp)
  (or (eq (car exp) 'data::required-correspondence)
      (eq (car exp) 'data::requiredCorrespondence)))

(defmethod sme-filter-violated? ((mapping mapping) 
                                 (filter required-correspondence-filter))
   (some #'(lambda (mh)
             (or (and (eq (base-item mh) (base-item filter))
                      (not (eq (target-item mh) (target-item filter))))
                 (and (not (eq (base-item mh) (base-item filter)))
                      (eq (target-item mh) (target-item filter)))))
         (mhs mapping)))

(defmethod required-correspondence-filter? ((filter t)) nil)
(defmethod required-correspondence-filter? ((filter required-correspondence-filter)) t)

;; Excluded correspondence filter.  When in force, forbids any mapping from
;; placing this pair of items in correspondence.  Useful when exploring alternate
;; solutions, e.g., add one of these filters for a correspondence that seems
;; inappropriate and remap.

(defclass excluded-correspondence-filter (sme-filter)
    ((base-item :reader base-item :initarg :base-item)
     (target-item :reader target-item :initarg :target-item)))

(defun excluded-correspondence-exp? (exp)
  (or (eq (car exp) 'data::excluded-correspondence)
      (eq (car exp) 'data::excludedCorrespondence)))
    
(defmethod sme-filter-violated? ((mapping mapping)
                                 (filter excluded-correspondence-filter))
   (some #'(lambda (mh)
             (and (eq (base-item mh) (base-item filter))
                  (eq (target-item mh) (target-item filter))))
         (mhs mapping)))


(defmethod excluded-correspondence-filter? ((filter t)) nil)
(defmethod excluded-correspondence-filter? 
    ((filter excluded-correspondence-filter)) t)

;; Exclude Cross-Partition correspondences is a type-level constraint.
;; It forbids entities that are known to be members of specific different ontological
;; categories to be placed into correspondence. This is useful for complex
;; within-domain analogies.    
;; Partition constraints are only applied when there are explicitly known attributes,
;; supplied by whatever system created the case.

(defclass exclude-cross-partition-correspondences (sme-filter)
  ((partitions :reader partitions :initarg :partitions)))

(defmethod exclude-cross-partition-correspondences-exp? (exp)
  (and (consp exp)
       (or (eq (car exp) 'data::excludeCross-PartitionCorrespondences)
           (eq (car exp) 'data::exclude-cross-partition-correspondences))))

(defmethod sme-filter-violated? ((mapping mapping)
                                 (filter exclude-cross-partition-correspondences))
  (dolist (entity-mh (entity-mhs mapping))
    ;; Collect the attributes for the base.
    (let* ((base-attributes (get-entity-attributes (base-item entity-mh)))
           (base-partitions (intersection base-attributes (partitions filter))))
      (cond ((null base-partitions) nil) ;; Presume okay due to lack of knowledge
            (t (let* ((target-attributes (get-entity-attributes (target-item entity-mh)))
                      (target-partitions (intersection target-attributes 
                                                       (partitions filter))))
                 (cond ((null target-partitions) nil)
                       ((or (set-difference base-partitions
                                            target-partitions)
                            (set-difference target-partitions
                                            base-partitions))
                        (return-from sme-filter-violated? (values t))))))))))

;;; Required partition constraints are also type-level constraints, 
;;; similarly useful for complex within-domain analogies

(defclass require-within-partition-correspondences (sme-filter)
  ((partition :reader partition :initarg :partition)))

(defmethod require-within-partition-correspondences-exp? (exp) 
  (and (consp exp)
       (or (eq (car exp) 'data::requireWithinPartitionCorrespondences)
           (eq (car exp) 'data::require-within-partition-correspondences))))

(defmethod sme-filter-violated? ((mapping mapping)
                                 (filter require-within-partition-correspondences))
  (dolist (entity-mh (entity-mhs mapping))
    (let ((base-attributes (get-entity-attributes (base-item entity-mh)))
          (target-attributes (get-entity-attributes (target-item entity-mh))))
      (cond ((member (partition filter) base-attributes)
             (unless (member (partition filter) target-attributes)
               ;; Got a mismatch
               (return-from sme-filter-violated? (values t))))
            ((member (partition filter) target-attributes)
             (return-from sme-filter-violated? (values t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Redundant lisp form of filter, used for serialization

(defgeneric filter-list-form (filter &optional mixed-case?)
  (:documentation
   "Gives the list form of the filter"))

(defmethod filter-list-form ((thing t) &optional mixed-case?) 
  (declare (ignore mixed-case?))
  nil)

(defmethod filter-list-form ((filter-list list) &optional (mixed-case? t)) 
  (delete nil
          (mapcar #'(lambda (member)
                      (filter-list-form member mixed-case?))
            filter-list)))
  
(defmethod filter-list-form ((filter excluded-correspondence-filter)
                             &optional (mixed-case? t))
  (list (if mixed-case? 'data::excludedCorrespondence 'data::excluded-correspondence)
        (user-form (base-item filter)) (user-form (target-item filter))))
                              
(defmethod filter-list-form ((filter identical-functions-filter)
                             &optional mixed-case?)
  (list (if mixed-case? 'data::identicalFunctions 'data::identical-functions)))
                                                                                                                                                            
(defmethod filter-list-form ((filter required-correspondence-filter)
                             &optional (mixed-case? t))
  (list (if mixed-case? 'data::requiredCorrespondence 'data::required-correspondence)
        (user-form (base-item filter)) (user-form (target-item filter))))

(defmethod filter-list-form ((filter exclude-cross-partition-correspondences)
                             &optional (mixed-case? t))
  (list (if mixed-case? 'data::excludeCross-PartitionCorrespondences
          'data::exclude-cross-partition-correspondences)
        (mapcar 'lisp-form (partitions filter))))

(defmethod filter-list-form ((filter require-within-partition-correspondences)
                             &optional (mixed-case? t))
  (list (if mixed-case? 'data::requireWithinPartitionCorrespondences
          'data::require-within-partition-correspondences) (lisp-form (partition filter))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Using the filters in matching

;; The idea is to pass in as a filter procedure a closure that uses this
;; procedure on the set of filters associated with the match.

(defun check-sme-filters (mapping filters)
   (some #'(lambda (filter) (sme-filter-violated? mapping filter)) filters))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simplify setup

(defun create-sme-filter-list (sme &key (excluded nil)
                                   (required nil)
                                   (identical-functions? nil)                                
                                   (exclude-cross-partition nil)
                                   (require-within-partition nil))
  (let ((filters nil))
    (when identical-functions?
      (push (make-instance 'identical-functions-filter) filters))
    (when excluded
      (dolist (exclusion excluded)
        (let ((base-item (fetch-item (car exclusion) (base sme)))
              (target-item (fetch-item (cadr exclusion) (target sme))))
          (when (and base-item target-item)
            (push (make-instance 'excluded-correspondence-filter
                    :base-item base-item :target-item target-item)
                  filters)))))
    (when required
      (dolist (requirement required)
        (let ((base-item (fetch-item (car requirement) (base sme)))
              (target-item (fetch-item (cadr requirement) (target sme))))
          (when (and base-item target-item) 
                  (push (make-instance 'required-correspondence-filter
                          :base-item base-item :target-item target-item)
                        filters)))))
     (when exclude-cross-partition
       (dolist (requirement exclude-cross-partition)
         (let ((partitions (mapcar #'(lambda (attribute)
                                       (find-predicate attribute
                                                       (vocabulary sme)))
                             requirement)))
           (when partitions
             (push (make-instance 'exclude-cross-partition-correspondences
                     :partitions partitions)
                   filters)))))
     (when require-within-partition
       (dolist (requirement require-within-partition)
         (let ((partition (find-predicate requirement (vocabulary sme))))
           (when partition
             (push (make-instance 'require-within-partition-correspondences
                     :partition partition)
                   filters)))))
      filters))

(defun create-filters-from-assertions (sme filter-assertions)
  (create-sme-filter-list 
   sme
   :excluded (mapcar 'cdr
               (delete nil (remove-if-not #'excluded-correspondence-exp?
                                          filter-assertions)))
   :required (mapcar 'cdr
               (delete nil (remove-if-not #'required-correspondence-exp?
                                          filter-assertions)))
   :identical-functions? (some #'identical-functions-exp? 
                               filter-assertions)
   :exclude-cross-partition
   (mapcar #'(lambda (asn)
               ;; Assume form is (<pred> (TheSet . <list of attributes>))
               (cdr (cadr asn)))
     (delete nil
             (remove-if-not #'exclude-cross-partition-correspondences-exp?
                            filter-assertions)))
   :require-within-partition
   (mapcar 'cadr 
     (delete nil (remove-if-not #'require-within-partition-correspondences-exp?
                                filter-assertions)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Applying filters within the context of a specific match

(defun find-mh-exclusions (sme)
  ;; Translates the exclusion constraints into a list of mh's to exclude
  (delete nil
          (mapcar #'(lambda (constraint)
                      (when (typep constraint 'excluded-correspondence-filter)
                        ;; If the MH isn't here by now, it's not going to be.
                        (find-indexed-mh (base-item constraint) (target-item constraint) sme)))
            (match-filters sme))))

(defun filtered-or-excluded? (mapping filter excluded-mhs)
  ;; Used in greedy merge
  (or (funcall filter mapping) ;; For pragmatic marking, other task filters
      (some #'(lambda (loser) (member loser (mhs mapping))) excluded-mhs)))

(defun find-mh-constraint-solutions (sme)
  ;; Computes seed mapping from required constraints
  (let* ((mhs (find-or-make-required-mhs sme)))
    (if mhs (list (make-temp-mapping mhs sme))
      nil)))
         
(defun find-or-make-required-mhs (sme)
  (delete nil
          (mapcar #'(lambda (constraint)
                      (cond ((typep constraint 'required-correspondence-filter)
                             (let ((mh (find-indexed-mh (base-item constraint)
                                                        (target-item constraint) sme)))
                               (cond ((mh? mh) mh)
                                     (t ;; When dealing with projection tasks, there may not
                                      ;; be a preexisting match, so we create one in order to
                                      ;; make the candidate inferences that we're looking for.
                                      (let ((new-mh
                                             (create-local-match (base-item constraint) 
                                                                 (target-item constraint) 
                                                                 :required sme)))
                                        (update-mh-structural-relations new-mh sme) 
                                        new-mh)))))
                            (t nil)))
            (match-filters sme))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code                          

