;;; -*- Mode: Lisp; -*-
;;;; ------------------------------------------------------------
;;;; File name: sc.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;    Author: Ron Ferguson & Ken Forbus
;;;;  $LastChangedDate: 2014-08-06 12:33:00 -0500 (Wed, 06 Aug 2014) $
;;;;  $LastChangedBy: forbus $
;;;; ------------------------------------------------------------
;;;; These procedures calculate the structural consistency relationships
;;;; between newly-created match hypotheses.
;;;;
;;;; They are called from extend-mhs, after the set of mhs has been
;;;; extended, but before roots have been calculated.
;;;;

(in-package :sme)

;;;; How the structural consistency computations work:
;;;;
;;;;   First, a bottom-up propagation of incompleteness is done,
;;;;  to ensure that MHs with incomplete arguments are also marked
;;;;  as incomplete.
;;;;
;;;;   Second, the nogoods and descendants are initialized in each 
;;;;  MH to the local nogoods and the MH itself, respectively.
;;;;
;;;;   Third, the structural relations are again propagated from the 
;;;;  leaves up toward the higher-level root MHs.
;;;;
;;;;   Finally, the commutative children are filtered.  If only one
;;;;   consistent set of correspondences for commutative arguments
;;;;   remain, then those constraints are propagated as well.
;;;;   (Otherwise, there is still effectively a disjunction, and
;;;;    it would be extremely expensive to attempt to use these
;;;;    constraints.)
 
;;;;  calculate-structural-relations is the entry point
;;;;----------------------------------------------------------------------
(defgeneric calculate-structural-relations (sme)
  (:documentation 
   "Calculate the structural relations of MHs in SME.  This
     method may be subclassed when using different structural
     consistency algorithms."))
   
(defmethod calculate-structural-relations ((sme sme))
   "Calculate the structural relations of MHs in SME."
  (ensure-incompleteness-respected sme)
   ;; Use FIFO queue for bottom-up search.
   ;;   mh-queue: the queue itself
   ;;   new entries: new MHs added to the queue.
   (do ((mh-queue (initialize-mh-sc-fields sme)
                  (nconc (cdr mh-queue) new-entries))
        (new-entries nil nil))
       ((null mh-queue) sme) ; return sme when queue emptied.
     ;; Update descendants & nogoods based on argument MH's
     (dolist (maybe-new (update-mh-structural-relations (car mh-queue) sme))
       (unless (member maybe-new mh-queue) (push maybe-new new-entries))))
  (filter-commutative-mh-children sme))

;;;; Incompleteness Propagation
;; Incomplete mhs must not be included in any mapping.
;; Otherwise, parallel connectivity is violated.
;; If an MH is incomplete, so is every MH above it.
;; The code below enforces this.

(defun ensure-incompleteness-respected (sme)
   "For any incomplete MH in sme, make parent incomplete.  Returns sme."
   (declare (type sme sme))
   (dolist (mh (mhs sme))
     (when (incomplete? mh)
        (dolist (parent (parents mh))
           (propagate-incompleteness mh parent)))))

;; Add reason to parent's incomplete list, unless already there.
;; Or unless it is a commutative match hypothesis -- these
;; do their own calculation regarding incompleteness.
;; Also recursively propagate if reason is new.

(defmethod propagate-incompleteness ((child match-hypothesis) (parent match-hypothesis))
   "Propagate the incompleteness reason to parent, and call recursively."
  (declare (type match-hypothesis child parent))
  (unless (or (incomplete? parent) (commutative-mh? parent))
    ;; Communative check above is essential, do not remove!
    (mark-incomplete parent child ':incomplete-argument-mh)
    (dolist (grandparent (parents parent))
       (propagate-incompleteness parent grandparent))))

;;;; Initialization of Local Nogoods and Descendants
;;;;---------------------------------------------------------------------
(defun initialize-mh-sc-fields (sme)
  "Initialize MH structural relations by setting descendants to self,
    nogoods to local nogoods."
   (declare (type sme sme)
     (inline has-new-leaves?))
   (let ((new-leaves nil) (functor-mh nil))
      (dolist (mh (new-mhs sme))
         ; Set local descendants 
         (setf (descendants mh) (set-add mh (descendants mh)))
         (setq functor-mh (functor-mh mh))
         (when functor-mh 
            (setf (descendants mh) (set-add functor-mh (descendants mh))))
         ; Set local nogoods
         (set-mh-local-nogoods mh)
         ; Find new leaves
         (when (has-new-leaves? mh sme)
            (push mh new-leaves)))
      ; New entity MHs also has leaves.
      (setf new-leaves (append new-leaves (new-emaps sme)))
      ; Return new leaves
      (sort new-leaves #'order<)))

(defmethod has-new-leaves? (mh (sme sme))
   (declare (type match-hypothesis mh))
   (with-slots (functor-mh) mh
     (flet ((old-mh? (mh) 
                     (not (member mh (new-mhs sme) :test #'eq))))
       (or (and functor-mh (old-mh? functor-mh))
           (some #'old-mh? (arguments mh))))))
 
(defmethod set-mh-local-nogoods ((mh match-hypothesis))
  "Set the nogoods as the list of MHs that use the same 
   base or target as me, but are not me.  Returns BitSet."
  (with-slots (sme base-item target-item nogoods) mh
    (setq nogoods
          (set-union (mhs-involving-base base-item sme) 
                      (mhs-involving-target target-item sme)))
    (setq nogoods
          (set-delete mh nogoods))))

;;;; Propagation of Structural Relations from Child to Parent

(defgeneric update-mh-structural-relations (match-hypothesis sme)
  (:documentation 
   "Updates the structural relations of match hypotheses.  Returns
    the parents for continuing the propagation."))

(defmethod update-mh-structural-relations ((mh match-hypothesis) (sme sme))
  (dolist (arg-mh (if (functor-mh mh) 
                      (cons (functor-mh mh) (arguments mh))
                    (arguments mh)))
    ;; Update parent's structural relations with args' structural relns
     (setf (descendants mh)
           (set-union! (descendants mh) (descendants arg-mh)))
     (setf (nogoods mh)
           (set-union! (nogoods mh) (nogoods arg-mh))))
  (let ((inconsistent? (set-first-common-member
                        (nogoods mh) (descendants mh))))
    (when inconsistent?
      (mark-inconsistent mh inconsistent? ':inconsistent-via-arg)))
  (parents mh))

(defmethod update-mh-structural-relations
    ((cmh commutative-match-hypothesis) (sme sme))
  ;; Handle functor first.
  (with-slots (functor-mh) cmh
    (when functor-mh
       (setf (descendants cmh)
             (set-union! (descendants cmh) (descendants functor-mh)))
       (setf (nogoods cmh)
             (set-union! (nogoods cmh) (nogoods functor-mh)))))
  ;; Update parent's structural relations using nogoods and
  ;;  descendants implied by the choice sets for possible children
  (with-slots (descendants nogoods possible-children) cmh
    ;; Build commutative table dynamically.
    (let ((b-ctable (sort-mhs-by-arg #'base-item possible-children
                                     (arguments (base-item cmh))))
          (t-ctable (sort-mhs-by-arg #'target-item possible-children
                                     (arguments (target-item cmh)))))
      (setf (descendants cmh)
        (set-union! (descendants cmh)
                    (common-cmh-choice-set-properties
                     b-ctable t-ctable #'descendants)))
      (setf (nogoods cmh)
        (set-union! (nogoods cmh)
                    (common-cmh-choice-set-properties 
                     b-ctable t-ctable #'nogoods)))))
  ;; Check for inconsistency, given child consistency.
  (let ((inconsistent? (set-first-common-member
                        (nogoods cmh) (descendants cmh))))
    (when inconsistent?
      (mark-inconsistent cmh inconsistent? ':inconsistent-via-child)))
  ;; Return parents to continue the propagation
  (parents cmh))

(defun common-cmh-choice-set-properties (b-ctable t-ctable property-fn)
  ;; b-ctable is an alist whose entries are (<base item> . <mhs with that bi>)
  ;; t-ctable is the dual alist for target items
  ;; property-fn is the procedure for refering to descendants or nogoods.
  ;; Since each entry is a choice set, a descendant/nogood that is in common with
  ;; all of the entries in the choice set must be a descendant/nogood
  ;; of the commutative match hypothesis.  
  (flet ((find-common-mh-choice-set-property 
          (choices)
          (cond ((null choices) (make-empty-set))
                (t (let ((result (copy-set
                                  (funcall property-fn (car choices)))))
                     (dolist (mh (cdr choices) result)
                       (setq result
                             (set-intersection! result
                                                (funcall property-fn mh)))))))))
    (let ((new-constraints (make-empty-set)))
      (dolist (b-choiceset b-ctable)
        (let ((cset-constraints (find-common-mh-choice-set-property
                                 (cdr b-choiceset))))
          (setq new-constraints (set-union! new-constraints cset-constraints))))
      (dolist (t-choiceset t-ctable)
        (let ((cset-constraints (find-common-mh-choice-set-property 
                                 (cdr t-choiceset))))
          (setq new-constraints (set-union! new-constraints cset-constraints))))
      new-constraints)))
        
;;;; Filtering Commutative Mh Children

(defmethod filter-commutative-mh-children ((sme sme))
  "For all the commutative MHs, check to see of they now
   have inconsistent or incomplete children which can be
   eliminated.  If only a single consistent set of children
   remain, consolidate and propagate structural relations."
  (declare (type sme sme))
  (dolist (mh (mhs sme))
     (when (commutative-mh? mh)
        (filter-children-of mh))))                                            

(defun filter-children-of (mh)
  "For all the commutative MH children, check if the possible
   children now contain inconsitencies that allow some of 
   them to be removed.  If the remaining possible children
   allow for only a single interpretation, consolidate children
   as a single set of arguments, and propagate the descendants
   and the nogoods."
  (declare (type match-hypothesis mh))
  (flet (
         ;;;         (table-key (entry) (car entry)) ;; unused
         (table-value (entry) (cdr entry))
         (single-value? (entry) (and (cdr entry) (null (cddr entry)))))
    (with-slots (possible-children base-item target-item) mh
      ;; Remove incomplete or inconsitent mhs.
      (setf possible-children (delete-if #'mh-losing? possible-children))
      ;; Create MH argument tables.
      (let ((barg-mh-table (sort-mhs-by-arg #'base-item possible-children
                                            (arguments base-item)))
            (targ-mh-table (sort-mhs-by-arg #'target-item possible-children
                                            (arguments target-item)))
            (losers nil))
        (cond
           ((setq losers (remove-if #'table-value barg-mh-table))
            (mark-incomplete mh losers ':missing-base-args))
           ((setq losers (remove-if #'table-value targ-mh-table))
            (mark-incomplete mh losers ':missing-target-arg))
           ((and (every #'single-value? barg-mh-table)
                 (every #'single-value? targ-mh-table))
            (let ((arg-mhs (mapcar #'cadr barg-mh-table)))
               (setf (arguments mh) arg-mhs)
               ;; Update the descendants
               (dolist (arg-mh arg-mhs)
                  (setf (descendants mh)
                        (set-union! (descendants mh) (descendants arg-mh)))
                  (setf (nogoods mh)
                        (set-union! (nogoods mh) (nogoods arg-mh)))
                  )
               ;; Propagate structural relns to parents
               (propagate-structural-relations-upward mh))))))))

(defun propagate-structural-relations-upward (mh)
  "For this MH, update the structural relations of the parents of this
   MH, and continue upward until no parents are left.  Check to see if 
   parent is now inconsistent, and mark if so.  Returns NIL."
  (declare (type match-hypothesis mh))
  (do ((parents (parents mh) (append (cdr parents) new-parents))
       (parent (car (parents mh)) (car parents))
       (new-parents nil nil)
       (inconsistent-mh nil))
      ((null parents) nil)
    (setq new-parents (parents parent))
    ;; Propagate structural relations
     (setf (descendants parent)
           (set-union! (descendants parent) (descendants mh)))
     (setf (nogoods parent)
           (set-union! (nogoods parent) (nogoods mh)))
    ;; Check for inconsistency
    (setq inconsistent-mh
      (set-first-common-member (descendants parent) (nogoods parent)))
    (when inconsistent-mh
       (mark-inconsistent parent inconsistent-mh ':inconsistent-comm-child))))

;; Sort the MHs by the selector given, and place into table
;;  indexed by expression.
(defun sort-mhs-by-arg (selector mhs arg-expressions)
  (let ((table (mapcar #'(lambda (expr) (cons (cdr expr) nil))
                       arg-expressions)))
    (dolist (mh mhs table)
     (let* ((expr (funcall selector mh))
            (entry (assoc expr table)))
       (cond ((null entry) (error "~%MH misplaced: ~A in ~A"
                                  mh arg-expressions))
             (t (push mh (cdr entry))))))))
