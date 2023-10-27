;; -*- mode: lisp -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; MARS: implementation of operators
;;
;; last edited 2/4/94, by kdf

(in-package :common-lisp-user)

;;;; operators in MARS

;; okay, this structure is a hairy kludge.  that's deliberate.
;; all the apologies are here, the rest of the code speaks for itself.

(defun mars-operator-applier (current-state fake-operator
					      &optional (*mars* *mars*))
  ;;; figures out for each kind of goal what operators should be tried
  ;;; for it.
  ;;; start by seeing if the goal is moot.
  (declare (ignore fake-operator))
  (when (or (null current-state)
	    (null (current-goal current-state)))
	  ;; should probably error out here, but let's not.
	  (return-from mars-operator-applier nil))
  (let ((current-goal (current-goal current-state))
	(goal-stack (goal-stack current-state)))
  (when (failed? current-goal) ;; do nothing
	(return-from mars-operator-applier nil))
  (when (achieved? current-goal) ;; pop goal stack
	(return-from mars-operator-applier
		     (list (cons `(already-achieved ,current-goal)
				 (make-new-state (car goal-stack)
						 (cdr goal-stack))))))
  (multiple-value-bind (achieved? result new-goals-sets)
		       (try-goal current-goal)
  (cond (achieved? ;; in this case, pop the goal stack
	 (if new-goals-sets ;; list of alternate stack extensions
	     (mapcar #'(lambda (new-goals-set)
			       (cons result
				     (make-new-state (car new-goals-set)
						     (append
						      (cdr new-goals-set)
						      goal-stack))))
			   new-goals-sets)
	   (list (cons result
		       (make-new-state (car goal-stack)
				       (cdr goal-stack))))))
	 (t (suggestions-for-goal current-goal goal-stack))))))

;;;; helper procedures

(defun failed? (goal)
  (funcall (lookup-procedure (car goal) *goal-failure-table* "failed")
	   goal))

  
(defun achieved? (goal)
  (funcall (lookup-procedure (car goal) *goal-achieved-table* "achieved")
	   goal))

(defun try-goal (goal)
  (funcall (lookup-procedure (car goal) *goal-try-table* "try")
	   goal))

(defun suggestions-for-goal (goal goal-stack)
  (funcall (lookup-procedure (car goal) *goal-suggestions-table*
			     "suggestions")
	   goal goal-stack))

(defun lookup-procedure (type alist tag)
  (let ((procedure (cdr (assoc type alist))))
    (if procedure procedure
      (error "no ~a procedure for ~a."
	     tag type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; these tables hold the procedures that process specific kinds of goals

(defvar *goal-failure-table* nil)
(defvar *goal-achieved-table* nil)
(defvar *goal-try-table* nil)
(defvar *goal-suggestions-table* nil)

;;; macrology for installing stuff

(defmacro install-failure-procedure (goal-type procedure)
  `(pushnew (cons ',goal-type ',procedure) *goal-failure-table*
	    :test #'equal))
(defmacro install-achieved-procedure (goal-type procedure)
  `(pushnew (cons ',goal-type ',procedure) *goal-achieved-table*
	    :test #'equal))
(defmacro install-try-procedure (goal-type procedure)
  `(pushnew (cons ',goal-type ',procedure) *goal-try-table*
	    :test #'equal))
(defmacro install-suggestions-procedure (goal-type procedure)
  `(pushnew (cons ',goal-type ',procedure) *goal-suggestions-table*
	    :test #'equal))


