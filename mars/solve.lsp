;; -*- mode: lisp -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; MARS: simple equation solver
;;
;;; last edited 2/5/94, by kdf

(in-package :common-lisp-user)

(defun solve-for (q eqn &optional (*mars* *mars*))
  ;; solves for <q> in <eqn>
  ;; first, gather up numerical information needed
  (let ((variables
	 (delete q (extract-algvars-from eqn)
		 :test #'equal))
	(values-alist nil))
    (setq values-alist (get-values-for-variables variables))
    (let ((losers (remove-if-not #'(lambda (entry) (eq (car entry) :unknown))
				 values-alist)))
      ;; pass back reason for failure, for search suggestions.
      (when losers (return-from solve-for (values nil losers))))
    ;; otherwise, it's time to solve it!
    (let* ((new-eqn (sublis values-alist eqn :test #'equal))
	   ;; here's where the real work happens
	   (algebra-problem (setup-algebra-problem
			     (simplify new-eqn) q))
	   (result nil))
      (mars-debugging
       (format t "~% attempting to solve equation:")
       (let ((*print-level* nil)(*print-length* nil))
	 (pprint new-eqn))) 
      (solve-algebra-problem algebra-problem)
      (cond ((algebra-problem-solution algebra-problem)
	     (setq result (path-current (algebra-problem-solution
					 algebra-problem)))
	     (cond ((and (eq (car result) '=)
			 (equal (cadr result) q)
			 (numberp (third result)))
		    (values t (third result)
			    `((equation ,eqn)
			      ,@ (mapcar #'(lambda (pair)
					     `(nvalue ,(car pair)
						      ,(cdr pair)))
					 values-alist))))
		   (t (values nil result
			      `((equation ,eqn)
			      ,@ (mapcar #'(lambda (pair)
					     `(nvalue ,(car pair)
						      ,(cdr pair)))
					 values-alist))))))
	    (t (values nil :failed-solution))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; manipulating lists of parameters

(defun get-values-for-variables (var-list)
  (mapcar
   #'(lambda (var)
       (multiple-value-bind (known? value exp)
           (lookup-nvalue var (working-memory *mars*))
         (declare (ignore exp))
	(cond (known? (cons var value))
	      (t (cons var :unknown))))) var-list))

(defun unknown-eqn-antecedents (q eqn)
  (delete nil
	  (mapcar #'(lambda (entry)
		      (if (eq (cdr entry) :unknown)
			  (car entry) nil))
		  (get-values-for-variables
		   (delete q (extract-algvars-from eqn)
			   :test #'equal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; some utilities extracted from cyclepad's eqn2con module.

(defparameter *algops* '(= + - * / sqr sqrt log exp expt)) ;; add if others

(defvar *vars-so-far* nil)

(defun algebraic-expression? (expr)
  (and (listp expr) (member (car expr) *algops*)))

(defun extract-algvars-from (expr)
  (let ((*vars-so-far* nil))
    (extract-algvars-from1 expr)
    *vars-so-far*))

(defun extract-algvars-from1 (expr)
  (cond ((or (null expr) (not (listp expr)))) ;; punt
        ((member (car expr) *algops*) ;; go down rest of expressions
         (dolist (term (cdr expr)) (extract-algvars-from1 term)))
        (t ;; presumably if this is the wrong thing, asserting an nvalue
         ;; of it won't hurt
         (pushnew expr *vars-so-far* :test #'equal))))

