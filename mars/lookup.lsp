;; -*- mode: lisp -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; looking up information in MARS
;;
;; last edited 2/4/94, by kdf

(in-package :common-lisp-user)

;;; information retreival problems in MARS:
;;
;;1. looking for facts currently believed about the problem.
;; this is accomplished by fetching information from the dgroup corresponding
;; to the problem.
;;
;; 2. looking for facts in the example with potential relevance to a
;; given goal.  for example, considering the goal
;;            (find (nvalue (nu-thermal cycle) ?value))
;; should cause the system to pull in everything it can find in the
;; example about (nu-thermal cycle).
;;
;; 3. looking for potentially useful candidate inferences.
;; this requires examining mappings.  
;; 
;; 4. background facts.  it seems only fair to have a stock of facts such
;;    as cp, cv, gamma values for ideal gases.
;;
;; to simplify operations, all of these descriptions will be stored as
;; dgroups.  ultimately, this means the system could compare solutions!
;;
;; the following dgroups are then needed:
;; problem -- what is given about the problem
;; example -- the entire example
;; working-ex -- the part of the example actually being compared at the moment
;; solution -- the solution as it is being built up.
;; background -- MARS' background knowledge.
;;

(defun lookup (pattern dgroup) 
  (let ((results nil))
    (map-expressions #'(lambda (exp)
			 (let ((bindings
				(unify (sme::user-form exp)
				       pattern)))
			   (unless (eq bindings :fail)
				   (push (cons exp bindings)
					 results))))
		     dgroup)
    results))

;; this should move into sme after the deadline.  the abstraction layer is
;; to allow us to optimize the dgroup implementation later on.

(defun map-expressions (procedure dgroup)
  (dolist (entry (sme::expressions dgroup))
    (dolist (expr (cdr entry))
      (funcall procedure expr))))

(defun expressions-mentioning (pattern dgroup)
  (let ((roots nil))
    (dolist (candidate-entry (lookup pattern dgroup) roots)
     ;; recall results of lookup are (<exp> . <bindings>)
     (dolist (root (sme::roots (car candidate-entry)))
	     (pushnew root roots)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  exploit expression structure to ask things like, "get the
;;  subexpression of x in which y appears such that the functor is z",
;;  i.e., extract the equation for (q heater2) from this expression if
;;  there is one.

(defun superexpressions-w/functor (subexpr pred)
  (do ((queue (copy-list (sme::parents subexpr))
	      (nconc (cdr queue) new))
       (new nil nil)
       (visited nil)
       (results nil))
      ((null queue) results)
      (unless (member (car queue) visited)
       (push (car queue) visited)
       (cond ((eq (sme::predicate (car queue)) pred)
	      (push (car queue) results))
	     (t (dolist (parent (sme::parents (car queue)))
		 (push parent new)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; special-case lookup routines

(defun lookup-nvalue (quantity dgroup)
  (dolist (candidate (lookup `(nvalue ,quantity ?value) dgroup)
		     (values nil nil nil))
   (let ((value (assoc '?value (cdr candidate))))
     (when (and value (numberp (cdr value)))
	   (return-from lookup-nvalue
			(values t (cdr value) (car candidate)))))))

(defun find-equations-mentioning (q dgroup)
  ;; search upward from the expression corresponding to q
  ;; to find all of them which are equations.
  (let ((q-expr (sme::fetch-item q dgroup)))
    (cond ((null q-expr) (values nil nil))
	  (t (let ((eqn-exprs
		    (superexpressions-w/functor
		     q-expr (sme::find-predicate 'equation
						 (sme::vocabulary dgroup)))))
	       (values (mapcar #'(lambda (eqn-expr)
				   (cadr (sme::user-form eqn-expr)))
			       eqn-exprs)
		       eqn-exprs))))))

(defun find-constraints-on (form dgroup)
  (let ((results (lookup `(implies ?antes ,form) dgroup)))
    ;; stub: probably want to do something more clever here. 
    (mapcar #'cadr results)))

(defun find-modeling-asn-derivation-for (q dgroup)
  (let (;; (qtype (car q))
	(entity (cadr q)))
    ;; this is grossly dependent on the form of modeling asns in
    ;; cyclepad
  (dolist (candidate (lookup `(derived-by (nvalue ,q (:skolem ?value))
					   (?asn-type ,entity))
			      dgroup))
    (let ((value (cdr (assoc '?value (cdr candidate))))
	  (asn-type (cdr (assoc '?asn-type (cdr candidate)))))
      (if (floatp value)
	  ;; go for it
	  (return-from find-modeling-asn-derivation-for
		       (values t value `(,asn-type ,entity))))))))

(defun show-nvalue-statements (dgroup
			       &optional (stream *standard-output*))
  (dolist (candidate (lookup '(nvalue ?q ?value) dgroup))
   (format stream "~% ~d = ~d"
	   (cdr (assoc '?q (cdr candidate)))
	   (cdr (assoc '?value (cdr candidate))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; manipulationg candidate inferences
;;
;; for simplicity, just dump them into a dgroup.
;; this makes them first-class entities that can be referred to.

(defun dgroup-for-cis (name mapping)
  (sme::define-description name nil
    (mapcar 'sme::lisp-form (sme::inferences mapping))))

