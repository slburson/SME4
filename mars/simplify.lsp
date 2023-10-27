;; -*- mode: lisp; -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; algebraic simplifier
;; last edited 1/29/93, by kdf

;;; copyright (c) 1993, kenneth d. forbus, northwestern university,
;;; and johan de kleer, the xerox corporation.
;;; all rights reserved.

;;; this version is inspired by one of g.j. sussman's scheme matchers.

(in-package :common-lisp-user)

(defvar *simplify-cache* (make-hash-table :test #'equal))

(defun simplify (exp)
  (declare (special *algebra-rules*))
  (or (gethash exp *simplify-cache*)
      (setf (gethash exp *simplify-cache*) 
	    (simplify-it exp *algebra-rules*))))

(defun simplify-it (exp rules &aux result)
  (setq result
	(try-matcher-rules
	 (if (listp exp) (mapcar #'simplify exp)
	   exp)
	 rules))
  (if (equal result exp) result
    (simplify-it result rules)))

(defun try-matcher-rules (exp rules)
 ;; return the original expression by default
  (dolist (rule rules exp)
    (let ((bindings (match (rule-pattern rule) exp nil)))
;;      for debugging
;;      (unless (eq bindings :fail)
;;      (format t "~% matched ~a on ~a." exp rule)
;;      (dolist (binding bindings)
;;      (format t "~% ~a: ~a." (car binding)
;;       (var-value (list '? (car binding)) bindings))))
      (unless (eq bindings :fail)
       (return-from try-matcher-rules
		    (substitute-in (rule-result rule)
                                   bindings))))))

(defun rule-pattern (rule) (car rule))
(defun rule-result (rule) (cadr rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; algebra utilities

(defun alg< (e1 e2) ;; sort predicate for algebraic expressions
  (cond ((equal? e1 e2) nil)
	((consp e1)
	 (if (consp e2)
	     (if (equal? (car e1) (car e2))
		 (alg< (cdr e1) (cdr e2))
		 (alg< (car e1) (car e2)))
	     nil))
	((consp e2) t)
	((symbolp e1)
	 (if (symbolp e2)
	     (string< (symbol-name e1) (symbol-name e2))
	     nil))
	((symbolp e2) t)
	((and (numberp e1) (numberp e2)) (< e1 e2))
	(t (error "alg< cannot compare these: ~a, ~a." e1 e2))))

(defun alg= (e1 e2) (not (or (alg< e1 e2) (alg< e2 e1))))
 
(defun sorted? (list pred)
  (cond ((or (null list) (null (cdr list))) t)
	((funcall pred (cadr list) (car list)) nil)
	(t (sorted? (cdr list) pred))))

(defun +/*? (exp) (or (eq exp '+) (eq exp '*)))

(defun same-constant? (exp constant)
  (and (numberp exp)
       (if (floatp exp) (equal? exp (float constant))
	   (= exp constant))))

(defun zero? (exp) (same-constant? exp 0))
(defun one? (exp) (same-constant? exp 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; rules for algebraic simplification

(defparameter *algebra-rules* '(

;; flush degenerate cases
(((? op +/*?) (? e)) (? e))
((+ (? zero zero?) (?? e)) (+ (?? e)))
((- (? zero zero?) (? e)) (- (? e)))
((- (? e) (? zero zero?)) (? e))
((- (? e) (? e)) 0)
((* (? one one?) (?? e)) (* (?? e)))
((* (? zero zero?) (?? e)) 0)
((expt (? e) (? zero zero?)) 1)
((expt (? e) (? one one?)) (? e))
((exp (? arg numberp)) (:eval (exp (? arg))))
((log (? arg numberp)) (:eval (log (? arg))))
((log (? one one?) (? base)) 0)
((log (? base) (? base)) 1)
((log (expt (? base) (? val)) (? base)) (? val))
((expt (? base) (log (? val) (? base))) (? val))
;; equivalences involving powers
((* (? e) (? e)) (sqr (? e)))
((expt (? e) (? two (lambda (exp) (same-constant? exp 2))))
 (sqr (? e)))
((sqrt (sqr (? e))) (abs (? e)))
((sqr (sqrt (? e))) (? e))

;; combine numerical constants
(((? op +/*?) (? e1 numberp) (? e2 numberp) (?? e3))
 ((? op) (:eval ((? op) (? e1) (? e2))) (?? e3)))
((- (? e1 numberp) (? e2 numberp)) (:eval (- (? e1) (? e2))))
((- (? e1 numberp)) (:eval (- (? e1))))
((sqr (? e1 numberp)) (:eval (* (? e1)  (? e1))))
((sqrt (? e1 numberp)) (:eval (sqrt (? e1))))
((expt (? e1 numberp) (? e2 numberp)) (:eval (expt (? e1) (? e2))))
((/ (? e1 numberp) (? e2 numberp)) (:eval (/ (? e1) (? e2))))
((abs (? e numberp)) (:eval (abs (? e))))
((log (? x numberp) (? base numberp)) 
 (:eval (/ (log (? x)) (log (? base)))))
;; flatten +,*
(((? op +/*?) (?? e1) ((? op) (?? e2) (?? e3)))
 ((? op) (?? e1) (?? e2) (?? e3)))
(((? op +/*?) ((? op) (?? e1) (?? e2)) (?? e3))
 ((? op) (?? e1) (?? e2) (?? e3)))
;; canonicalize +,*
(((? op +/*?) (?? terms
		  (lambda (terms) (not (sorted? terms #'alg<)))))
 ((? op) (:splice (:eval (sort (quote (? terms)) #'alg<)))))))
