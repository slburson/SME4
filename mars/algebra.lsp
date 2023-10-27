;; -*- mode: lisp; -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; algebra system for analogical problem solver experiments
;;; last edited: 2/3/94, by kdf

;;; modified from building problem solvers version

(in-package :common-lisp-user)

#|
a problem consists of an equation in the unknown, x.  the goal is
to construct an equation that has just x on the left hand side,
and no occurences of x on the rhs.

a state in these problems has the form (= <lhs> <rhs>).  the
sides are just s-expressions corresponding to mathematical terms.
we provide a basic simplification program in another file to take
care of mundane stuff like collapsing constants.

this file contains the operator descriptions and cps hooks.  the
function setup-algebra-problem sets up the algebra problem space.
|#

(defvar *the-unknown* nil) ;; the classical unknown 
;; yes, yes, globals are ugly, but i'm in a hurry :-).

;; make code more readable
(defmacro lhs (x) `(cadr ,x))
(defmacro rhs (x) `(caddr ,x))

(defun occurs-in? (exp1 exp2) 
  (cond ((equal exp1 exp2) t)
  ((null exp2) nil)
  ((listp exp2)
   (or (occurs-in? exp1 (car exp2))
       (occurs-in? exp1 (cdr exp2))))))

(defun has-unknown? (exp) (occurs-in? *the-unknown* exp))
(defun no-unknown? (exp) (not (occurs-in? *the-unknown* exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; problem space interface procedures

(defun got-algebra-goal? (state)
  (and (equal (lhs state) *the-unknown*) ;; lhs = x
       (no-unknown? (rhs state)))) ;; no x's in rhs.

(defun find-algebra-operator (state operator)
;; operators take the form (<name> <procedure>)
  (funcall (cadr operator) state))

(defun print-derivation-step (state op-instance)
  (format nil "~a, via ~a" state (car op-instance)))

;;;; computing distance estimates for algebra problems
;; a reasonable heuristic is the sum of the depths of
;; occurrences of x in the expression tree.

(defun algebra-distance (expr)
  (labels ((sum-tree-depth
      (exp depth)
      (cond ((null exp) 0)
      ((equal exp *the-unknown*) depth)
      ((not (listp exp)) 0)
      (t (+ (sum-tree-depth (car exp) (1+ depth))
      (sum-tree-depth (cdr exp) depth))))))
    (+ (sum-tree-depth (lhs expr) 1)
       (sum-tree-depth (rhs expr) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; interface to system

(defstruct (algebra-problem (:print-function
                             (lambda (ap str ignore)
                               (declare (ignore ignore))
                               (format str "<algebra problem ~a>"
                                 (algebra-problem-name ap)))))
  (name nil)
  (problem-space nil)
  (expression nil)
  (unknown nil)
  (solution nil))

(defun setup-algebra-problem (expression unknown)
  (make-algebra-problem
   :name expression
   :expression expression
   :unknown unknown
   :solution nil
   :problem-space 
   (make-problem
    :name 'algebra
    :goal-recognizer 'got-algebra-goal?
    :operator-applier 'find-algebra-operator
    :state-printer #'(lambda (f) (format nil "~a" f))
    :solution-element-printer #'print-derivation-step
    :states-identical? 'equal
    :distance-remaining 'algebra-distance
     :operators '((isolate-log try-isolate-log)
                  (isolate-ln try-isolate-ln)
                  (isolate-sum try-isolate-sum)
                  (isolate-difference try-isolate-difference)
                  (isolate-difference try-isolate-difference2)
                  (isolate-difference try-isolate-unary-minus)
                  (isolate-square try-isolate-square)
                  (isolate-quotient try-isolate-quotient)
                  (isolate-inverse try-isolate-inverse)
                  (isolate-product try-isolate-product)
                  (collect-product-difference try-collect-prod-diff)
                  (attract-log-sum try-attract-log-sum)
                  (canonicalize try-canonicalization)
                  (try-flip try-flip)))))

(defun solve-algebra-problem (ap) ;; **** add stats counter?
  (declare (special *the-unknown*))
  (let ((*the-unknown* (algebra-problem-unknown ap)))
    (setf (algebra-problem-solution ap)
    (best-solve (algebra-problem-expression ap)
          (algebra-problem-problem-space ap)))))

;; a test case
(defvar *bundy* '(= (+ (log (+ x 1) e) (log (- x 1) e)) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; isolation techniques

(defun try-isolate-log (form &aux bindings)
  (setq bindings
  (match '(= (log (? arg has-unknown?)
      (? base no-unknown?))
       (? rhs no-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(isolate-log-instances ,form)
       (simplify
        (substitute-in `(= (? arg) (expt (? base) (? rhs)))
           bindings))))))

(defun try-isolate-ln (form &aux bindings)
  (setq bindings
  (match '(= (log (? arg has-unknown?))
       (? rhs no-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(isolate-log-instances ,form)
       (simplify
        (substitute-in `(= (? arg) (exp (? rhs)))
           bindings))))))

(defun try-isolate-square (form &aux bindings)
  (setq bindings
  (match '(= (sqr (? arg has-unknown?))
       (? rhs no-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(isolate-square ,form)
       (simplify (substitute-in `(= (? arg) (sqrt (? rhs)))
              bindings))))))

(defun try-isolate-quotient (form &aux bindings)
  (setq bindings
  (match '(= (/ (? arg1 has-unknown?)
          (? arg2 no-unknown?))
       (? rhs no-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(isolate-quotient ,form)
       (simplify (substitute-in `(= (? arg1) (* (? rhs) (? arg2)))
              bindings))))))

(defun try-isolate-inverse (form &aux bindings)
   (setq bindings
     (match '(= (/ (? arg1 no-unknown?)
                   (? arg2 has-unknown?))
                (? rhs))
       form))
   (unless (eq bindings :fail)
      `(,(cons `(isolate-inverse ,form)
           (simplify (substitute-in `(= (? arg1) (* (? rhs) (? arg2)))
                       bindings))))))

(defun try-isolate-product (form &aux bindings)
  (setq bindings
  (match '(= (* (? arg2 no-unknown?)
          (? arg1 has-unknown?))
       (? rhs no-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(isolate-product ,form)
       (simplify (substitute-in `(= (? arg1) (/ (? rhs) (? arg2)))
              bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun try-isolate-sum (form &aux bindings)
  (setq bindings
  (match '(= (+ (?? pre no-unknown?)
          (? arg has-unknown?)
          (?? post no-unknown?))
       (? rhs no-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(isolate-sum ,form)
       (simplify
        (substitute-in `(= (? arg)
         (- (? rhs) (+ (?? pre) (?? post))))
           bindings))))))

(defun try-isolate-difference (form &aux bindings)
  (setq bindings
  (match '(= (- (? arg1 has-unknown?)
          (? arg2 no-unknown?))
       (? rhs no-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(isolate-difference ,form)
       (simplify (substitute-in `(= (? arg1) (+ (? rhs) (? arg2)))
              bindings))))))

(defun try-isolate-difference2 (form &aux bindings)
   (setq bindings
     (match '(= (- (? arg1 no-unknown?)
                   (? arg2 has-unknown?))
                (? rhs no-unknown?))
       form))
   (unless (eq bindings :fail)
      `(, (cons `(isolate-difference ,form)
            (simplify (substitute-in `(= (? arg2) (- (? arg1) (? rhs)))
                        bindings))))))

(defun try-isolate-unary-minus (form &aux bindings)
   (setq bindings
     (match `(= (- (? arg has-unknown?)) (? rhs no-unknown?)) form))
   (unless (eq bindings :fail)
      `(,(cons `(isolate-difference ,form)
           (simplify (substitute-in `(= (? arg) (* -1 (? rhs)))
                       bindings))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; collection methods

;;; sum collection:
;;; given (u+v)*(u-v), turn it into u^2-v^2 
;;; use only on "least dominating terms" in x.
;;; u must have an occurrence of x.

(defun find-least-dominating-terms (exp &aux result xts)
  (cond ((or (null exp) (not (listp exp))) nil)
  (t (setq xts (remove-if #'no-unknown? exp))
     (cond ((cdr xts) (setq result (list exp))
      (dolist (xt xts result)
       (setq result
       (nconc result
        (find-least-dominating-terms xt)))))
     (t (find-least-dominating-terms (car xts)))))))

(defun try-collect-prod-diff (form &aux bindings results)
 (dolist (ldt (find-least-dominating-terms form) results)
  (setq bindings
  (match '(* (+ (? v no-unknown?)
          (? u has-unknown?))
       (- (? u) (? v)))
         ldt))
  (unless (eq bindings :fail)
   (push (cons `(collect-product-sum ,ldt)
         (simplify
    (subst (substitute-in
      `(- (sqr (? u)) (sqr (? v)))
      bindings)
           ldt form)))
   results))))

;;; attraction rule for logs
;;; (log u w) + (log v w) => (log (* u v) w)
;;; where u, v contain x and w doesn't.

(defun try-attract-log-sum (form &aux results bindings)
 (dolist (ldt (find-least-dominating-terms form)
        results)
  (setq bindings
  (match '(+ (log (? u has-unknown?)
      (? w no-unknown?))
       (log (? v has-unknown?)
      (? w)))
         ldt))
  (unless (eq bindings :fail)
   (push (cons `(attract-log-sum ,ldt)
         (simplify
    (subst (substitute-in
      `(log (* (? u) (? v)) (? w))
      bindings)
           ldt form)))
   results))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun try-canonicalization (form &aux result)
  (setq result (simplify form))
  (unless (equal result form)
    `(,(cons `(canonicalization ,form)
       result))))

(defun try-flip (form &aux bindings)
  (setq bindings
  (match `(= (? the-lhs no-unknown?)
       (? the-rhs has-unknown?))
         form))
  (unless (eq bindings :fail)
    `(,(cons `(swap-lhs-rhs ,form)
       (simplify
        (substitute-in `(= (? the-rhs) (? the-lhs))
           bindings))))))