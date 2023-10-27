;; -*- mode: lisp; -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; pattern matcher for algebra subsystem
;; last edited 1/29/93, by kdf

;; extracted from building problem solvers

;;; this version is inspired by one of g.j. sussman's scheme matchers.
;;; we eschew continuation-passing, for clarity.

;;; there are two kinds of variables.
;;; element variables match a single element of a list.
;;; segment variables match a (perhaps empty) piece of a list.
;;; element variables are (? <var name> <optional restriction>)
;;;  where <var name> is a symbol, and the restriction is a one-place
;;;  procedure which returns non-nil if the binding satisfies it.
;;; segment variables are like element variables, but start with ??.

(in-package :common-lisp-user)

;;; the entry point is match, which takes a pattern, an expression,
;;; and an alist of bindings.

(defun match (pat dat &optional (dict nil))
  (cond ((eq dict :fail) :fail) ;; propagate lossage
	((eq pat dat) dict)
	((element-var? pat)
	 (match-element-var pat dat dict))
	((not (consp pat))
	 (if (equal? pat dat) dict :fail))
	((segment-var? (car pat))
	 (match-segment-var pat dat dict))
	((not (consp dat)) :fail)
	(t (match (cdr pat) (cdr dat)
		  (match (car pat) (car dat) dict)))))

(defun match-element-var (pat dat dict &aux entry pred)
  (setq entry (lookup-var pat dict))
  (cond (entry 
	 (if (equal? (cadr entry) dat) dict :fail))
	(t (setq pred (var-restriction pat))
	   (cond ((or (not pred)
		      (funcall pred dat))
		  (bind-element-var (var-name pat) dat dict))
		 (t :fail)))))

(defvar *tol* 1.0e-6)

(defun equal? (a b)
  (cond ((and (floatp a) (floatp b)) (< (abs (- a b)) *tol*))
	(t (equal a b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; finding matches for segment variables
;; this is non-deterministic, hence requires iteration.

(defun match-segment-var (pat dat dict &aux entry rest)
  (setq entry (lookup-var (car pat) dict))
  (cond (entry ;; check for match
         (setq rest 
	       (check-segment dat (segment-beg entry)
			      (segment-end entry)))
	 (if (eq rest :fail) :fail
	     (match (cdr pat) rest dict)))
	(t ;; search for alternate segment bindings
	 (try-segment-bindings (car pat) (cdr pat) dat dict))))

(defun check-segment (dat beg end)
  (cond ((eq beg end) dat)
	((not (consp dat)) :fail)
	((equal? (car dat) (car beg))
	 (check-segment (cdr dat) (cdr beg) end))
	(t :fail)))

(defun try-segment-bindings (var pat dat dict &aux name pred beg)
  (setq name (var-name var)
	pred (var-restriction var)
	beg dat)
  (do ((end dat (cdr end))
       (ndict nil))
      ((null end)
       (cond ((or (null pred)
		  (funcall pred (segment->list beg nil)))
	      (match pat nil ;; try the very end
		     (bind-segment-var name beg nil dict)))
	     (t :fail)))
    (when (or (null pred)
	      (funcall pred (segment->list beg end)))
      (setq ndict (match pat end 
			 (bind-segment-var name beg end dict)))
      (unless (eq ndict :fail)
	      (return-from try-segment-bindings ndict)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defining variables

(defun pattern-variable? (x) (or (element-var? x) (segment-var? x)))
(defun element-var? (x) (and (consp x) (eq (car x) '?)))
(defun segment-var? (x) (and (consp x) (eq (car x) '??)))
(defun var-name (x) (cadr x))
(defun var-restriction (x) (caddr x))

;; dictionary entries take the form
;; (<name> <position> <value>), where <position> is nil if an element
;;   variable, and (<beg> . <end>) if a segment variable.

;; accessing entries
(defun lookup-var (var dict) (assoc (var-name var) dict))

(defun var-value (var dict &aux entry)
  (setq entry (lookup-var var dict))
  (unless entry (error "not bound variable: ~a, ~a." var dict))
  (cond ((= (length entry) 2) (cadr entry)) ;; element variable
	(t (segment->list (cadr entry) (caddr entry)))))

(defun segment-beg (entry) (cadr entry))
(defun segment-end (entry) (caddr entry))

(defun segment->list (start end)
  (do ((point start (cdr point))
       (l nil))
      ((eq point end) (nreverse l))
    (push (car point) l)))

;; updating dictionaries
(defun bind-element-var (name dat dict)
  (cons (list name dat) dict))
(defun bind-segment-var (name beg end dict)
  (cons (list name beg end) dict))

;; performing substitutions
(defun substitute-in (exp dict)
  (cond ((null exp) nil)
	((element-var? exp) (var-value exp dict))
	((consp exp)
	 (cond ((segment-var? (car exp))
		(append (var-value (car exp) dict)
			(substitute-in (cdr exp) dict)))
	       ((eq (car exp) :eval)
		(eval (substitute-in (cadr exp) dict)))
	       ((and (consp (car exp)) (eq (caar exp) :splice))
		(append (substitute-in (cadar exp) dict)
			(substitute-in (cdr exp) dict)))
	       (t (cons (substitute-in (car exp) dict)
			(substitute-in (cdr exp) dict)))))
	(t exp)))

