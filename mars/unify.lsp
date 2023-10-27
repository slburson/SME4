:common-lisp-user;; -*- mode: lisp; -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; variables and unification
;; last edited 1/29/93, by kdf

;;; copyright (c) 1986-1992, kenneth d. forbus, northwestern university,
;;; and johan de kleer, the xerox corporation.
;;; all rights reserved.

(in-package :common-lisp-user)

(defun variable? (x)
  (and (symbolp x)	;a symbol whose first character is "?"
       (char= #\? (elt (symbol-name x) 0))))

(defun unify (a b &optional (bindings nil))
   (cond ((equal a b) bindings)
	 ((variable? a) (unify-variable a b bindings))
	 ((variable? b) (unify-variable b a bindings))
	 ((or (not (listp a)) (not (listp b))) :fail)
	 ((not (eq :fail (setq bindings
			       (unify (car a) (car b) bindings))))
	  (unify (cdr a) (cdr b) bindings))
	 (t :fail)))

(defun unify-variable (var exp bindings &aux binding)
  ;; must distinguish no value from value of nil
  (setq binding (assoc var bindings))
  (cond (binding (unify (cdr binding) exp bindings))
	;; if safe, bind <var> to <exp>
	((free-in? var exp bindings) (cons (cons var exp) bindings))
	(t :fail)))

(defun free-in? (var exp bindings)
  ;; returns nil if <var> occurs in <exp>, assuming <bindings>.
  (cond ((null exp) t)
	((equal var exp) nil)
	((variable? exp)
	 (let ((val (assoc exp bindings)))
	   (if val 
	       (free-in? var (cdr val) bindings)
	     t)))
	((not (listp exp)) t)
	((free-in? var (car exp) bindings)
	 (free-in? var (cdr exp) bindings))))
