;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: utils.lsp
;;;;    System: SME
;;;;   Version: 1.0
;;;;    Author: Ron Ferguson, Ken Forbus
;;;;   Created: January 17, 1999 00:18:39
;;;;   Purpose: Various utility routines used by SME.
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2013-06-03 18:38:43 -0500 (Mon, 03 Jun 2013) $
;;;;  $LastChangedBy: aml758 $
;;;; ---------------------------------------------------------------------------
(in-package :sme)

;; Keywords are great ways to avoid package problems.   Alas, some of the old 
;;  dgroup files don't use keywords, and sometimes there are really obscure 
;;  package problems.  The code below minimizes package problems by basing its
;; decisions on the identity of symbol names, and completely ignoring packages.  
;;

(defun keywordize (symbol)
  (intern (symbol-name symbol) 'keyword))

(defun same-symbol-name? (sym1 sym2)
  ;; Package-independent comparison of symbol names
  (and (symbolp sym1) (symbolp sym2)
       (string= (symbol-name sym1) (symbol-name sym2))))

(defun keyword-value (keyword klist &key (default-value nil))
  (let ((val (member keyword klist :test 'same-symbol-name?)))
    (if val (cadr val) default-value)))

(defun keyword-true? (keyword klist &key (not-found? nil))
  (if (not (null (keyword-value keyword klist :default-value nil)))
      t not-found?))

(defun keyword-false? (keyword klist &key (not-found? t))
  (if (null (keyword-value keyword klist :default-value nil))
      t not-found?))

(defun strip-keyword-pair-from-list (keyword klist)
  ;; all these lists are short, so shouldn't matter
  (cond ((or (null klist)
	     (null (cdr klist))) klist)
	((same-symbol-name? (car klist) keyword)
	 (cddr klist))
	;; Assumes no sequences of <keyword> <keyword>..
	(t (cons (car klist)
		 (cons (cadr klist)
		       (strip-keyword-pair-from-list keyword (cddr klist)))))))

;;; (show-var var-name)  -> Show value of a variable [macro]

(defmacro show-var (var-name)
  `(format t "~%Variable ~A: ~A" ',var-name ,var-name))

(defun always-false (x)
  "Null function, used as placeholder for other predicate functions."
  (declare (ignore x))
  nil)

(defun intersect? (lst1 lst2)
   (do ((elems lst1 (rest elems))
        (result nil (member (first elems) lst2 :test #'eq)))
       ((or result (null elems)) result)))

(defun min-without-nils (&rest vals)
  (apply #'min (remove nil vals)))

;;; Dumper utilities
;;; ----------------------------------------------------------------------------
(defun dump-sme-file-header (type-of-dump-string stream)
  (format stream ";; -*- LISP -*-")
  (format stream "~%;; ~A dumped from SME ~A"
    type-of-dump-string *sme-version*)
  (format stream "~%;; Created by ~A, using ~A, a ~A at ~A running ~A" ;[~A]"
    (sme-user-name) (machine-instance) (machine-type) 
    (short-site-name) (software-type)) ;(software-version)))
  (format stream "~%;; ~A" (get-time-string))
  (format stream "~%;; NOTE: This updated dehdration method maintains the order of expressions in dgroups."))
  
(defun sme-user-name ()
  "Return the user name for this particular machine. 
   Implementation-dependent."
  #+(or acl5 acl6) (sys:user-name)
  #-(or acl5 acl6) "Unknown"
  )

(defparameter *keyword-value-indent* "   ")

(defun output-keyword-value-pair-to-line (keyword value stream)
   (format stream "~%~A ~A ~A" *keyword-value-indent*
     keyword value))

(defun get-time-string ()
  "Returns a string describing the current date and time."
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (get-decoded-time)
    (declare (ignore day daylight-p zone))
    (format nil "~A/~A/~A ~2,'0D:~2,'0D:~2,'0D"
      month date year hour minute second)))

;;; Grouping function
;;; ----------------------------------------------------------------------------
(defun common-pairs (list1 list2 &key (key #'identity) (test #'eql))
   "For the given two lists, return the pairs of items that
    are identical as a list of cons.  A list of leftover items from both
    lists are returned as a second value.  No guaranteed order."
   (let* ((leftovers (set-exclusive-or list1 list2 :key key :test test))
          (common1 (set-difference list1 leftovers :key key :test test)))
      (let ((result nil)
            (item2 nil))
         (dolist (item1 common1)
            (setf item2 (find (funcall key item1) list2 :test test :key key))
            (push (cons item1 item2) result))
         (values result leftovers))))

;;; Sorting functions
;;; ----------------------------------------------------------------------------
(defun sort-by-score (object-list)
   "Sort the given set of object by their score, largest score first."
   (sort (copy-list object-list) #'> :key #'score))
