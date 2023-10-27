;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: no-bits.lsp
;;;;    System: SME
;;;;   Version: 1.0
;;;;    Author: mostek
;;;;   Created: April 22, 1999 08:57:51
;;;;   Purpose: Set operations to support structural consistency
;;;; ---------------------------------------------------------------------------
;;;;  modified: Friday, December 4, 2009 at 18:57:15 by usher
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;;; Sets are used for descendants and nogoods in SME.
;;;; We've used both bit vectors and ordered lists for sets at different times.
;;;; This version uses ordered lists, since all SME datastructures have an integer id.

(defun make-empty-set () nil)
(defun copy-set (set) (copy-list set))

(defun set-add (object lst)
  "Add an IDed object to an orderd list.  Destructive function."
  ;; add the object in sorted order based on its ID
  ;; object is of type match-hypothesis
  (cond ((null lst) (list object))
        ((> (id object) (id (first lst)))
         (cons object lst))
        (t
         (do ((head lst)
              (done? nil))
             (done? lst)
           (cond ((null (rest head))
                  (setq done? t)
                  (setf (rest head) (list object)))
                 ((eq object (first head))
                  (setq done? t))
                 ((> (id object) (id (cadr head)))
                  (setq done? t)
                  (setf (rest head) (cons object (rest head))))
                 (t 
                  (setq head (rest head))))))))

(defun set-member? (object lst)
  (cond ((null lst) nil)
        ((> (id object) (id (first lst))) ;; Not found
         nil)
        ((= (id object) (id (first lst))) t)
        (t (set-member? object (cdr lst)))))

(defun set-delete (object lst)
  "Destructively removes an object from an ordered list"
  ;; not quite the same as the one with bits - not done in place
  ;;
  ;; object is of type match-hypothesis
  (cond ((eq object (first lst))
         (rest lst))
        (t
         (do ((head lst)
              (done? nil))
             (done? lst)
           (cond ((null (rest head))
                  (setq done? t))
                 ((eq object (cadr head))
                  (setq done? t)
                  (setf (rest head) (cddr head)))
                 ((> (id object) (id (cadr head)))
                  (setq done? t))
                 (t 
                  (setq head (rest head))))))))

(defun set-size (lst) (length lst)) ;; Assumes no duplicates

;;;;----------------------------------------------------------- -
;;;; Membership functions.
;;;;----------------------------------------------------------- -

(defun set-common-member? (list1 list2)
   "Do these two ordered list contain a common member?"
   (do ((lst1 list1)
        (lst2 list2)
        (result nil))
       ((or result (null lst2) (null lst1)) result)
      (cond ((eq (first lst1) (first lst2))
             (setq result lst1))
            ((> (id (first lst1)) (id (first lst2)))
             (setq lst1 (rest lst1)))
            (t
             (setq lst2 (rest lst2))))))



(defun set-first-common-member (lst1 lst2)
  "Return the first common member of two ordered lists"
   (first (set-common-member? lst1 lst2)))

   
(defun set-union (list1 list2)
   "Return the union of the two ordered lists."
   ;; not quite the same as the one with bits - not done in place
   (do ((lst1 list1)
        (lst2 list2)
        (done? nil)
        (result nil))
       (done? (nreverse result))
      (cond ((null lst1)
             (dolist (elem lst2)
                (push elem result))
             (setq done? t))
            ((null lst2)
             (dolist (elem lst1)
                (push elem result))
             (setq done? t))
            ((eq (first lst1) (first lst2))
             (push (pop lst1) result)
             (setq lst2 (rest lst2)))
            ((> (id (first lst1)) (id (first lst2)))
             (push (pop lst1) result))
            (t
             (push (pop lst2) result)))))
 
(defun set-union! (list1 list2)
   "Return the destructive union of the two ordered lists."
   ;; only destructively updates list1
   ;; not quite the same as the one with bits - not done in place
   (cond ((null list2)
          list1)
         ((null list1)
          (copy-list list2))
         (t
          (let ((head list1)
                (new-items list2)
                (front-items nil)
                (result list1))
             ;; first find the items in list2 that are before list1
             (do ((done? nil))
                 (done?)
                (cond ((null new-items) (setq done? t))
                      ((eq (first new-items) (first head))
                       (setq new-items (rest new-items)))
                      ((> (id (first new-items)) (id (first head)))
                       (push (pop new-items) front-items))
                      (t (setq done? t))))
             ;; now work through the common parts of the lists
             (do ((done? nil))
                 (done?)
                (cond ((null new-items) (setq done? t))
                      ((null (rest head))
                       (setf (rest head) (copy-list new-items))
                       (setq done? t))
                      ((eq (cadr head) (first new-items))
                       (setq new-items (rest new-items)))
                      ((> (id (cadr head)) (id (first new-items)))
                       (setq head (rest head)))
                      (t
                       (setf (rest head) (cons (first new-items) (rest head)))
                       (setq head (rest head))
                       (setq new-items (rest new-items)))))
             ;; return the result
             (if front-items
                (nconc (nreverse front-items) result)
                result)))))

(defun set-intersection! (list1 list2)
   "Return the destructive intersection of the two ordered lists."
   ;; like nintersection but must maintain order
   (if (null list1) 
      nil
      (do ((head list1)
           (other-items list2)
           (result nil)
           (done? nil))
          (done? result)
         (cond ((null other-items)
                (setf (rest head) nil)
                (setq done? t))
               ((null result)
                (cond ((null head)
                       (setq done? t))
                      ((eq (first head) (first other-items))
                       (setq result head)
                       (setq other-items (rest other-items)))
                      ((> (id (first head)) (id (first other-items)))
                       (setq head (rest head)))
                      (t
                       (setq other-items (rest other-items)))))
               ((null (rest head))
                (setq done? t))
               ((eq (cadr head) (first other-items))
                (setq head (rest head))
                (setq other-items (rest other-items)))
               ((> (id (cadr head)) (id (first other-items)))
                (setf (rest head) (cddr head)))
               (t
                (setq other-items (rest other-items)))))))

          

;;;; ---------------------------------------------------------------------------
;;; END OF CODE
