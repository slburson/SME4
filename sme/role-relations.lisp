;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: role-relations.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: mostek
;;;;   Created: September 15, 1999 10:05:23
;;;;   Purpose: Used with Crisis Management displays
;;;; ---------------------------------------------------------------------------
;;;;  Modified: Tuesday, March 21, 2000 at 14:18:57 by mostek
;;;; ---------------------------------------------------------------------------

(in-package :sme)

(defun find-mapped-role-relations (entity-name mapping &optional (vocab *vocabulary*))
   "Returns a list of role relation expresssions that are the mappings or 
    candidate inferences of the given entity in the given mapping"
  ;; entity-name is the frame token
  ;; find all the role-relation expressions that the entity-name have in the base description
  ;; See if there are correspondences for the values of these expressions in the mapping,
  ;; if not, make one
  ;; return a list of lisp expressions that would be role relation expression in the target
  (declare (ignore vocab))
  (mapcar #'(lambda(rr-expr)
              (find-or-make-corresponding-rr-expr rr-expr mapping))
    (remove-if-not #'role-relation-expr?
                   (parents (fetch-entity entity-name (base (sme mapping)))))))

(defun role-relation-expr? (expr)
  ;; non role-relations have nil in the role-relation-pos slot
  (role-relation-pos (predicate expr)))

(defun find-or-make-corresponding-rr-expr (rr-expr mapping)
  (cons (lisp-form (predicate rr-expr))
        (mapcar #'(lambda(arg-pair)
                    (find-or-make-correspondence (rest arg-pair) mapping))
          (arguments rr-expr))))     

(defun find-or-make-correspondence (entity mapping)
   ;; see if there is a MH in the mapping for this entity, otherwise skolemize one
   (let ((mh (find-mh-w-base entity mapping)))
      (if mh
         (lisp-form (target-item mh))
         (list :skolem (lisp-form entity)))))

(defun find-mh-w-base (entity mapping)
   (some #'(lambda(mh) (is-base-value? entity mh)) 
     (mhs mapping)))

(defun is-base-value? (entity mh)
   (and (entity? mh)
        (eq entity (base-item mh))
        mh))


;;;; ---------------------------------------------------------------------------
;;; END OF CODE
