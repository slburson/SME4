;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: entity.lsp
;;;;    System: SME
;;;;   Version: v3r09
;;;;    Author: Ken Forbus & Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: 1993
;;;;  Modified: Thursday, April 26, 2007 at 02:03:56 by halstead
;;;;   Purpose: Definition of SME entities.
;;;; --------------------------------------------------------------------------
(in-package :sme)

;;; The entity datastructure represents an object, conceptual or otherwise,
;;; in a description.  Entities are local to dgroups.  Any recognition of
;;; identity across two descriptions is the responsibility of the system which
;;; uses SME. 

(defclass item (se-data) ;; include some structural evaluation fields
  ()
  (:documentation "Top-level class encompassing both Expressions and Entities."))

(defclass entity (item)
  ((user-form
    :documentation "The name of the entity"
    :initarg :user-form :initform nil :accessor user-form :accessor name)
   (parents
    :documentation "The expressions this entity appears in."
    :initarg :parents  :initform nil  :accessor parents)
   (description
    :documentation "The description this entity is part of."
    :initarg :description  :accessor description)
   (id
    :documentation "Unique identifier of this entity"
    :accessor id  :initarg :id  :initform -1)
   (timestamp
    :documentation "Integer timestamp relative to dgroup clock"
    :initarg :timestamp :accessor timestamp))
  (:documentation
   "An entity is a single object referenced as an argument
    of expressions.  New entities can be created using DEFINE-ENTITY, 
   although they are typically defined as a side-effect of creating 
   descriptions."))

(defclass attribute-value (entity)
    ()
  (:documentation
   "An entity that is also a value.
    Examples include numbers as well as HOT in an expression
    (dvalue (temperature-of coffee) hot)."))

(defgeneric entity? (object)
  (:documentation "Predicate -- Is this object an entity?"))

(defmethod entity? ((e t)) nil)
(defmethod entity? ((e entity)) t)

(defgeneric attribute-value? (object)
  (:documentation "Predicate -- Is this object an attribute value entity?"))

(defmethod attribute-value? ((e t)) nil)
(defmethod attribute-value? ((e attribute-value)) t)

(defmethod print-object ((entity entity) stream)
  (format stream "<Entity ~A>" (user-form entity)))

(defmethod arity ((entity entity)) 0)

;; To keep this compatible with the graphing routines,
;;   we need to note that entities have no children.
(defmethod children ((entity entity))
  nil)


;;; Public functions-- creation of instances
(defparameter *sme-entity-default-class* 'entity)
(defparameter *sme-entity-attribute-default-class* 'attribute-value)

(defgeneric define-entity (name container)
  (:documentation
   "Define a single entity within the given container (at present,
    the only container supported is a dgroup -- later on we
    may support vocabularies as well).  The name should be a 
    symbol or string representing the entity name.  In the future,
    this method may be expanded to include class or instance 
    relations between entities."))

(defmethod define-entity (name (description description))
  "Create a new entity instance with the given name in the
   given description."
  (let* ((isaval 
          (unless *unique-attribute-values?*
            (or *_building-attribute-value*
                (is-attribute-value? name (vocabulary description)))))
         (new-entity 
          (unless isaval (fetch-entity name description))))
    (unless new-entity
      (setq new-entity
            (make-instance 
                (if isaval *sme-entity-attribute-default-class*
                  *sme-entity-default-class*)
              :user-form name
              :id (draw-entity-id description)
              :timestamp (advance-dgroup-timeclock description)
              :description description))
      (push new-entity (entities description)))
    new-entity))
    
(defmacro defentity (name &rest arglist)
  (declare (ignore name arglist))
  "defentity is a holder from the previous version of SME.  In that version 
   of SME, it was important that all entities be defined via DefEntity before
   they were used in a dgroup.  Since that restriction has been removed from
   the current version of SME, it now evaluates to a no-op.
   The new SME can create entity definitions directly from the description
   definition.  Note that the new SME can still read old SME files."
  nil)

;;; Define null load procedure as well.
(defun do-defentity (argument-list)
   (declare (ignore argument-list))
   nil)

;;; The order of an entity is always zero!
(defmethod order ((entity entity)) 0)

(defun entity# (num dgroup)
  (dolist (ent (entities dgroup))
    (if (= (id ent) num)
	(return-from entity# ent))))


(defmethod roots ((ent entity))
  ;; ***** This is expensive enough that it might be worth caching - KDF
  (if (null (parents ent)) (list ent) ;; self-rooted
      (let ((roots nil))
	(dolist (parent (parents ent) roots)
	  (dolist (new-root (roots parent)) ;; counting on low depth
     (pushnew new-root roots :test #'eq))))))

;;;; Entity deletion.
;;;; Extremely dangerous, as it must destroy all expressions that use it.

(defun delete-entity (entity)
   "Delete a single entity from its dgroup.  Use at your own risk."
   (delete-entity-from-dgroup entity)
   (dolist (parent (parents entity))
      (delete-expression parent)))

(defun delete-entity-from-dgroup (entity)
   (setf (entities (description entity))
         (delete entity (entities (description entity)))))


(defgeneric is-attribute-value? (entity-name vocab)
  (:documentation 
    "Uses the vocabulary to check if this entity is also an attribute value"))

(defmethod is-attribute-value? ((entity-name t) (vocab vocabulary))
  nil)

(defmethod is-attribute-value? ((entity-name number) (vocab t))
   t)
                   
(defmethod get-entity-attributes ((entity entity))
  (let ((result nil))
    (dolist (exp (parents entity) result)
      (if (attribute? exp)
        ;; N.B. Don't need a pushnew because SME uniquifies expressions
        (push (predicate exp) result)))))

(defmethod user-forms-equal ((item1 entity) (item2 entity))
  (eq item1 item2))

(defmethod user-forms-equal (item1 (item2 entity))
  (equal item1 (user-form item2)))

(defmethod user-forms-equal (item1 (item2 attribute-value))
  (when *unique-attribute-values?* 
    (equal item1 (user-form item2))))

