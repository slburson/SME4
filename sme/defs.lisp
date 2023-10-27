;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: defs.lsp
;;;;    System: SME
;;;;   Version: 1.0
;;;;    Author: Ron Ferguson, Ken Forbus
;;;;   Created: January 17, 1999 00:18:39
;;;;   Purpose: Global definitions for SME system
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2012-12-03 16:00:06 -0600 (Mon, 03 Dec 2012) $
;;;;  $LastChangedBy: aml758 $
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;; The following variables are used to set up defaults for the current
;;;  values of the structure-mapping-engine, the vocabulary, and
;;;  the description.  Please do not access these directly,   
;;;  use the in-sme, with-sme, with-vocabulary, and with-description 
;;;  macros instead.

(defvar *vocabulary* nil
  "The current vocabulary in use by the current SME.  Also, the default
   vocabulary used for any newly created SME.  This variable must be set
   before creating new SME objects.  It is initially NIL.") 

(defvar *sme* nil
  "The default SME object for a number of SME functions.  DEFINE-SME
   automatically sets this variable to the newly-created SME.")

;; These variables are used as sources for id counters
;; for their respective datastructures.
(defvar *sme-counter* 0    "Counter used to generate SME ID number.")
(defvar *dgroup-counter* 0 "Counter used to generate dgroup ID number.")

;; The next two constants affect the creation of descriptions
(defvar *unique-attribute-values?* t
  "When t, there can never be more than one of the same attribute-value in a dgroup.
   E.g. All instances of the number 1 will be treated as the same entity.
   This artificially constrains most matches, but it is on by default since it
   is the original behavior.")

(defvar *record-same-values?* nil
  "When t, then multiple instances of identical attribute values are recorded as
   such in the dgroup, via (sameValue <Entity foo1> <Entity foo2>).  Note that
   this can only happen when *unique-attribute-values* is set to off.")

(defun get-new-dgroup-id ()
   "Return the next dgroup ID number"
   (incf *dgroup-counter*))

(defun get-new-sme-id ()
   "Return the next SME ID number."
   (incf *sme-counter*))

(defvar *warn-on-kernel-plateaus?* nil
  "If true and a kernel plateau occurs, warn the user explicitly.")
(defvar *warn-on-greedy-inversion?* nil
  "If true and a greedy inversion occurs, warn the user explicitly.")

(defvar *use-less-greedy-greedy-merge-seeker?* t
  "If true, use a modified form of greedy merge allowing addition of 
   consistent kernels from other mappings.")

(defvar *use-legacy-greedy-merge?* nil
  "There was a small change to the greedy merge on 8/25/06 which can 
   change sme results, simply due to their order dependence (it changes
   the order in which equally good mappings may appear).  This rolls 
   back that change.")

;;;; More global parameters & debugging system

;;; This variable provides the defaults for each SME created.
(defvar *default-debugging-features* '())
(defvar *debugging-features* ;; central registry
  '((:incremental "Display information about incremental results")
    (:merge "Display information about the greedy cream operation.")))

;;; Class Definitions that need to be defined up front

;;; Mixin
(defclass documented-object ()
   ((name
     :documentation "The name of the object, as a symbol."
     :accessor name :initform nil :initarg :name :type symbol)
    (doc-string
     :documentation "One-line description of object."
     :accessor doc-string :initform "" :initarg :doc-string :type string)
    (notes
     :documentation "A set of notes, given as a (potentially) very long string"
     :accessor notes :initarg :notes :initform "" :type string)))

(defmethod user-form ((obj documented-object)) (name obj))

(defclass vocabulary (documented-object)
  ((predicate-table
    :documentation "The predicates in this vocabulary, hashed by pred name."
    :accessor predicate-table :initarg :predicate-table :initform (make-hash-table :test #'equal))
   ;; KDF 5/6/01 -- Had to go to equal hash table because of NAT predicates
   (predicate-id-table
    :documentation "The predicates in this vocabulary, hashed by pred id."
    :accessor predicate-id-table :initarg :predicate-id-table :initform (make-hash-table))
   (counter
    :documentation "ID counter for this vocabulary"
    :initform -1 :accessor id-counter :initarg :counter :type integer)
   (ubiquitous-predicates
    :initform nil :type list :accessor ubiquitous-predicates :initarg :ubiquitous-predicates
    :documentation "Ubiquitous predicates aren't sufficient for matching")
   (subordinates
    :documentation "Vocabularies this vocabulary can call on if they don't know
      about a predicate.  List of vocabulary objects."
    :accessor subordinates :initarg :subordinates :initform nil :type list)
   (minimal-ascension-depth
    :initform 0 :accessor minimal-ascension-depth :initarg :minimal-ascension-depth
    :documentation "Tells the number of levels to use for minimal ascension"))
  (:documentation 
   "A vocabulary describes the predicates which are available for use
    in descriptions.  to find a predicate in a vocabulary, use
    find-predicate.  vocabularies are typically created by loading
    vocabulary files using vocabulary-from-file.  vocabulary files
    typically contain lists of defpredicate statements."))


;;;; Order predicates for various datastructures

;;; The functions used by these predicates are defined
;;;  as methods later.
(defgeneric order (x)
  (:documentation "The order of the item in a tree structure."))
(defgeneric timestamp (x)
  (:documentation "The timestamp for an item."))
(defgeneric id (x)
  (:documentation "The number ID for an item."))
(defgeneric score (x)
  (:documentation "The score for an item--always a number."))
(defgeneric base-item (x)
  (:documentation "The base half of a pair of match hypotheses."))
(defgeneric target-item (x)
  (:documentation "The target half of a pair of match hypotheses."))

(defun order< (x y) (< (order x) (order y)))
(defun order> (x y) (> (order x) (order y)))
(defun timestamp< (x y) (< (timestamp x) (timestamp y)))
(defun timestamp> (x y) (> (timestamp x) (timestamp y)))
(defun id< (x y) (< (id x) (id y)))
(defun id> (x y) (> (id x) (id y)))
(defun score< (x y) (< (score x) (score y)))
(defun score> (x y) (> (score x) (score y)))
(defun mh< (x y) ;; lexigraphic ordering on id's
  (if (id< (base-item x)
           (base-item y))
    x
    (if (id< (base-item y)
             (base-item x))
      y
      (if (id< (target-item x)
               (target-item y))
        x
        y))))

;;; This order predicate is for making more readable displays

(defun display< (x y)
   (if (order< x y) t
      (if (order> x y) nil
         (lexical-lisp-form< x y))))

(defun lexical-lisp-form< (x y) ;; expensive, but useful for displays
   (string< (format nil "~A" (lisp-form x))
     (format nil "~A" (lisp-form y))))

(defclass se-data ()
    ((se-marker :type t :initform nil :accessor se-marker
      :documentation "Marker used in structural evaluation algorithms")
     (score :type float :initform 0.0 :accessor score
       :documentation "Structural evaluation score for this datastructure")))

;;; Timing procedures
;;; N.B. See testing\test-stand.lsp for Franz-specific microsecond timers,
;;;      since with the default millisecond timers in Franz, SME runs are often
;;;      zero on modern machines.

(defun concise-time-string ()
   "Return the time in the format HH:MM:SS."
   (multiple-value-bind (second minute hour)
       (get-decoded-time)
      (format nil "~A:~A:~A" hour minute second)))

;; Slightly tricky to avoid capture of values with nested timings
;; [Which will be thrown off by printing times, but just in case.]
(defmacro with-recorded-real-time (operation-name form)
   (let ((op-name-start-var (intern (format nil "~A-START" operation-name)
                              (find-package :cl-user)))
         (op-name-end-var (intern  (format nil "~A-END" operation-name)
                              (find-package :cl-user)))
         (op-name-value (intern  (format nil "~A-VALUE" operation-name)
                              (find-package :cl-user))))
         `(let ((,op-name-end-var 0)
                (,op-name-start-var 0)
                (,op-name-value nil))
            (format *sme-debug-stream* "~%~A started ~A."
              ,operation-name (concise-time-string))
             (setq ,op-name-start-var (get-internal-real-time))
             (setq ,op-name-value ,form)
             (setq ,op-name-end-var (get-internal-real-time))
             (format *sme-debug-stream* "~%~A ended ~A (~A seconds)."
               ,operation-name (concise-time-string)
               (/ (- ,op-name-end-var ,op-name-start-var)
                  (float internal-time-units-per-second)))
             (format *sme-debug-stream* "~%~A ~A Results" ,operation-name
               (if (listp ,op-name-value) (length ,op-name-value)
                  ,op-name-value))
             ,op-name-value)))

;;; Simple loader

(defun load-sme-data (file-name)
  (with-open-file (fin file-name :direction :input)
    (let ((marker (cons file-name nil)))
      (do ((form (read fin nil marker)
                 (read fin nil marker))
           (last-value nil))
          ((eq form marker) last-value)
        (when (listp form)
          (setq last-value
                (case (car form)
                  (sme::defdescription (do-defdescription (cdr form)))
                  (sme::defentity (do-defentity (cdr form)))
                  (sme::defpredicate (do-defpredicate (cdr form)))
                  (sme::defubiquitous-predicate
                      (do-defubiquitous-predicate (cdr form))))))))))


                       


