;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: alignment.lsp
;;;;    System: SME v4
;;;;    Author: Ken Forbus
;;;;   Created: June 19, 2002 08:38:05
;;;;   Purpose: Deciding when items are alignable
;;;; ---------------------------------------------------------------------------
;;;;  modified: Tuesday, June 29, 2010 at 13:48:40 by aml758
;;;; ---------------------------------------------------------------------------


(in-package :sme)

;;;; This file provides two methods used heavily in MH construction, and
;;;; methods that provide hooks for implementing tiered identicality.

;;;; (locally-alignable? base-item target-item sme) determines whether
;;;; or not base-item and target-item can be aligned, independent of
;;;; any other context.  This is used when testing pairs of statements
;;;; from the base and target in the MH construction step.
;;;;
;;;; (arguments-alignable? base-item target-item sme) determines whether
;;;; or not base-item and target-item can be aligned, but under circumstances
;;;; when we know that it could be useful to align them, if it is sensible
;;;; to do so.  One such cases are when they are arguments of larger
;;;; potentially matching statements in the base and target, hence the name.
;;;; The other is when we know from some external source, such as a required
;;;; correspondence, that it would be useful to align two items.
;;;;
;;;; locally-alignable? uses strict identicality, whereas arguments-alignable? uses
;;;; tiered identicality.  How far an SME goes in methods it tried in the tiered 
;;;; identicality condition is determined by the subclass used.  
;;;;
;;;; Both procedures return two values, the first a t/nil signal indicating
;;;; whether or not the items can be aligned, and the second a rationale, a list
;;;; whose CAR is a keyword indicating the type of the rationale and whose CDR is
;;;; the rest of the information in the rationale.  
;;;;
;;;; The following method is also provided to implement minimal ascension:
;;;; (minimal-ascension-satisfied? base-pred target-pred vocabulary)
;;;; which returns a t/nil success flag plus a rationale.  The form of
;;;; the rationale is
;;;; (:minimal-ascension <superordinate> <depth> <reason> <symmetric?>)
;;;; where <superordinate> is the common superordinate (used in generating 
;;;; an abstraction for the match, <depth> is the minimum depth to it (used
;;;; in scaling the structural evaluation score for the MH), <reason> is a list 
;;;; of statements from the KR system that justify this, for debugging, and
;;;; <symmetric?> specifies whether the superordinate is a symmetric predicate.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; locally-alignable? 

(defgeneric locally-alignable? (item1 item2 sme)
  (:documentation 
   "True if the two items given can be aligned locally, i.e.
    without the support of another match hypothesis."))

(defmethod locally-alignable? ((entity entity)(expr expression)
                               (sme sme))
  (values nil nil))

(defmethod locally-alignable? ((expr expression)(entity entity)
                               (sme sme))
  nil nil)

(defmethod locally-alignable? ((base-entity entity) (target-entity entity)
                               (sme sme))
   (values nil nil))


(defmethod locally-alignable? ((base-expr expression) (target-expr expression)
                               (sme sme))
  (cond ((not (eq (functor base-expr) (functor target-expr)))
         (values nil '(:nonidentical-functors)))
        ((/= (arity base-expr) (arity target-expr))
          (values nil '(:arity-mismatch)))
    ((ubiquitous-predicate-in? (functor base-expr)
       (vocabulary (description base-expr)))
         ;; This must be last test, see arguments-alignable? below
         (values nil '(:ubiquitous-predicate)))
        (t
          (values t '(:simple-identicality)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; arguments-alignable?
;;;
;;; returns two values:
;;;   t/nil
;;;   rationale
;;; The rationale for minimal ascension is of the form 
;;;   (:minimal-ascension superordinate depth reason symmetric?)
;;;
 
(defgeneric arguments-alignable? (item1 item2 sme)
  (:documentation 
   "True if two items may be aligned, given some contextual reason to do so."))

(defmethod arguments-alignable? ((expr expression)(entity entity)(sme sme))
  (values nil nil))

(defmethod arguments-alignable? ((entity entity)(expr expression)(sme sme))
  (values nil nil))

(defmethod arguments-alignable? ((base-entity entity) (target-entity entity) 
                                 (sme sme))
  (values t 'default-all-entities-locally-alignable))

(defmethod arguments-alignable? ((base-expr expression)(target-expr expression)
                                 (sme sme))
  (multiple-value-bind (alignable? reason)
      (locally-alignable? base-expr target-expr sme)
    (cond (alignable? (values t reason))
          ((eq (car reason) ':ubiquitous-predicate)
           (values t '(:ubiquitous-alignable-as-arguments)))
          ((and (function? base-expr) (function? target-expr)
                (= (arity base-expr) (arity target-expr)))
           (values t '(:functions-alignable-as-arguments)))
          (t 
           (minimal-ascension-satisfied? 
            (functor base-expr) (functor target-expr) (vocabulary sme))))))

(defun rationale-supports-symmetry? (rationale)
  (and (listp rationale)
       (fifth rationale)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; minimal-ascension-satisfied?

;;; This is the stub method -- if no KR system attached, then we get nothing.
;;; If there is an attached KR system that supports minimal ascension, then it needs
;;; to define this method as per the note in the top of this file, specializing on
;;; the vocabulary, typically

(defmethod minimal-ascension-satisfied? ((base-functor t) (target-functor t) 
                                         (vocabulary t))
  (values nil nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
