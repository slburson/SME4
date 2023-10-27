;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: rehydration.lsp
;;;;    System: SME
;;;;    Author: Shawn Nicholson
;;;;   Created: October 9, 2001 14:53:28
;;;;   Purpose: Support for rehydrating SMEs
;;;; ---------------------------------------------------------------------------
;;;;  Modified: Tuesday, October 26, 2004 at 17:33:34 by forbus
;;;; ---------------------------------------------------------------------------

(in-package :sme)


;; In the context of rehydration of a dehydrated sme we need to alter how
;;  define-predicate works.
;; The current default behavior of define-predicate (in pred.lsp) is to call
;;  find-predicate to see if the predicate already exists and then to warn
;;  the user that they are redefining an already extant predicate.
;; However, find-predicate, if there is no predicate, automatically defines
;;  a predicate based on the current knowledge base (this point is important)
;;  and returns it. 
;; This means that find-predicate is always guaranteed to return a predicate,
;;  thus define-predicate will warn that you are redefining a predicate 100%
;;  of the time.
;; There also exists another, more insidious problem with the way this works.
;;  Find-predicate creates a new predicate (when none currently exists) based
;;  on knowledge it gets from the current knowledge source - this includes 
;;  such information as predicate-type, arity, etc.  
;; Under normal conditions this is appropriate behavior.  However, in the 
;;  case of rehydrating a dehydrated SME, you want to use the vocabulary
;;  provided in the rehydration.  This is why you use define-predicate.
;;  define-predicate calls find-predicate warns that it is redefining a 
;;  predicate and then creates the new predicate and replaces the one 
;;  that was defined by find-predicate.
;; The problem here is that what type of predicate is defined is specialized
;;  in the Find-Predicate method.  However, the type of predicate that 
;;  define-predicate creates is hardcoded.  
;;
;; rehydrate-predicate is a procedure nearly identical to define-predicate
;;  except that it doesn't call find-predicate (with find-predicate's 
;;  behavior of creating it when it doesn't find it), instead it simply calls
;;  extant-predicate? - a method specialized on the vocabulary to tell if
;;   a predicate is already defined.
;; The second difference is that where define-predicate creates the new predicate
;;  rehydrate-predicate calls a method that can be specialized on the vocabulary
;;  to create the new predicate.  
;;  

(defmacro rehydratePredicate (name arguments pred-type &rest other-info)
  "Define a predicate in the default vocabulary.  Arguments are 
   as described for the REHYDRATE-PREDICATE routine."
  `(rehydrate-predicate
    ',name *vocabulary* ,(keywordize pred-type) ',arguments ',other-info))


(defgeneric extant-predicate? (predicate-name vocab)
  (:documentation "specialize on the vocabulary type to find out if predicate-name
                    is in your vocabulary - don't do anything other than
                    return true or false if your predicate is not there."))

(defmethod extant-predicate? (predicate-name vocab)
  (gethash predicate-name (predicate-table vocab)))


(defgeneric create-new-predicate (vocabulary name arguments pred-type commutative?
                                             symmetric? set? doc-string notes pidgin
                                             role-relation-pos n-ary?)
  (:documentation "Specialize on your vocabulary, this is to create a new predicate
                    with the following parameters"))


(defmethod create-new-predicate (vocabulary name arguments pred-type commutative?
                                            symmetric? set? doc-string notes pidgin
                                            role-relation-pos n-ary?)
  (if (null role-relation-pos)
    (make-instance 'classic-predicate
    :name name :id (incf (id-counter vocabulary))
    :arguments arguments :numargs (length arguments)
    :type pred-type :commutative? commutative?
    :n-ary? n-ary?
    :symmetric? symmetric? :set? set?
    :doc-string doc-string :notes notes
    :pidgin-description-string pidgin)
  (make-instance 'neo-classic-predicate
    :name name :id (incf (id-counter vocabulary))
    :arguments arguments :numargs (length arguments)
    :type pred-type :commutative? commutative?
    :n-ary? n-ary?
    :symmetric? symmetric? :set? set?
    :doc-string doc-string :notes notes
    :pidgin-description-string pidgin
    :role-relation-pos role-relation-pos)))



(defun rehydrate-predicate (name vocabulary pred-type arguments keyword-args)
 
  "Defines Name as a predicate of type Pred-type in Vocabulary.
   Predicate-type is one of :ATTRIBUTE, :RELATION, :FUNCTION, :LOGICAL
   or :TAXONOMY.   Arguments are either entities or expressions.
   Keyword arguments include :COMMUTATIVE? (T or nil, depending on 
   whether the expression arguments may be permuted without changing
   an expression's meaning) and :N-ARY? (T if the last argument of the
   predicate definition may be repeated an arbitrary number of times).
   For documenting a predicate, use the keyword arguments 
   :DOCUMENTATION and :PIDGIN." 
  ;; Start by checking if in the ballpark
  (if (eq pred-type :predicate) (setq pred-type :relation))
  (unless (predicate-type-p pred-type)
    (error "Invalid predicate type in rehydratePredicate: ~A, ~A." 
      name pred-type))
  (when (extant-predicate? name vocabulary)
    (warn "Redefinition of ~A in ~A:~%  ~A,~% ~A"
      name vocabulary arguments keyword-args))
  ;; Then parse the argument list 
  (destructuring-bind (&key commutative? n-ary? symmetric? set? script? 
                            notes role-relation-pos &allow-other-keys) 
      keyword-args
    (declare (ignore script?))
    (when (or (and commutative? n-ary?) (eql pred-type ':set))
      (setq set? t))
    (when set?
      (setq commutative? t)
      (setq n-ary? t))
    ;; Calculate correct internal argument list
    (setq arguments (expand-predicate-arguments arguments))
    ;; Calculate documentation, if any.
    (multiple-value-bind (documentation pidgin)
        (generate-predicate-documentation arguments keyword-args)
      (let ((predicate (create-new-predicate
                        vocabulary
                        name 
                        arguments
                        pred-type
                        commutative?
                        symmetric?
                        set?
                        documentation
                        notes
                        pidgin
                        role-relation-pos
                        n-ary?)))
        (add-predicate predicate vocabulary) ;; add predicate to vocabulary
        predicate))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
