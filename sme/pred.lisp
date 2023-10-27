;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: pred.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;    Author: Ken Forbus & Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: 1994
;;;;   Purpose: Predicate definition
;;;; --------------------------------------------------------------------------
(in-package :sme)

;;; Define the types of predicates.

(defvar *sme-predicate-types*
  '(:relation :attribute :function :logical :taxonomy)
  "Available predicate types")

(defun predicate-type-p (x) (member x *sme-predicate-types* :test #'eq))

;;; Base class for predicates.
;;;  Different KR systems could have other conventions
;;;  for argument structure, etc.

(defclass predicate (se-data documented-object)
  ((id
    :documentation "Integer id for ordering purposes"
    :initarg :id :initform 0 :type integer :reader id)
   (parents
    :documentation
    "List of ids of superordinate parents (determined by the vocabulary). Used in minimal ascension"
    :initarg :parents :initform nil :type list :accessor parents)
   (vocabulary
    :documentation "Language this predicate defined in"
    :initarg :vocabulary :initform nil :accessor vocabulary
    :initarg :vocabulary ))
  (:documentation
   "The predicate description class.  The slots of this class describe
     the argument names and types for the given predicate, as well as
     certain other features of the predicate, such as the whether the
     arguments are commutative.  Predicates can be defined using
     either the DefPredicate macro or the define-predicate function.
     This class is subclassed by Classic-Predicate, which is the
     default for new predicates."))

;;; Functor = predicate or function for a statement or a term.
(defgeneric functor (object)
  (:documentation
   "For predicates, this method returns the name of the predicate.
    For expressions, it returns the predicate itself."))

(defmethod functor ((pred predicate)) (name pred))

;;;; Default SME predicates
(defclass classic-predicate (predicate)
  ((pred-type
    :documentation 
    "The type of predicate:  one of :function, :relation, or :attribute, 
     or one of the other predicate types listed in *sme-predicate-types*"
    :initarg :type :initform nil :accessor pred-type)
   (numargs
    :documentation "length of argument list"
    :initarg :numargs :accessor numargs)
   (arguments
    :documentation
    "Alist of predicates arguments, with multiple types interpreted disjunctively"
    ;; The arguments are given as association lists, where the car is 
    ;;  the case relation (unique for that predicate argument) and the
    ;;  remaining arguments are the types of the argument.
    :initarg :arguments  :initform nil :accessor arguments)
   (commutative?
    :documentation "Is predicate commutative?"
    :initarg :commutative? :reader commutative? :initform nil)
   (n-ary?
    :documentation "Is predicate n-ary?"
    :initarg :n-ary? :reader n-ary? :initform nil)
   (symmetric?
    :documentation "Is a predicate for a symmetric relation?"
    :initarg :symmetric? :accessor symmetric? :initform nil)
   (script?
    :documentation "Is predicate script-matching?"
    :initarg :script? :initform nil :reader script?)
   (set?
    :documentation "Is predicate a set?"
    :initarg :set? :initform nil :reader set?)
   (group?
    :documentation "Is this a group predicate?"
    :initarg :group? :initform nil :accessor group?)
   (pidgin-description-string
    :documentation 
    "An english sentence with slot names for the arguments of an expression.
       This is used to generate the pidgin description of the dgroup 
       expressions, which is used to check the semantics of a dgroup."
    :accessor pidgen-description-string ;; misspelled older form
    :accessor pidgin-description-string
    :initform "" 
    :initarg :pidgen-description-string ;; misspelled older form
    :initarg :pidgin-description-string))
  (:documentation 
   "This is the stand predicate class used in SME.  It includes slots
    for documenting the predicate and describing the argument types."))

(defclass neo-classic-predicate (classic-predicate)
  ((role-relation-pos
    :documentation
    "Indicates if this predicate is a role-relation (non-nil) and if so, what
     argument pos the reified event is in (index starts at 1)." 
    :initarg :role-relation-pos :initform nil :accessor role-relation-pos)
   (frame-attribute
    :documentation
    "A flag to denote that the entity described by this predicate is a reified event." 
    :initarg :frame-attribute :initform nil :accessor frame-attribute)
   )
  (:documentation 
   "This adds information related to role relations for use with the HPKB-SME"))

(defmethod role-relation-pos ((random t)) nil)

(defmethod predicate? ((thing t))
  "Is the given object an SME predicate?"
  ;; Default: No
  nil)

(defmethod predicate? ((thing predicate)) t)

(defmacro functor? (possible-functor) `(predicate? ,possible-functor))

(defmethod function? ((pred t)) nil)
(defmethod function? ((pred predicate))
  (eq (pred-type pred) ':function))

(defmethod logical? ((pred t)) nil)
(defmethod logical? ((pred predicate))
  (eq (pred-type pred) ':logical))

(defmethod is-relation? ((pred t)) nil)
(defmethod is-relation? ((pred predicate))
  (eq (pred-type pred) ':relation))

(defmethod attribute? ((pred t)) nil)
(defmethod attribute? ((pred predicate))
   (eq (pred-type pred) ':attribute))

(defmethod print-object ((pred predicate) stream)
   "Print predicate as predicate type and the functor name."
   (format stream "<~A ~A>" (pred-type pred) (name pred)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Defining new predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defpredicate (name arguments pred-type &rest other-info)
  "Define a predicate in the default vocabulary.  Arguments are 
   as described for the define-predicate routine."
  `(define-predicate
    ',name *vocabulary* ,(keywordize pred-type) ',arguments ',other-info))

(defun do-defpredicate (form)
   (define-predicate (car form) ;; name
     *vocabulary*
     (keywordize (third form)) ;; predicate type
     (cadr form) ;; arguments
     (cdddr form))) ;; other stuff

(defun define-predicate (name vocabulary pred-type arguments keyword-args)
 
  "Defines Name as a predicate of type Pred-type in Vocabulary.
   Predicate-type is one of :attribute, :relation, :function, :logical
   or :taxonomy.   Arguments are either entities or expressions.
   Keyword arguments include :commutative? (t or nil, depending on 
   whether the expression arguments may be permuted without changing
   an expression's meaning) and :n-ary? (t if the last argument of the
   predicate definition may be repeated an arbitrary number of times).
   For documenting a predicate, use the keyword arguments 
   :documentation and :pidgin." 
  ;; Start by checking if in the ballpark
  (unless (predicate-type-p pred-type)
    (cond ((member :runtime-system *features*)
           (warn "Invalid predicate type in defPredicate: ~A, ~A." name pred-type)
           (return-from define-predicate (values nil)))
          (t (error "Invalid predicate type in defPredicate: ~A, ~A." name pred-type))))
  (when (find-predicate name vocabulary :create? nil)
	(error "Redefinition of ~A in ~A:~%  ~A,~% ~A"
	      name vocabulary arguments keyword-args))
   ;; Then parse the argument list 
   (destructuring-bind (&key commutative? n-ary? symmetric? set? script? 
                         notes &allow-other-keys) keyword-args
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
        (let ((predicate 
                (make-instance 'classic-predicate
                  :name name :id (incf (id-counter vocabulary))
                  :arguments arguments :numargs (length arguments)
                  :type pred-type :commutative? commutative?
                  :script? script? :n-ary? n-ary?
                  :symmetric? symmetric? :set? set?
                  :doc-string documentation :notes notes
                  :pidgin-description-string pidgin)))
           (add-predicate predicate vocabulary) ;; add predicate to vocabulary
           predicate))))                ;; return predicate

;;;; Helpers for defining predicates

;;; Expand the argument list, doing the following:
;;;    Expanding single symbols to a case-relation/type alist.
;;;
;;;    Checking for duplicate case relations.
;;;    Checking that the types are available and not redundant.
;;;
(defun expand-predicate-arguments (arguments &aux (arg-pos 0))
  "Expand the argument description given in predicate defn"
   (mapcar #'(lambda (arg-entry)
               (incf arg-pos)
               (if (symbolp arg-entry)
                  (list (make-predicate-arg-var arg-pos) arg-entry)
                  arg-entry))
     arguments))

(defun make-predicate-arg-var (counter)
   "Helper function for expand-predicate-arguments"
   ;; For common arguments, return value, otherwise create special.
   (case counter
     (0 'cl-user::arg-0) (1 'cl-user::arg-1) (2 'cl-user::arg-2) 
     (3 'cl-user::arg-3) (4 'cl-user::arg-4)
     (otherwise
       (intern (format nil "ARG-~D" counter) :cl-user))))
   
(defun generate-predicate-documentation (arguments keyword-args)
  "Look for two keywords--:documentation and :pidgin.  Return
   their two values.  If no documentation, punt."
   (declare (ignore arguments))
   (destructuring-bind (&key documentation pidgin &allow-other-keys) keyword-args
     (values (or documentation "") (or pidgin ""))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; These methods are how the SME gets knowledge-specific information from
;;;  the knowledge source.
;;;
;;; variable?
;;; pred-arity
;;; relation?
;;; logical-connective?
;;; pred-function?
;;; commutative-relation?
;;; event-predicate?
;;; role-relation?
;;; collection?
;;; actor-slot?
;;; reverse-actor-slot?

(defun undefined-kb-source-error (meth)
  (error "You must define a definition for the method:
      ~A specialized on your vocabulary object type" meth))


(defgeneric variable? (pred-name vocab)
  (:documentation "Returns true if pred-name is a variable"))

(defmethod variable? (pred-name vocab)
  (declare (ignore pred-name vocab))
  (undefined-kb-source-error "variable?"))


(defgeneric pred-arity (pred vocab)
  (:documentation "Returns the arity of the predicate"))

(defmethod pred-arity (pred vocab)
  (declare (ignore pred vocab))
  (undefined-kb-source-error "pred-arity"))


(defgeneric relation? (pred vocab)
  (:documentation "Returns true if this is a relation"))

(defmethod relation? (pred vocab)
  (declare (ignore pred vocab))
  (undefined-kb-source-error "relation?"))


(defgeneric logical-connective? (pred vocab)
  (:documentation "Returns true if this is a logical-connective"))

(defmethod logical-connective? (pred vocab)
  (declare (ignore pred vocab))
  (undefined-kb-source-error "logical-connective?"))


(defgeneric pred-function? (pred vocab)
  (:documentation "Returns true if this is a function"))

(defmethod pred-function? (pred vocab)
  (declare (ignore pred vocab))
  (undefined-kb-source-error "pred-function?"))

(defgeneric commutative-relation? (pred vocab)
  (:documentation "Returns true if this is a commutative-relation"))

(defmethod commutative-relation? (pred vocab)
  (declare (ignore pred vocab))
  (undefined-kb-source-error "commutative-relation?"))

(defmethod event-predicate? ((thing t) (vocab vocabulary)) nil)

(defgeneric role-relation? (pred-name vocab)
  (:documentation "Returns true if this predicate is a role-relation"))       

(defmethod role-relation? (pred-name vocab)
  (declare (ignore pred-name vocab))
  (undefined-kb-source-error "role-relation?"))

(defmethod predicate-is-role-relation? ((pred t)) nil)

(defmethod predicate-is-role-relation? ((pred predicate))
  (numberp (role-relation-pos pred)))

(defgeneric collection? (pred-name vocab)
  (:documentation "Returns true if pred-name represents a collection"))

(defmethod collection? (pred-name vocab)
  (declare (ignore pred-name vocab))
  (undefined-kb-source-error "collection?"))

(defgeneric actor-slot? (pred-name vocab)
  (:documentation "Returns true if pred-name represents a actor slot"))

(defmethod actor-slot? (pred-name vocab)
  (declare (ignore pred-name vocab))
  (undefined-kb-source-error "actor-slot?"))

(defgeneric reverse-actor-slot? (pred-name vocab)
  (:documentation "Returns true if pred-name represents a reversed actor slot"))

(defmethod reverse-actor-slot? (pred-name vocab)
  (declare (ignore pred-name vocab))
  (undefined-kb-source-error "reverse-actor-slot?"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility functions
;;; ----------------------------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod roots ((pred predicate))
  "The roots of a predicate are the predicate itself."
  (list pred))

(defun document-predicate (predicate &optional (stream t))
  "Write a description of the given predicate instance to the 
   given stream.  The description of the predicate includes the
   documentation, its argument list, and the pidgin representation
   string (if any)."
  (declare (type predicate predicate))
  (format stream "~A ~A~%" (name predicate) (arguments predicate))
  (format stream "~A~%" (doc-string predicate))
  (format stream "~A~%" (pidgin-description-string predicate))
  (terpri stream))

;;; Ubiquitous Predicates
;;; ----------------------------------------------------------------------------

(defun ubiquitous-predicates? (&optional (vocab *vocabulary*))
  (ubiquitous-predicates vocab)) 

(defmethod ubiquitous-predicate? ((pred predicate))
  "Is the predicate with this name ubiquitous in its vocabulary?"
  (ubiquitous-predicate-in? (id pred) (vocabulary pred)))

(defmethod ubiquitous-predicate? ((pred number))
   "Is the predicate with this name ubiquitous in its vocabulary?"
  (warn
      "ubiquitous-predicate? obsolete -- please use ubiquitous-predicate-in? instead.")
  (ubiquitous-predicate-in? pred *vocabulary*))

(defmethod ubiquitous-predicate? ((pred symbol))
  "Is the predicate with this name ubiquitous in its vocabulary?"
  (let ((predicate (find-predicate pred *vocabulary*)))
    (when (predicate? predicate)
      (ubiquitous-predicate-in? (id predicate) *vocabulary*))))

(defmethod ubiquitous-predicate-in? ((id predicate) (vocabulary t))
  (ubiquitous-predicate-in? (id id) vocabulary))

(defmethod ubiquitous-predicate-in? ((id symbol) (vocabulary t))
  (let ((predicate (find-predicate id vocabulary)))
    (when (predicate? predicate)
      (ubiquitous-predicate-in? (id predicate) vocabulary))))  

(defmethod ubiquitous-predicate-in? ((id number) (vocabulary vocabulary))
  (member id (ubiquitous-predicates vocabulary) :test '=))

(defmacro defubiquitous-predicate (predicate-name)
   "Define the predicate name as ubiquitous in the current vocabulary.
    Assumes that the predicate has already been defined. "
  `(declare-predicate-ubiquitous ',predicate-name *vocabulary*))  

(defun do-defubiquitous-predicate (predicate-name-in-list)
   (declare-predicate-ubiquitous (car predicate-name-in-list) *vocabulary*))

(defmethod declare-predicate-ubiquitous ((predicate-name t)
                                         (vocabulary vocabulary))
   "Declare that a predicate of the given name (in the given vocabulary)
    is ubiquitous.  A ubiquitous predicate is one that pervades a dgroup 
    to such an extent that it is nearly useless for indexing, and insufficient
    for a local match.  Ubiquitous predicates have special properties in MAC/FAC
    and SME.  In MAC/FAC, ubiquitous predicates are excluded from content vectors.
    In SME, ubiquitous predicates fil the local-alignable? test, and can be 
    aligned only as the arguments of aligned expressions."
   (let ((pred (find-predicate predicate-name vocabulary)))
      (when pred
         (pushnew (id pred)
           (ubiquitous-predicates vocabulary)))))

(defmethod undeclare-predicate-ubiquitous ((predicate-name t)
                                       (vocabulary vocabulary))
   "Declare that a predicate of the given name (in the given vocabulary)
    is no longer ubiquitous."
   (let ((pred (find-predicate predicate-name vocabulary)))
      (when pred
         (setf (ubiquitous-predicates vocabulary)
               (remove (id pred)
                 (ubiquitous-predicates vocabulary))))))

;;; Arity 
;;; ----------------------------------------------------------------------------
(defgeneric arity (item)
  (:documentation "Returns the arity of the item, which may be a predicate,
    expression, match hypothesis, or entity.  Entities have an arity of zero.
    Note: n-ary predicates return the arity of the original argument list,
    while n-ary expressions and match-hypotheses return the arity of the
    actual list of arguments for that instantiation.  Returns an integer."))

(defmethod arity ((item classic-predicate))
  "Return the number of arguments for this predicate."
  (numargs item))

;;; Attribute Values 
;;; ----------------------------------------------------------------------------
(defgeneric predicate-has-attribute-value-slots? (pred vocabulary))

(defmethod predicate-has-attribute-value-slots? (pred (vocabulary vocabulary))
  (declare (ignore pred))
  nil)
    
;;; Dumping
;;; ----------------------------------------------------------------------------

(defmethod sme-dumper ((pred predicate) (stream stream))
   "Dump an individual predicate to the output stream in rereadable format."
   ;; No headers, because these are never in stand-alone files.
   (format stream "~%(sme:defPredicate ~A ~A ~A"
     (name pred) (arguments pred) (pred-type pred))
   (if (commutative? pred)
      (output-keyword-value-pair-to-line ":commutative?" t stream))
   (if (n-ary? pred)
      (output-keyword-value-pair-to-line ":n-ary?" t stream))
   (if (symmetric? pred)
     (output-keyword-value-pair-to-line ":symmetric?" t stream))
   (if (script? pred)
      (output-keyword-value-pair-to-line ":script?" t stream))
   (if (set? pred)
      (output-keyword-value-pair-to-line ":set?" t stream))
   (if (group? pred)
      (output-keyword-value-pair-to-line ":group?" t stream))
   (if (role-relation-pos pred)
     (output-keyword-value-pair-to-line ":role-relation-pos" 
                                        (role-relation-pos pred) stream))
   (unless (= (length (pidgin-description-string pred)) 0)
      (format stream "~% :pidgin ~%~S "
        (pidgin-description-string pred)))
   (unless (= (length (doc-string pred)) 0)
      (format stream "~% :documentation ~S " (doc-string pred)))
   (unless (= (length (notes pred)) 0)
      (format stream "~% :notes ~S " (notes pred)))
   (format stream ")")
   )

(defmethod predicate= ((pred1 classic-predicate) (pred2 classic-predicate))
   (and (eq (name pred1) (name pred2))
        (equal (arguments pred1) (arguments pred2))
        (eq (commutative? pred1) (commutative? pred2))
        (eq (n-ary? pred1) (n-ary? pred2))
        (eq (symmetric? pred1) (symmetric? pred2))
        (eq (script? pred1) (script? pred2))
        (eq (set? pred1) (set? pred2))
        (eq (group? pred1) (group? pred2))
        (string= (pidgin-description-string pred1)
          (pidgin-description-string pred2))
        (string= (doc-string pred1) (doc-string pred2))
        (string= (notes pred1) (notes pred2))))

(defmethod predicate= ((pred1 neo-classic-predicate) (pred2 neo-classic-predicate))
   (and (eq (name pred1) (name pred2))
        (equal (arguments pred1) (arguments pred2))
        (eq (commutative? pred1) (commutative? pred2))
        (eq (n-ary? pred1) (n-ary? pred2))
        (eq (symmetric? pred1) (symmetric? pred2))
        (eq (script? pred1) (script? pred2))
        (eq (set? pred1) (set? pred2))
        (eq (group? pred1) (group? pred2))
        (eq (role-relation-pos pred1) (role-relation-pos pred2))
        (string= (pidgin-description-string pred1)
          (pidgin-description-string pred2))
        (string= (doc-string pred1) (doc-string pred2))
        (string= (notes pred1) (notes pred2))))
