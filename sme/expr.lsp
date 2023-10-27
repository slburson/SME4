;;;; -*- Mode: Lisp; -*-
;;;; ------------------------------------------------------------
;;;; File name: expr.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;    Author: Ken Forbus & Ron Ferguson
;;;;  $LastChangedDate: 2015-02-15 01:09:52 -0600 (Sun, 15 Feb 2015) $
;;;;  $LastChangedBy: liang $
;;;; ------------------------------------------------------------

(in-package :sme)

;;; Expression definition

;;; All expressions are created within a particular dgroup.
;;; Within a dgroup, it is an error to have two expressions for
;;;  two equal lisp forms.
;;; Two expressions from different dgroups are equivalent if their
;;;   lisp forms are equal.

(defclass expression (item)
  ((id
    :documentation "Integer identifier for expression"
    :initarg :id  :reader id)
   (predicate 
    :documentation "The predicate of the expression"
    :initarg :predicate :accessor predicate 
    :accessor functor)   ;; synonym.
   (arguments
    :documentation "Alist of arguments for expression"
    :initarg :arguments  :accessor arguments)
   (user-form
    :documentation "Form user wants to see (rereadable?)"
    :initarg :user-form  :reader user-form)
   (description
    :documentation "Dgroup the expression belongs to"
    :initarg :description  :initform nil  :accessor description)
   (order
    :documentation "The order expression possesses in dgroup"
    :initarg :order  :initform -99  :accessor order)
   (parents
    :documentation "Expressions for which this is an argument."
    :initarg :parents  :initform nil  :accessor parents)
   (timestamp
    :documentation "Integer timestamp, relative to dgroup clock."
    :initarg :timestamp  :accessor timestamp)
   ;; SEF - added this bit to track whether an expression is top-level.
   (top-level?
    :documentation "Whether or not this expression is a top-level fact."
    :initarg :top-level? :initform nil :accessor top-level?))
  (:documentation
   "An expression represents a single predicate calculus expression,
    which may take either entities or other expressions as objects."))

(defclass probabilistic-expression (expression)
  ((probability
    :documentation "Probability that the expression is true"
    :initform 1 :initarg :probability)
   (unlikely-count
    :documentation "Counts the number of rounds that the expr has been held
      onto, despite its improbability.  should be replaced w/ significance."
    :initform 1 :accessor unlikely-count)))

(defclass attribute-expression (expression) ())

(defclass probabilistic-attribute-expression 
    (attribute-expression probabilistic-expression) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Basic generic procedures for expressions
(eval-when (compile load eval)
  (proclaim '(inline expression? function?)))

(defun expression? (possible-expr)
   "Is the argument an SME dgroup expression?"
   (typep possible-expr 'expression))

(defmethod print-object ((exp expression) stream)
  (format stream "<Exp ~A~A>" (name (predicate exp))(name exp)))

(defmethod make-load-form ((expr expression) &optional env)
  (declare (ignore env))
  (let ((lisp-form (lisp-form expr))
        (prob (when (typep expr 'probabilistic-expression)
                (sme::probability expr))))
    (if prob (cons prob lisp-form) lisp-form)))

(defgeneric function? (object)
  (:documentation
   "True if this particular object either a function
    predicate or an expression containing a function
    predicate."))

(defmethod function? ((expr expression))
  (function? (predicate expr)))

(defmethod attribute? ((expr expression))
   (attribute? (predicate expr)))

(defmethod arity ((expr expression))
  (length (arguments expr)))

(defmethod rereadable-form ((exp expression)) (user-form exp))

(defgeneric probability (expr))
(defmethod probability ((expr expression)) 1)
(defmethod probability ((expr probabilistic-expression))
  (slot-value expr 'probability))

(defgeneric (setf probability) (new-val expr))
(defmethod (setf probability) (new-val (expr expression))
  (declare (ignore new-val))
  (error "Cannot set the probability of a non-probabilistic expression."))
(defmethod (setf probability) (new-val (expr probabilistic-expression))
  (setf (slot-value expr 'probability) new-val))

;;; Give the arguments without the case-relations.
(defmethod children ((exp expression))
  (mapcar 'cdr (arguments exp)))

(defun expression-leaves (expr)
   "For the given expression, return all the leaf nodes (i.e., 
    entities) in the expression."
   (declare (type expression expr))
   (mapcan #'(lambda (child)
               (cond ((entity? child)
                      (list child))
                     ((expression? expr)
                      (expression-leaves child))
                     (t 
                      (error "~A neither entity nor expression" expr))))
     (children expr)))

;; Be minimal on names, count on GUI to make things clearer
(defmethod name ((exp expression)) (id exp))

(defun make-expr-arg (case-relation item)
  "Construct an expression argument."
  (cons case-relation item))

(defun expr-arg-case-relation (expr-arg) (car expr-arg))
(defun expr-arg-item (expr-arg) (cdr expr-arg))


;;;; Calculating structural properties of an expression

(defmethod descendants ((exp expression))
  (let ((desc-queue (list exp))
	(all-descendants nil))
    (do ((current-exp (car desc-queue) (car desc-queue))
	 (desc-queue (cdr desc-queue) (cdr desc-queue)))
	((null current-exp) all-descendants)
      (unless (entity? current-exp)
	(pushnew current-exp all-descendants :test #'eq)
	(dolist (child (children current-exp))
	  (push child desc-queue))))))

(defmethod roots ((exp expression))
  (if (null (parents exp)) (list exp) ;; self-rooted
      (let ((roots nil))
	(dolist (parent (parents exp) roots)
	  (dolist (new-root (roots parent)) ;; counting on low depth
	    (pushnew new-root roots :test #'eq))))))

(defmethod root? ((thing t)) nil)

(defmethod root? ((thing expression)) (null (parents thing)))

;;; Defining an expression. 
;;; The expression code currently does not check the type of the predicate
;;; to see if it is n-ary, commutative, etc.  Instead, it assumes (at least
;;; in matching the expression arguments to the predicate arguments) that
;;; any "extra" arguments in the expression must have the same case
;;; relation, namely, the last case-relation in the predicate's
;;; list.  This corresponds to the way we were handling n-ary expressions
;;; before.

(defparameter *touch-creation-time-on-redefinition* nil
  "Indicates if the timestamp associated with the expression is reset when 
   the expression is referenced/defined at a later time") 

(defmacro with-creation-time-touch-on-redefinition (&body body)
   "Execute the <body> but allow timestamp associated with the expression is reset when 
   the expression is referenced/defined"
   `(let ((*touch-creation-time-on-redefinition* t))
       ,@body))

;;;;Variable that can be set to the type of expression to be created
(defparameter *default-expression-class* 'expression)
(defparameter *default-probabilistic-expression-class* 'probabilistic-expression)
(defparameter *default-attribute-expression-class* 'attribute-expression)

;;;; Default method for defining expressions.
;;;;
;;;; Assumes that the expression is a list, whose CAR is the functor
;;;; and whose arguments are the rest of the list.

(defgeneric define-expression (user-form dgroup &optional id probability)
  (:documentation 
   "Define a new expression within the given dgroup.  Returns
    the newly defined expression.  define-expression is seldom
    called directly -- dgroups are constructed with define-description
    or defdescription instead.  However, define-expression is useful
    when interfacing SME with systems that produce representations,
    or for extending an existing dgroup (very useful when incremental mapping
    is used, for example)."))

(defmethod define-expression ((user-form t) (description description)
                              &optional (expression-id nil) (probability nil))
  (let ((expression (define-expression% user-form description
                      expression-id probability)))
    (when expression
      (push expression (top-level-expressions description))
      (setf (top-level? expression) t)
      expression)))

(defgeneric define-expression% (user-form dgroup &optional id probability)
  (:documentation 
   "Helper method for defining expressions."))

(defmethod define-expression% ((user-form t) (description description)
                               &optional (expression-id nil) (probability nil))
  "Define a new expression within the given Description.  Returns 
   the newly defined expression."
  (declare (type description description))
  (unless (or (listp user-form) (null user-form))
    (error "SME: Expression for ~A must be a list, nonempty: ~A."
      description user-form))
  ;; If the expression has already been defined, just return it.
  (let ((existing (fetch-expression-flexible user-form description)))
    (cond ((and existing *touch-creation-time-on-redefinition*)
           (setf (timestamp existing) (advance-dgroup-timeclock description)))
          ((not existing) ; Check for NATs
           (setq existing (fetch-entity user-form description)))) 
    (when existing
      (when (and probability (expression? existing)
                 (typep existing 'probabilistic-expression))
        (setf (probability existing) probability))
      (return-from define-expression% existing)))
  ;;Otherwise need to create it
  (let* ((vocabulary (vocabulary description))
         (exp-predicate (find-predicate (car user-form) vocabulary)))
    (when (null exp-predicate)
      (error "SME: ~A doesn't define predicate ~A: ~A."
        vocabulary (car user-form) user-form))
    (make-new-expression exp-predicate user-form description
                         :id expression-id :probability probability)))

;;; Should only be used internally, if you really know what you're doing,
;;;  as it bypasses all the checks done in define-expression
(defun make-new-expression (predicate user-form description 
                                      &key id probability arguments)
  (unless arguments (setf arguments (cdr user-form)))
  (let ((expression
         (make-instance 
             (cond ((and probability (attribute? predicate))
                    'probabilistic-attribute-expression)
                   ((attribute? predicate) *default-attribute-expression-class*)
                   (probability *default-probabilistic-expression-class*)
                   (t *default-expression-class*))
           :id (or id (draw-expression-id description))
           :description description
           :timestamp (advance-dgroup-timeclock description)
           :predicate predicate :user-form user-form)))
    (when probability (setf (probability expression) probability))
    (when (catch 'argument-arity-error 
            (fill-in-expression-arguments 
             expression predicate arguments description))
      (add-expression expression description)
      (push expression (roots description))
      expression)))

(defun add-expression (new-expression description)
   (let ((bin nil))
      (cond ((setq bin (assoc (predicate new-expression)
			      (expressions description)))
             (setf (rest bin) (cons new-expression (rest bin))))
            (t
             (push (cons (predicate new-expression) (list new-expression))
                   (expressions description))))))
    
(defmethod drawn-from-external-knowledge? ((description t)) nil)
;; The default is no, for standalone operation.  This method exists
;; because when we are drawing dgroups from external knowledge sources
;; (e.g., KB's or other programs) sometimes stuff will be returned that
;; we can't actually deal with.  We don't always have control over these
;; external systems, and hence SME must be able to ignore such problems.
;; We don't want to do this by default, since this will mask bugs. 


;;;; FILL-IN-EXPRESSION-ARGUMENTS
;;; defines how the arguments for a specific predicate with respect to
;;; a particular vocabulary are filled in.  Thus relations and functions could
;;; be treated differently within a specific vocabulary, and indeed different
;;; vocabularies (aka KR systems) could encode their arguments differently.
;;;
;;; In general, we assume args is a list, in some appropriate format.

(defvar *_building-attribute-value* nil)
;;when you are sure that 
;;(predicate-has-attribute-value-slots? (name pred) (vocabulary description))
;;will return nil, can turn this to nil to avoid checking for better efficiency.
(defvar *check-avslots* t) 

(defmethod fill-in-expression-arguments (expr pred args description)
  ;; Default version, just lines'em up.
  (declare (type expression expr) (type predicate pred)
           (type description description))
  (let ((avslots (and *check-avslots* 
                      (predicate-has-attribute-value-slots?
                       (name pred) (vocabulary description)))
          ))
    (do* ((pred-args (arguments pred))
          (new-arguments nil)
          (arg-counter 0 (1+ arg-counter));; count arguments.
          (exp-args args (cdr exp-args))   
          (pr-args pred-args (cdr pr-args))
          (earg (car exp-args) (car exp-args))
          (parg (car pr-args) (car pr-args)))
         ((and (null earg) ;; allow nil as argument to an expression
               (null exp-args)) 
          (setf (arguments expr)
            (reverse new-arguments))
          (install-expression-argument-relationships expr description)
          expr)
      (unless parg 
        (setq parg (handle-extra-args expr parg
                                      arg-counter)))
      (let ((*_building-attribute-value* 
             (or *_building-attribute-value* (member (1+ arg-counter) avslots))))
        (push (create-description-item earg parg description
                                       (vocabulary description))
              new-arguments)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creates entity or expression, according to the form, role, and vocabulary.

(defmethod create-description-item ((form t) role description (vocabulary t))
  (declare (type description description))
  ;; Simplest case -- if list, it's an expression.  Otherwise, it's an
  ;; entity.  Other methods should be used for KR systems that have
  ;; compound objects.
  (cond ((atom form)
         (make-expr-arg (if (listp role) (case-relation role) role)
                        (define-entity form description)))
        (t (let ((entity (fetch-entity-unless-isa-value form description)))
             (cond (entity (make-expr-arg (if (listp role) (case-relation role)
                                            role)
                                          entity))
                   (t (let ((subexp (define-expression% form description)))
                        (if (null subexp) (throw 'argument-arity-error nil)
                          (make-expr-arg (if (listp role) (case-relation role)
                                           role)
                                         subexp)))))))))

(defmethod create-description-item ((entity entity) role description
                                    (vocabulary t))
  (declare (ignore description))
  (make-expr-arg (if (listp role) (case-relation role) role) entity))

(defmethod create-description-item ((expr expression) role description
                                    (vocabulary t))
  (declare (ignore description))
  (make-expr-arg (if (listp role) (case-relation role) role) expr))

;; If we have additional arguments to the expression,
;;  then if
;;    Normal expression: signal error (too many args).
;;    N-ary: add additional case relations.
;;     If the expression is also
;;       Commutative: added case-relations are identical.
;;       Noncommutative: added case-relations are non-identical.
;;
(defun handle-extra-args (expression pred-arg arg-counter)
  (declare (type expression expression) (type integer arg-counter))
  (flet ((new-case-relation (counter)
	   (intern 
	    (concatenate 'string "ARG-" (write-to-string counter)))))
    (let ((predicate (predicate expression)))
      (cond
       ((not (n-ary? predicate))
        (cond ((member :runtime-system *features*)
               (warn "Too many arguments in expression ~A" (user-form expression))
               (throw 'argument-arity-error nil))
              (t (error "Too many arguments in expression ~A" (user-form expression)))))
       ((commutative? predicate);; if set, return same case-relation as before.
	pred-arg)
       (t  ;; if n-ary but not set, generate new case-relation from
	;; last arg and new arg-counter.
	(make-expr-arg (new-case-relation arg-counter) (cdr pred-arg)))))))

(defun install-expression-argument-relationships (expr description)
  (declare (type expression expr) (type description description))
  (dolist (arg-pair (arguments expr) expr)
;;  (format t "~% Deleting ~A as a root.." (cdr arg-pair))
    (let ((arg-expr (cdr arg-pair)))
      (setf (roots description)
	    (delete arg-expr (roots description) :count 1))
      (push expr (parents arg-expr)) ;; install parent pointer
      (setf (order expr)
	    (max (order expr) (1+ (order arg-expr)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntactic sugar.

(eval-when (compile load eval)
  (proclaim '(inline case-relation argument-type)))

(defun case-relation (pred-arg) (car pred-arg))
(defun argument-type (pred-arg) (cdr pred-arg))

(defun expr# (num dgroup)
  (dolist (expr (get-dgroup-expressions dgroup))
   (if (= (id expr) num)
       (return-from expr# expr))))

;;;;; Expression validation
;;
;; The idea is to validate expressions 

(defmethod validate-form-as-expression-for-dgroup
    ((form t) (dgroup description))
   (values nil '("Must be a list (possibly nested)")))

(defmethod validate-form-as-expression-for-dgroup
    ((form list) (dgroup description))
   (validate-form-as-expression-using-vocabulary
     form (vocabulary dgroup)))

(defun validate-form-as-expression-using-vocabulary (form vocabulary)
   (let ((problems-found nil))
      (map-through-expression
       #'(lambda (expr)
           (let ((predicate (find-predicate (car expr) vocabulary)))
              (cond ((null predicate)
                     (push (format nil "Predicate ~A not found"
                             (car expr))
                       problems-found))
                    (t (let ((nargs (length (cdr expr)))
                             (valid-nargs (arity predicate)))
                          (unless (or (n-ary? predicate)
                                      (= nargs valid-nargs))
                             (push 
                               (format nil 
                                 "~A given ~A arguments, takes ~A"
                                 (name predicate) nargs
                                 valid-nargs)
                               problems-found)))))))
       form)
      (if problems-found (values nil problems-found)
         (values t nil))))

(defun map-through-expression (procedure expression)
   ;; Calls procedure on the expression and all of its subexpressions,
   ;; recursively.  Assumes simple form of expression, i.e., 
   ;; (<predicate> . <arguments>), with no keywords and no currying.
   (funcall procedure expression)
   (dolist (argument (cdr expression))
      (when (listp argument) ;; presumed to be an expression
         (map-through-expression procedure argument))))

;;;; Expression deletion
;;;; This should be used with extreme caution!

(defun delete-expression (expr)
   (delete-expression-from-dgroup expr)
   ;; Clean up entities, SEK 7/5/99
   (dolist (entity (entities (description expr)))
      (setf (parents entity) (delete expr (parents entity))))
   ;; Clean up roots, SEK 7/5/99
   (setf (roots (description expr)) (delete expr (roots (description expr))))
   ;; Recursive is okay, since depth is likely to be very small
   (dolist (parent (parents expr))
      (unless (eq parent expr) (delete-expression parent))))

      
(defun delete-expression-from-dgroup (expr)
   ;; 
   (let ((bin (assoc (predicate expr) (expressions (description expr)))))
             (delete expr bin :test #'eq)))

;;; Handling attribute values
(defun expression-contains-attribute-value? (expr-form vocab &optional ispred?)
  (cond
   (ispred? (predicate-has-attribute-value-slots? expr-form vocab))
   ((listp expr-form)
    (or (expression-contains-attribute-value? (car expr-form) vocab t)
        (some (lambda (arg) (expression-contains-attribute-value? arg vocab))
          (cdr expr-form))))))

(defmethod user-forms-equal ((item1 expression) (item2 expression))
  (eq item1 item2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;Fixing this for symmetric relations

(defmethod user-forms-equal ((item1 list) (item2 expression))
  (and (equal (car item1) (car (user-form item2)))
       (equal (length (cdr item1)) (length (arguments item2)))
       (or (every (lambda (arg1 arg2) (user-forms-equal arg1 (cdr arg2)))
                  (cdr item1) (arguments item2))
           (and (symmetric? (predicate item2))
                (every (lambda (arg1 arg2) (user-forms-equal arg1 (cdr arg2)))
                  (cdr item1) (reverse (arguments item2)))))))

