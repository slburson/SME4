;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: vocabulary.lsp
;;;;    System: SME
;;;;    Author: Ken Forbus
;;;;   Created: March 15, 2001 12:47:50
;;;;   Purpose: Defines vocabulary access for SME
;;;; ---------------------------------------------------------------------------
;;;;  modified: Monday, November 10, 2008 at 12:11:19 by forbus
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;; Vocabularies represent sets of predicates that can be used to build
;;;  descriptions (Dgroups).  
;;;
;;; The interface to vocabularies includes:
;;;
;;; (vocabulary-from-file <source-file>) creates a vocabulary using 
;;;    the definitions in the file <source-file>, and makes it the default.
;;;   (in-vocabulary vocabulary)  -- Set default vocabulary.
;;;   (with-vocabulary vocabulary
;;;     (state1)(state2)...) -- Scope a set of statements


(defparameter *minimal-ascend-attributes* t 
  "Determines whether or not minimal ascension is applied to attributes as well as relations.") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is the base vocabulary class.  It contains only bookkeeping info,
;; such as which descriptions use it and the name of the vocabulary.
;;
;; Definition was moved to defs.lsp since other files that must be loaded before
;; this one depend on the class definition.

(defun create-empty-vocabulary (name &optional (class 'vocabulary))
   "Create an empty vocabulary with the given name and class.  New predicates
    can be added to an empty vocabulary using the define-predicate function
    or the defpredicate macro."
   (make-instance class :name name))

(defun vocabulary? (possible-vocabulary)
   "Is the given object an SME vocabulary?"
   (typep possible-vocabulary 'vocabulary))

(defmethod print-object ((vocab vocabulary) stream)
  (format stream "<Vocabulary ~A>" (name vocab)))

;;;; specializations of generic methods for default vocabulary

(defmethod variable? ((thing t) (vocab vocabulary))
  (and (symbolp thing) (eq (subseq (symbol-name thing) 0 1) #\?)))

(defmethod collection? ((thing t) (vocab vocabulary))
  (let ((p (gethash thing (predicate-table vocab))))
    (and p (eq (pred-type p) :attribute))))

(defmethod relation?  ((thing t) (vocab vocabulary))
  (let ((p (gethash thing (predicate-table vocab))))
    (and p (eq (pred-type p) :relation))))

(defmethod logical-connective?  ((thing t) (vocab vocabulary))
  (let ((p (gethash thing (predicate-table vocab))))
    (and p (eq (pred-type p) :logical))))

(defmethod pred-function?  ((thing t) (vocab vocabulary))
  (let ((p (gethash thing (predicate-table vocab))))
    (and p (eq (pred-type p) :function))))

(defmethod pred-arity ((thing t) (vocab vocabulary))
  (let ((p (gethash thing (predicate-table vocab))))
    (when p
      (cond ((n-ary? p) :n-ary)
            (t (length (arguments p)))))))

(defmethod commutative-relation? ((thing t) (vocab vocabulary))
  (let ((p (gethash thing (predicate-table vocab))))
    (and p (commutative? p))))

;;; Return the list of predicates.
(defmethod predicates ((vocab vocabulary) &aux pred-list)
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (push value pred-list))
	   (predicate-table vocab))
  pred-list)

(defmethod map-predicates ((vocab vocabulary) (proc function))
   "Map over the set of predicates in the vocabulary."
   (maphash #'(lambda (key value)
                 (declare (ignore key))
                 (funcall proc value))
      (predicate-table vocab)))

(defgeneric number-of-predicates (vocabulary)
  (:documentation
   "Returns the number of predicates in the given vocabulary.
    When the number of predicates cannot be determine 
    (true for some data sources) this function returns NIL."))

(defmethod number-of-predicates ((vocabulary t))
   "The default value for this function is to return NIL."
   nil)

(defmethod number-of-predicates ((vocabulary vocabulary))
   "Get the number of predicates in a typical vocabulary using
    the size of the hash table." 
  (hash-table-size (predicate-table vocabulary)))

;;Probably shouldn't be recursive on the subordinates of the vocabulary
(defmethod clear-predicates ((vocabulary vocabulary))
  "Clear the predicate cache (hash table) in the vocabulary"
  (clrhash (predicate-id-table vocabulary))
  (clrhash (predicate-table vocabulary)))

(defmethod add-description (dgroup &optional (vocab *vocabulary*))
   "Add a description to the given vocabulary."
   (setf (vocabulary dgroup) vocab)
   dgroup)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Methods for adding to the vocabulary
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric add-predicate (predicate &optional vocabulary)
  (:documentation "Add a single predicate to the vocabulary."))

(defmethod add-predicate ((pred predicate) &optional (vocab *vocabulary*))
  (setf (gethash (name pred) (predicate-table vocab)) pred)
  (setf (gethash (id pred) (predicate-id-table vocab)) pred)
  (setf (vocabulary pred) vocab)
  pred)

(defgeneric remove-predicate (predicate &optional vocabulary)
  (:documentation
   "Removes a predicate from the vocabulary. Rarely needed."))

(defmethod remove-predicate ((pred predicate) &optional (vocab *vocabulary*))
  (remhash (name pred) (predicate-table vocab))
  (remhash (id pred) (predicate-id-table vocab))
  (setf (vocabulary pred) nil)
  pred)


(defgeneric find-predicate (predicate-name container &key create?)
  (:documentation  
   "Find the predicate of the given name in the given predicate container.
    The container may be a vocabulary, dgroup, or database-vocabulary.
    If create? is non-nil, the predicate is created if it is not found
    If create? is nil, Returns NIL if predicate cannot be found.
    ")) 


(defmethod find-predicate ((predicate-name t) (vocab vocabulary) &key (create? t))
  ;; Predicate is either in predicate table, or may be in subordinate vocabulary.
  (declare (ignore create?))
  (or (gethash predicate-name (predicate-table vocab))
      (some #'(lambda (sub) (find-predicate predicate-name sub :create? nil)) 
            (subordinates vocab))))
  
  
(defmethod find-predicate ((predicate-name symbol) (vocab vocabulary) &key (create? t))
   ;; Predicate is either in predicate table, or may be in subordinate predicate
   ;;  table.
  (let ((entry (or (gethash predicate-name (predicate-table vocab))
                   (some #'(lambda (sub) 
                             ;; Hard specify create? to be nil.  Only create predicates
                             ;;  in the vocabulary being used, not in any vocabularies
                             ;;  it can call on. If someone wants to specifically add
                             ;;  something to another vocabulary, they can do that manually
                             (find-predicate predicate-name sub :create? nil)) 
                         (subordinates vocab)))))
    (cond (entry entry)
          ((null create?) nil)
          (t
           (create-predicate predicate-name vocab)))))


(defmethod find-predicate ((predicate-id number) (vocab vocabulary) &key (create? t))
  ;; Predicate is either in predicate table, or may be in subordinate vocabulary.
  (declare (ignore create?))
  (or (gethash predicate-id (predicate-id-table vocab))
      (some #'(lambda (sub) (find-predicate predicate-id sub :create? nil))
            (subordinates vocab))))

(defmethod find-predicate (predicate-name (dgroup t) &key (create? t))
   "Find the predicate in a given dgroup (just defaults to vocabulary)."
   (find-predicate predicate-name (vocabulary dgroup) :create? create?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-predicate ((predicate-name t) (vocab vocabulary))
  (let ((pred (make-sme-predicate-entry predicate-name vocab)))
    (cond ((null pred) nil)
          (t
           (add-predicate pred vocab)
           pred))))

(defmethod predicate-type (pred vocab)
  ;; Returns one of :relation, :attribute :function :logical
  ;; These are based on the SME predicate types
  (cond ((variable? pred vocab) nil)
        ((collection? pred vocab) :attribute)
        ((eq (pred-arity pred vocab) :unknown) nil)
        ((pred-function? pred vocab) :function)
        ((logical-connective? pred vocab) :logical)
        ((relation? pred vocab) :relation)
        (t nil))) 

(defmethod make-sme-predicate-entry (predicate-name vocab)
  (cond ((listp predicate-name)
         (cond ((eq (car predicate-name) 'data::lambda)
                ;; (lambda (arglist) <body>)
                (define-sme-predicate predicate-name (cadr predicate-name)
                  :function (length (cadr predicate-name))
                  nil vocab)) ;; assume not commutative
               ((or (eq (car predicate-name) 'data::kappa)
                    (eq (car predicate-name) 'data::Kappa))
                (define-sme-predicate predicate-name (cadr predicate-name)
                  :relation (length (cadr predicate-name))
                  nil vocab)) ;; assume not commutative
               (t (error "Unknown predicate type: ~A in ~A." 
                    predicate-name vocab))))
        ((variable? predicate-name vocab) 
         nil)
        ((symbolp predicate-name)
         (let ((predicate-type (predicate-type predicate-name vocab)))
           (when predicate-type
             (let ((arity (pred-arity predicate-name vocab)))
               (define-sme-predicate predicate-name (make-dummy-arglist arity)
                                     predicate-type
                                     arity
                                     (commutative-relation? predicate-name vocab) vocab)))))
        (t (error "Unknown predicate name type: ~A in ~A."
             predicate-name vocab))))



(defun make-dummy-arglist (arity)
  (if (eq arity :n-ary) '()
    (if (integerp arity)
        (let ((counter -1)
              (result nil))
          (dotimes (i arity (nreverse result))
            (push (incf counter) result)))
      '())))

(defun role-relation-predicate-pos (name type arity vocab)
  ;; Returns nil if name is not a role-relation-predicate
  ;; Returns the index into the ARGUMENTS of an expression
  ;;  corresponding to the reified-event.
  ;; Returns either 1 or 2
  ;;  1 = the first argument in the expression
  ;;  2 = second argument.  (n.b. think nth on lisp form, 0 is functor)
  (cond ((not (and (eq type :relation)
                   (integerp arity)
                   (= arity 2)))
         nil)
        ((actor-slot? name vocab)  1)
        ((reverse-actor-slot? name vocab) 2)
        (t nil)))

(defun define-sme-predicate (name arguments type arity commutative? vocab)
  (make-instance 'sme::neo-classic-predicate
    :name name :id (incf (id-counter vocab))
    :arguments arguments :numargs arity
    :type type :commutative? commutative?
    :n-ary? (eq arity :n-ary)
    :role-relation-pos (role-relation-predicate-pos name type arity vocab)
    :frame-attribute (frame-attribute-predicate? name type vocab)
    :vocabulary vocab))

(defmethod role-relation? (pred-name (vocab vocabulary))
  (let ((pred (find-predicate pred-name vocab :create? nil)))
    ;; Defaults to nil
    (cond ((null pred) (values nil :unknown))
          ((numberp (role-relation-pos pred))
           (values t :known))
          (t (values nil :known)))))

(defun role-relation-predicate? (name type arity vocab)
  (and (eq type :relation)
       (and (numberp arity) (= arity 2))
       (role-relation? name vocab)))

(defun frame-attribute-predicate? (name type vocab)
  (and (eq type :attribute)
       (event-predicate? name vocab)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility routines.

(defun in-vocabulary (vocabulary)
  "Set the default vocabulary, *vocabulary*, to the given value.  Analogous to 
   IN-PACKAGE."
  (declare (type vocabulary vocabulary))
  (setf *vocabulary* vocabulary))

(defmacro with-vocabulary (vocabulary &body statements)
  "For the set of statements given, temporarily bind
   the value of the default vocabulary to the passed value."
  `(let ((*vocabulary* ,vocabulary))
     ,@ statements))

(defun vocabulary-from-file (name &optional (depth nil))
  (let ((vocabulary (create-empty-vocabulary name))
        (file-name (concatenate 'string 
                       (if (pathnamep *sme-vocabulary-path*)
                           (namestring *sme-vocabulary-path*)
                         *sme-vocabulary-path*)
                     name
                     (if (search *sme-vocabulary-extension* name
                                 :test 'string-equal)
                         "" (format nil ".~A" *sme-vocabulary-extension*)))))
        (when (integerp depth)
          (setf (minimal-ascension-depth vocabulary) depth))
    (with-vocabulary vocabulary (load-sme-data file-name))
    (in-vocabulary vocabulary)
    vocabulary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun document-vocabulary (vocabulary &optional (stream t))
  "Write a description of every predicate in the entire vocabulary
   to the given stream."
  (declare (type vocabulary vocabulary))
  (format stream "~80:@<vocabulary ~A~>~%" (name vocabulary))
  (format stream "~A~%~%" (doc-string vocabulary))
  (maphash #'(lambda (key pred)
	       (declare (ignore key))
	       (document-predicate pred stream))
	   (predicate-table vocabulary)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dumping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod sme-dumper ((vocabulary vocabulary) (stream stream))
   (let ((*package* (find-package :common-lisp-user)))
      (dump-sme-file-header "Vocabulary" stream)
      (format stream "~%(sme:vocabulary-documentation ~S~%  )"
        (doc-string vocabulary))
      (format stream "~%(sme:vocabulary-notes ~% ~S~%  )"
        (notes vocabulary))
      (dolist (pred (sort (copy-list (predicates vocabulary))
                       #'(lambda (x y) (string< (symbol-name (name x))
                                         (symbol-name (name y))))))
         (sme-dumper pred stream))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Comparing Vocabularies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun same-predicates? (vocab1 vocab2)
   "Compare set of predicates in two vocabularies.  Extremely expensive operation."
   (null (set-exclusive-or (predicates vocab1)
           (predicates vocab2) :test 'predicate=)))

(defun vocabulary-comparison (vocabulary1 vocabulary2 &optional (stream *standard-output*))
   "This is a tool for comparing vocabularies to see which predicates
    are held in common, and which predicates are different in a
    pair of vocabularies.  Vocabulary 1 and 2 can be either 
    vocabulary objects of filenames. Returns nil."
   (when (stringp vocabulary1)
      (setq vocabulary1 (vocabulary-from-file vocabulary1)))
   (when (stringp vocabulary2)
      (setq vocabulary2 (vocabulary-from-file vocabulary2)))
   (let* ((preds1 (sort (copy-list (predicates vocabulary1)) #'string< :key 'name)) 
          (preds2 (sort (copy-list (predicates vocabulary2)) #'string< :key 'name))
          (1not2 (set-difference preds1 preds2 :test 'predicate=))
          (2not1 (set-difference preds2 preds1 :test 'predicate=))
          (changed-preds (intersection 1not2 2not1 :key 'name :test 'string=)))
      ;; Rewrite the set differences to account for predicates that are
      ;;  different, but have the same name.
      (setq 1not2 (set-difference 1not2 changed-preds :key 'name :test 'string=))
      (setq 2not1 (set-difference 2not1 changed-preds :key 'name :test 'string=))
      ;; Print out the information.
      (format stream "~%~%Predicates in first, but not in second:~%")
      (pprint 1not2 stream)
      (terpri stream)
      (format stream "~%~%Predicates in second, but not in first:~%")
      (pprint 2not1 stream)
      (terpri stream)
      (format stream "~%~%Changed predicates:~%")
      (pprint changed-preds stream)
      (terpri stream))
   nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
