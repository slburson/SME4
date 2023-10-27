;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: dgroup.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ken Forbus & Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: ???
;;;;   Purpose: Description definition and management
;;;; --------------------------------------------------------------------------
(in-package :sme)

(defparameter *reverse-expressions-in-dgroups?* nil)

(defclass description (documented-object)
  ((expressions
    :documentation "The expressions within this dgroup"
    :type list :initarg :expressions :accessor expressions :initform nil)
   (entities
    :documentation "The entities used within this dgroup"
    :type list :initarg :entities :accessor entities :initform nil)
   (roots
    :documentation "The root expressions in the dgroup"
    :type list :initarg :roots :accessor roots :initform nil)
   (top-level-expressions
    :documentation "Top level expressions in the dgroup"
    :type list :initarg :top-level-expressions :accessor top-level-expressions 
    :initform nil)
   (vocabulary
    :documentation "The vocabulary description uses is over"
    :type vocabulary :initarg :vocabulary :accessor vocabulary
    :initarg :vocabulary ;; for backward compatibility.
    )
   (expr-counter
    :documentation "id counter for expressions."
    :type integer :initarg :expr-counter :accessor expr-counter :initform 0)
   (entity-counter
    :documentation "id counter for entities."
    :initarg :entity-counter :accessor entity-counter :initform 0)
   (id
    :documentation "Integer id for the dgroup"
    :initarg :id :accessor id :type integer)
   (timeclock
    :documentation "The local timestamp, inc'd at each addition"
    :type integer :accessor timeclock :initarg :timeclock :initform 0)
   (plist :accessor plist :initarg :plist :initform nil
    :documentation "slot for application information"))
  (:documentation 
   "A Description (also called a Dgroup) is a set of predicate
    calculus expressions which describe an object, situation, or
    domain.  New descriptions can be defined using the DefDescription
    macro or the define-description function.")) 

(defun description? (possible-description)
   "Is the given object an SME description?"
   (typep possible-description 'description))

(defun dgroup? (possible-description)
   "Is the given object an SME description?"
   (typep possible-description 'description))

(defmethod print-object ((dgroup description) stream)
  (format stream "<Description ~A>" (name dgroup)))

(defun dgroup-from-file (file-name)
  (with-vocabulary *vocabulary*
    (load-sme-data 
     (format nil "~A~A~A" *sme-description-path*
       file-name
       (if (search *sme-description-extension* file-name)
           "" ;; already there
         (format nil ".~A" *sme-description-extension*))))))

;; Following used by MARS.
(defmacro with-description-file-information (pathname extension &rest body)
   "Run the body forms with the given dgroup pathname and extension"
   `(let ((*sme-description-path* ,pathname)
          (*sme-description-extension* ,extension))
       ,@ body))

(defun merge-descriptions (new-name dgroup1 dgroup2)
  "Create a description that contains all the expressions
   of both the dgroups given.  Operation is not destructive.
   Done by taking set of all entity and expression forms
   and reading them into a new dgroup.  No expression or
   entity objects are shared."
  (flet ((dgroup-entities (dgroup)
	   (mapcar 'user-form (entities dgroup)))
	 (dgroup-expressions (dgroup)
	   (mapcar 'user-form (roots dgroup))))
    (let ((entity-list (union (dgroup-entities dgroup1)
			      (dgroup-entities dgroup2)))
	  (expr-list (union (dgroup-expressions dgroup1)
			    (dgroup-expressions dgroup2))))
      (define-description new-name entity-list expr-list))))

;;;------------------------------------------------------------
;;; Macros to create descriptions.
;;;------------------------------------------------------------

;; Backward compatibility --if there is no :expressions
;;   keyword in the description form, assume that
;;   the entire body of the description is the expression list.
;;
(defmacro defdescription (&rest name-and-exps)
  "Define a domain description.  The first argument is the name
   of the description.  The remaining forms are treated as
   expressions describing the domain.  Equal expression forms 
   are treated as duplicated references to the same form.  
   Form arguments which are symbols (i.e. not lists) are 
   defined as entities, while the first symbol of the form
   is assumed to be an expression predicate.  DefDescription
   always uses the default vocabulary (*vocabulary*).
   DefDescription expands to define-description to create 
   the description.  If the sexpr after the name is a string,
   it is assumed to be the documentation string."
  `(do-defdescription ',name-and-exps))

(defun do-defdescription (name-and-exps &aux (doc-string ""))
   (let ((name (car name-and-exps))
         (exps (cdr name-and-exps)))
      (when (stringp (car exps))
         (setq doc-string (car exps))
         (setq exps (cdr exps)))
      (let ((entities (keyword-value ':entities exps))
            (documentation (keyword-value ':documentation exps))
            (probabilities (keyword-value ':probabilities exps))
            (notes (or (keyword-value ':notes exps) ""))
            ;;; passing a default value of -1 since nil should be a legitimate 
            ;;; value for expressions.
            (expressions (let ((value (keyword-value ':expressions exps
                                                     :default-value -1)))
                            (if (eq value -1)
                               exps
                               value))))
         (when documentation
            (setq doc-string (concatenate 'string doc-string documentation)))
         (setq expressions
           (perform-dgroup-name-substitutions expressions))
         (define-description name entities expressions
           *vocabulary* doc-string notes probabilities))))

(defun entities-from-expr-forms (expr-list &aux entities expr-names skipnext)
  (setq entities
        (mapcan #'(lambda (expr)
                    (cond ((listp expr)
                           (copy-list (entities-from-expr-form expr)))
                          ((eql expr ':name)
                           (setq skipnext t)
                           nil)
                          (skipnext
                           (push expr expr-names)
                           (setq skipnext nil))
                          (t (error "Unknown expressions form: ~A"
                               expr))))
          expr-list))
  (setq entities (remove-duplicates entities :test #'eq))
  (setq expr-names (remove-duplicates expr-names :test #'eq))
  (setq entities (set-difference entities expr-names :test #'eq))
  (values entities expr-names))

(defun entities-from-expr-form (expr-form)
  (mapcan #'(lambda (expr-arg)
              (cond ((or (symbolp expr-arg)
                         (numberp expr-arg)) (list expr-arg))
                    ((listp expr-arg) (entities-from-expr-form expr-arg))
                    (t (error "Don't understand expression form: ~A in expr: ~A"
                         expr-arg expr-form))))
    (cdr expr-form)))

(defun test-entities-from-expr-forms ()
  (entities-from-expr-forms '((precedes a b (sequence 10 11))
			      (inside x) (red y)
			      (given name) :name name1
			      (take name1))))
			      

(defparameter *sme-description-default-class* 'description
  "The default dgroup type used by SME.  Can be modified to 
   change the type of dgroups SME uses. Useful when embedded in
   larger-scale reasoners, for integration with their knowledge
   systems.")

(defmacro with-sme-description-type (type &rest forms)
   "Evaluate forms using the following dgroup type as the default type."
   `(let ((*sme-description-default-class* ,type)) ,@ forms))

(defun define-description (name entities expressions
                                &optional (vocabulary *vocabulary*)
                                (documentation "")
                                (notes "")
                                (probabilities)
                                &aux new-dgroup)
  "Create a new description (Dgroup) object with 
   the given name, entities and expressions.  The 
   entities argument is given as a list of symbols, while
   the expressions argument is a list of forms.
   If the vocabulary argument is omitted, it defaults to
   *vocabulary*."
  ;; Create the empty dgroup.
  (setq new-dgroup (make-instance *sme-description-default-class*
				  :name name
				  :id (get-new-dgroup-id)
				  :vocabulary vocabulary
				  :doc-string documentation
                                  :notes notes))
  ;; Add the dgroup to the vocabulary
  (add-description new-dgroup vocabulary)
  
  (let ((expressions (if *reverse-expressions-in-dgroups?*
                       (reverse expressions)
                       expressions))
        (probabilities (if *reverse-expressions-in-dgroups?*
                       (reverse probabilities)
                       probabilities))
        (entities (if *reverse-expressions-in-dgroups?*
                    (reverse entities)
                    entities)))
  
    ;; Create the entities.
    (dolist (entity-name entities)
      ;; We assume that the list of entities is duplicate-free.
      (define-entity entity-name new-dgroup))
    
    (if probabilities
      (mapcar #'(lambda (expression probability)
                  (define-expression expression new-dgroup nil probability))
        expressions probabilities)
      (dolist (expression expressions)
        ;; All the name hackery is done elsewhere now. 	  
        (define-expression expression new-dgroup))))
  new-dgroup)


;;; Procedures for fetching items from descriptions
;;; ----------------------------------------------------------------------------

(defgeneric fetch-item (expression-form source)
  (:documentation
   "Fetch an expression form from a dgroup or other source of expressions."))
  
(defmethod fetch-item (form (dgroup description))
   "Fetch a dgroup of the given form from the description."
   (let ((result (fetch-entity form dgroup)))
      (cond (result result)
            ((atom form) (find-predicate form (vocabulary dgroup) :create? nil))
            (t (setq result (fetch-expression form dgroup))
              (if result result
                 (find-predicate form (vocabulary dgroup) :create? nil))))))

;;; Help functions for above:
(defmethod fetch-expression (expr-form (dgroup description))
  (find-expression-via-user-form expr-form 
    (expressions dgroup) (vocabulary dgroup)))

(defun find-expression-via-user-form (expr-form expressions vocab)
   ;; the expressions are assumed to come in as a list of alists with the key
   ;; being the predicate of all the expressions in the bin
   ;; also assumes that the expr-form and all expressions are conses with
   ;; an atom in the head position
   (let ((bin (assoc (find-predicate (first expr-form) vocab) expressions)))
      (when bin
         (find expr-form (rest bin) :test #'equal :key #'user-form))))

;;; This version of fetch-expression was needed because it is now possible for
;;; different entities or expressions to have the same user-form, because of
;;; the presence of attribute-values.  Therefore, in this version of the function,
;;; each item in the expr-form tree may be a user-form, an entity, or an expression.
;;; It can therefore be used to find expressions containing a specific entity, 
;;; rather than any entity with the same form.
(defun fetch-expression-flexible (expr-form dgroup &aux (vocab (vocabulary dgroup)))
  ; Ensures that identical expressions containing attribute values are kept
  ;  separate when *unique-attribute-values?* is true
  (when (or *unique-attribute-values?*
            (and (not *_building-attribute-value*)
                 (not (expression-contains-attribute-value? expr-form vocab))))
    (let* ((exprs (expressions dgroup))
           (bin (assoc (find-predicate (first expr-form) vocab) exprs)))
      (when bin 
        (find-if (lambda (expr) (user-forms-equal expr-form expr)) (cdr bin))))))

(defgeneric user-forms-equal (item1 item2))

(defmethod user-forms-equal (item1 item2) 
  (declare (ignore item1 item2)) nil)

(defmethod fetch-entity (entity-name (dgroup description))
  (find entity-name (entities dgroup)
	:test #'equal :key #'user-form))

(defun fetch-entity-unless-isa-value (form description)
  (when (or *unique-attribute-values?*
            (and (not *_building-attribute-value*)
                 (not (is-attribute-value? form (vocabulary description)))))
    (fetch-entity form description)))

;;;------------------------------------------------------------
;;; Manage timestamps and new expressions.
;;;------------------------------------------------------------
;;; (advance-dgroup-timeclock dgroup) 
;;;         -> add a tick to the timestamp, return time.
;;;

(defun advance-dgroup-timeclock (dgroup)
   "Advance the dgroup timeclock by one."
   (declare (type description dgroup))
   (incf (timeclock dgroup)))

(defmethod expressions-since ((dgroup description)(time integer))
   "Return the set of expressions added since last mapping."
   (rebuild-expression-alist-for-expressions-since dgroup time))

(defun rebuild-expression-alist-for-expressions-since (dgroup time)
   (let ((result nil)
         (keepers nil))
      (dolist (bin (expressions dgroup) result)
         (setq keepers (remove-if #'(lambda (expr)
                                      (<= (timestamp expr) time))
                         (rest bin)))
         (when keepers
            (push (cons (first bin) keepers) result)))))

(defmethod entities-since ((dgroup description)(time integer))
   "Return the set of entities added since last mapping."
   (remove-if #'(lambda (entity)
                  (<= (timestamp entity) time))
     (entities dgroup)))

(defmethod items-since ((dgroup description)(time integer))
   "Return the set of all entities or expressions since last mapping."
   (expressions-since dgroup time))


;;;------------------------------------------------------------
;;; Utility routines.
;;;------------------------------------------------------------

;;; (draw-expression-id dgroup)  -> return an ID and increment.
;;; (duplicate dgroup) -> return a duplicate copy of dgroup.

(defmethod draw-expression-id ((dgroup description))
   "Return the next dgroup expression ID and increment ID counter."
   (incf (expr-counter dgroup)))

(defmethod draw-entity-id ((dgroup description))
   "Return the next dgroup entity ID and increment ID counter."
   (incf (entity-counter dgroup)))

(defmethod duplicate ((dgroup description))
   "Duplicate the given description."
   ;; Here we do the simplest possible operation, which is to
   ;; construct a new dgroup whose contents are the same.
   (define-description (name dgroup)
     (mapcar #'lisp-form (entities dgroup))
     (mapcar #'lisp-form (roots dgroup))
     (vocabulary dgroup)
     ""
     ""
     (mapcar #'probability (roots dgroup))))

;;;; Substitution of names within a dgroup definition.
;;; ---------------------------------------------------------------------------
;; The old version of SME supported the following format in its expressions:
;;     (<expression> :name <symbol>)
;; After that expression, whenever one saw <symbol> in any expression, one would 
;;  replace it by <expression>.  These procedures parse this format and carry 
;;  out the substitutions.

(defun find-dgroup-named-expressions (expression-list)
   "Find the named expressions within this list of expression forms."
   (let ((name-alist nil) (expressions nil))
      (dolist (expr-entry expression-list
                (values (nreverse expressions) name-alist))
         (cond ((and (listp expr-entry)
                     (listp (car expr-entry))
                     (eq (cadr expr-entry) :name))
                (push (car expr-entry) expressions)
                (push (cons (third expr-entry) (car expr-entry))
                  name-alist))
               (t (push expr-entry expressions))))))

;; A small theorem: If the substitution list itself is loop-free, then
;; no loops can be created when performing sublis on the expressions which 
;; do not appear as subexpressions of values in the substitution list.
;; Therefore we can "snap" all the name pointers first in the substitution
;; list itself, carefully, and if that succeeds then just use sublis for
;; the rest.
(defun careful-sublis (alist form path)
  (if (null form) nil
    (let ((entry (assoc form alist)))
      (cond (entry
             (if (member form path) ;; loop
               (error "Loop in subsitutions: ~A, ~A, ~A" form path alist))
             (careful-sublis alist (cdr entry) (cons form path)))
            ((not (consp form)) form)
            (t (cons (car form) ;; don't try substituting for the functor
                     (mapcar #'(lambda (arg-form)
                                 (careful-sublis alist arg-form path))
			     (cdr form))))))))

(defun perform-dgroup-name-substitutions (expression-list)
   (multiple-value-bind (expressions name-alist)
       (find-dgroup-named-expressions expression-list)
      (setq name-alist
        (mapcar #'(lambda (entry)
                    (cons (car entry)
                      (careful-sublis name-alist (cdr entry) nil)))
          name-alist))
      (mapcar #'(lambda (expr)
                  (cons (car expr)
                    (sublis name-alist (cdr expr))))
        expressions)))

;;; Dumping dgroups as files
;;; ----------------------------------------------------------------------------
(defgeneric sme-dumper (sme-datum stream)
  (:documentation 
   "Write an SME object to the given stream in a re-readable
    format.  Current works for dgroups and vocabularies."))

(defmethod sme-dumper ((sme-datum description) (stream stream))
   "Write the given dgroup to disk using the given stream."
   (let ((*package* (find-package :common-lisp-user)))
      ;; File header first
      (dump-sme-file-header "Description" stream)
      #+aclpc (let ((time (get-universal-time)))
                 (format stream "~%;; ~/time/ ~/date/" time time))
      ;; Dump the dgroup itself
     (format stream "~%~S ;; End of description"
       (make-load-form sme-datum))))

(defmethod make-load-form ((dgr description) &optional env)
  (declare (ignore env))
  `(sme::defdescription ,(sme::name dgr)
     :documentation ,(doc-string dgr)
     :notes ,(notes dgr)
     :expressions ,(mapcar 'make-load-form (roots dgr))))

;;; Debugging Functions (user routines)
;;; ----------------------------------------------------------------------------
(defun expb# (num &optional (*sme* *sme*))
   "Return the base expression with the given ID number"
   (dolist (expr (get-dgroup-expressions (base *sme*)))
      (if (= (id expr) num)
         (return-from expb# expr))))

(defun expt# (num &optional (*sme* *sme*))
   "Return the target expression with the given ID number"
   (dolist (expr (get-dgroup-expressions (target *sme*)))
      (if (= (id expr) num)
         (return-from expt# expr))))

;;;;; A few more selectors and operators for external systems
;;; ----------------------------------------------------------------------------
(defmethod expression-count ((dgr description))
   "Return the number of expression in the dgroup.  Assumes no deletions."
   (expr-counter dgr)) 

(defmethod entity-count ((dgr description))
   "Return the number of entities in the dgroup.  Assumes no deletions."
   (entity-counter dgr))

(defmethod max-order ((dgr description))
   (reduce #'(lambda (old exp)
               (if (> (order exp) old) (order exp) old))
     (get-dgroup-expressions dgr)
     :initial-value 0))

(defgeneric distinct-functor-count (description)
  (:documentation "Returns the number of distinct functors (predicates) 
     in the description."))

(defmethod distinct-functor-count ((dgr description))
  "Returns the number of distinct functors for a standard SME dgroup"
  (do-distinct-functor-count dgr #'(lambda (x) (declare (ignore x)) t)))

(defmethod distinct-relation-functor-count ((dgr description))
  "Returns the number of distinct relations in a dgroup"
  (do-distinct-functor-count dgr #'(lambda (f) (relation? f (vocabulary dgr)))))

(defun do-distinct-functor-count (dgr test-procedure)
  (let ((functors nil))
    (dolist (exp (get-dgroup-expressions dgr) (length functors))
      (when (funcall test-procedure (functor exp))
        (pushnew (functor exp) functors)))))

(defgeneric root-count (description)
  (:documentation "Returns the number of root expressions in the given description."))

(defmethod root-count ((dgr description))
   (length (roots dgr)))

;;;;;;;;;;;; Normalization of dgroup scores
;; Used to normalize structural evaluation scores of mappings.

(defun normalized-score (mapping)
   (/ (score mapping)
      (let ((sme (sme mapping)))
         (min 
           (maximum-description-structural-evaluation (base sme))
           (maximum-description-structural-evaluation (target sme))))))

(defun overlap-score (mapping)
   (/ (score mapping)
           (let ((sme (sme mapping)))
              (max ;; penalize matches where sizes are very different
                (maximum-description-structural-evaluation (base sme))
                (maximum-description-structural-evaluation (target sme)))))) 

(defun maximum-description-structural-evaluation (dgroup &optional (sme *sme*))
   (clear-dgroup-se-info dgroup)
   (compute-description-local-scores dgroup (mapping-parameters sme))
   (compute-description-trickle-down-scores dgroup (mapping-parameters sme))
   (add-up-description-scores dgroup))

(defun clear-dgroup-se-info (dgroup)
   (dolist (exp (get-dgroup-expressions dgroup))
      (setf (se-marker exp) 0)
      (setf (score exp) 0))
   (dolist (ent (entities dgroup))
      (setf (se-marker ent) 0)
      (setf (score ent) 0)))

(defun compute-description-local-scores (dgroup parameters)
   (do ((queue (copy-list (roots dgroup))
         (nconc new (cdr queue)))
        (entry nil nil)
        (new nil nil))
       ((null queue))
      (setq entry (car queue))
      (cond ((> (se-marker entry) 0)) ;; done already
            (t (setf (score entry) (same-functor parameters))
              (setf (se-marker entry) 1)
              (when (expression? entry)
                 (unless (> (se-marker entry) 0)
                    (push (predicate entry) new))
                 (dolist (argument-pair (arguments entry))
                    (let ((argument (cdr argument-pair)))
                       (unless (> (se-marker argument) 0)
                          (push argument new)))))))))

(defun compute-description-trickle-down-scores (dgroup parameters)
  (with-slots (same-functor same-function max-local-score trickle-down
                            minimal-ascension-multiplier)
     parameters
     (do ((queue (copy-list (roots dgroup))
            (nconc (cdr queue) new)) ;; order critical here!
          (new nil nil)
          (entry nil))
         ((null queue))
        (setq entry (car queue))
        (cond ((> (se-marker entry) 1)) ;; done already  
              (t (setf (se-marker entry) 2)    
                 (when (expression? entry)
                    (when (functor-trickle-down? parameters)
                       (add-evidence (predicate entry)
                                     (* trickle-down (score entry))
                                     max-local-score
                                     minimal-ascension-multiplier)) 
                    (dolist (argument-pair (arguments entry))
                       (let ((argument (cdr argument-pair)))
                          (add-evidence argument
                            (* trickle-down (score entry)) max-local-score)
                          (push argument new)))))))))

(defun add-up-description-scores (dgroup)
   (let ((total-score 0.0))
      (dolist (exp (get-dgroup-expressions dgroup))
         (incf total-score (score exp)))
      (dolist (ent (entities dgroup))
         (setf total-score (incf total-score (score ent))))
      total-score))

(defun get-dgroup-expressions (dgroup)
   "given a dgroup, get a list of it's expressions"
   (do ((bins (expressions dgroup) (rest bins))
           (result nil (append (rest (first bins)) result)))
          ((null bins) result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; More operators over dgroups

(defun map-over-expressions (procedure dgroup)
  (dolist (bucket (expressions dgroup))
    (dolist (exp (cdr bucket))
      (funcall procedure exp))))

(defun map-over-entities (procedure dgroup)
  (dolist (ent (entities dgroup))
    (funcall procedure ent)))

