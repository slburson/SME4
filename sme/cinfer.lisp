;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                             -*-
;;;; ------------------------------------------------------------------------------
;;;; File name: cinfer.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Kenneth D. Forbus
;;;;   Created: Long, long ago
;;;;  $LastChangedDate: 2014-09-04 19:31:16 -0500 (Thu, 04 Sep 2014) $
;;;;  $LastChangedBy: forbus $
;;;;   Purpose: Candidate inference generation
;;;; ------------------------------------------------------------------------------


(in-package :sme)

;; The basic algorithm for finding candidate inferences is the same as used in SME 2e,
;; except that in v4, reverse candidate inferences can also be computed on demand,
;; to make finding differences simpler.
;;
;; Candidate inferences are implemented as objects, containing support and 
;; extrapolation scores (as defined in CogSci97 paper) as well as the form of 
;; the inference itself.  As per the theory, candidate inferences may or may not
;; be valid -- that sort of checking is the job of processes that are using SME
;; to perform comparisons.

(defmacro with-entity-supported-inferences (&rest forms)
  ;; A more liberal condition, allowing more information to be carried over.
   `(let ((*default-allow-entity-supported-inferences?* t)) ,@ forms))

;;;; Candidate Inference object

(defclass candidate-inference ()
      ((form :type list
         :documentation "Form of the candidate inference"
         :initarg :form
         :reader form)
       (id :type integer :initarg :id :reader id)
       (mapping :type t
         :documentation "Mapping this candidate inference is from."
         :initarg :mapping
         :reader mapping)
       (reverse? :type t
         :documentation "A flag to indicate whether this is a forward or reverse inference."
         :initarg :reverse?
         :initform nil
         :reader reverse?)
       (mhs :type list
         :documentation "Match hypotheses providing the structural support."
         :initarg :mhs
         :reader mhs)
       ;; The MHs are stored with the inference because they can help determine 
       ;; what mapping to switch to if the current mapping doesn't look 
       ;; appropriate.  Also used in the evaluation computations.
       (support-score :type float
         :documentation "Support evaluation of candidate inference"
         :initform 0.0 :accessor support-score)
       (extrapolation-score :type float
         :documentation "Extrapolation evaluation of CI"
         :initform 0.0 :accessor extrapolation-score)
       (shadow-cache :type t
         :documentation "Shadow structure cache for evaluation"
         :initform nil :accessor ci-shadow-cache)
       (base-item
        :documentation "Source item CI originates from.  
                        This is the base for forward CIs, the target for reverse CIs."
         :type t :initarg :base-item :reader base-item)
       ;; The "base-item" name remains for backward compatibility. 
       ))

(defmethod print-object ((ci candidate-inference) (stream t))
  (format stream "<SME:ci ~A,~A>" (id ci)(form ci)))

;; Support for the candidate inference data abstraction

(defmethod sme ((ci candidate-inference))
   (sme (mapping ci)))

(defmethod mapping-parameters ((ci candidate-inference))
   (mapping-parameters (sme ci)))

(defmethod candidate-inference? ((ci t)) nil)

(defmethod candidate-inference? ((ci candidate-inference)) t)

;;;; Entry point for candidate inference generation

(defmethod calculate-mapping-inferences ((map mapping) &aux infs reverse-infs)
  "Find and return inferences (as list)."
  ;; Dispatch according to the SME to allow alternate strategies.
  (setq infs (find-mapping-inferences (sme map) map :reverse? nil))
  (setf (inferences map) infs)
  (when (compute-reverse-inferences?
         (mapping-parameters (sme map)))
    (setq reverse-infs 
          (find-mapping-inferences (sme map) map :reverse? t))
    (setf (reverse-inferences map) reverse-infs))
  (append infs reverse-infs))

;;; Preserved for backwards compatibility:
(defmethod calculate-mapping-inferences-bidirectional ((map mapping) &aux infs reverse-infs)
  "Find and return inferences (as list)."
  ;; Dispatch according to the SME to allow alternate strategies.
  (setq infs (find-mapping-inferences (sme map) map :reverse? nil))
  (setq reverse-infs (find-mapping-inferences (sme map) map :reverse? t))
  (setf (inferences map) infs)
  (setf (reverse-inferences map) reverse-infs)
  (append infs reverse-infs))

(defmethod find-mapping-inferences ((sme sme) (map mapping) &key (reverse? nil))
   "Finds the candidate inferences for a mapping"
   ;; This version creates structures for CI's, and stores with them 
   ;; information about what MH's each inference depends upon.  This information
   ;; should be useful in, for instance, decisions about what matches might be
   ;; reasonable next candidates if contemplating a switch.
  (let ((source-roots (top-level-expressions (if reverse? (target sme) (base sme))))
        (which (if reverse? 'target-item 'base-item))
        (infs nil))
    (dolist (source-root source-roots)
      (unless (ubiquitous-predicate-in? (functor source-root)
                (vocabulary (base sme)))
          ;; Don't infer top-level ubiquitous predicates
          (when (and (not (source-root-included? source-root map which))
                     (source-root-intersects? source-root map sme which))
             (multiple-value-bind (form mhs)  
                 (try-for-ci source-root map nil which)
                (when form
                   (push (make-instance 'candidate-inference
                           :form form
                           :id (incf (ci-counter map))
                           :reverse? reverse?
                           :mhs mhs
                           :mapping map
                           :base-item source-root)
                     infs))))))
     (dolist (inf infs) (structurally-evaluate-candidate-inference inf))
     infs))

(defun source-root-included? (source-root mapping &optional (which 'base-item))
   "True if source root expression is mapped in given mapping."
   (declare (type expression source-root) (type mapping mapping))
   (find source-root (mhs mapping) :key which))

(defun source-root-intersects? (source-root mapping sme &optional (which 'base-item))
  "True if source root expression contains subexpressions which 
   are mapped in the given mapping."
  (declare (type expression source-root) (type mapping mapping) (type sme sme))
  (if (allow-entity-supported-inferences? (mapping-parameters sme))
    (some #'(lambda (mh) 
              (mh-intersects-expr-or-entity? mh source-root which))
          (mhs mapping))
    (some #'(lambda (mh) 
              (mh-intersects-expr-only? mh source-root which))
          (mhs mapping))))
        
(defun mh-intersects-expr-or-entity? (mh source-root &optional (which 'base-item))
  (let ((source-item (funcall which mh)))
    (and (not (predicate? source-item))
         (member source-root (roots source-item) :test #'eq))))

(defun mh-intersects-expr-only? (mh source-root &optional (which 'base-item))
  (let ((source-item (funcall which mh)))
    (when (not (or (predicate? source-item) (entity? source-item)))
      (member source-root (roots source-item) :test #'eq))))

;;;; Testing to see if a particular source item should be a candidate inference
;;; "base-item" -> "source-item", search uses "which" being 'base-item or 'target-item
(defun try-for-ci (source-item map mhs which &aux ci)
   "Given a particular source item in a mapping, generates
a single candidate inference carrying that base item over to the target. (or
vice-versa). Returns two values: the list form of the candidate inference and
the MHs involved(?).  Always succeeds."
  (declare (type mapping map))
  ;; Recurses down base-item, looking for grounding.  Keeps track of
  ;; what MH's are used in the candidate inference.
  (let* ((mh (find source-item (mhs map) :key which))
         (corresponding-item (opposite-item-fn which))
         (corr-plus-mhs (when mh (list (funcall corresponding-item mh)
                                         (adjoin mh mhs)))))
    (cond ((entity? source-item)
           (if mh (values-list corr-plus-mhs)
             (values (list ':skolem source-item) mhs)))
          ((predicate? source-item) 
           (if mh (values-list corr-plus-mhs) ;; non-identical?
             (find-non-identical-predicate source-item map which mhs)))
          (mh (values-list corr-plus-mhs))
          (t ;; something new
           (multiple-value-bind (predicate-ci new-mhs)
               (try-for-ci (predicate source-item) map mhs which)
             (setq ci (list predicate-ci))
             (dolist (arg-entry (arguments source-item))
               (multiple-value-bind (arg-ci more-mhs)
                   (try-for-ci (cdr arg-entry) map new-mhs which)
                 (push arg-ci ci)
                 (setq new-mhs more-mhs)))
             (setq ci (nreverse ci))
             (values ci new-mhs))))))

(defun opposite-item-fn (item-fn)
  (if (eq item-fn 'base-item) 'target-item 'base-item))

;;; Support metamappings by looking for correspondences that treat predicates as entities.
;;; For more on metamapping, see Hinrichs, T. and Forbus, K. (2011). 
;;; Transfer Learning Through Analogy in Games. AI Magazine, 32(1), 72-83. 
(defun find-non-identical-predicate (item map key-fn mhs)
  (let ((mh (find (lisp-form item) (mhs map)
                  :key #'(lambda (mh) (lisp-form (funcall key-fn mh))))))
    (if mh
      (values (funcall (opposite-item-fn key-fn) mh) (adjoin mh mhs))
      (values item mhs))))
           
