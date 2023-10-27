;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: sme.lsp
;;;;    System: SME
;;;;   Version: 4.0
;;;;    Author: Ron Ferguson & Ken Forbus
;;;;   Created: Long ago (1992)
;;;;  $LastChangedDate: 2014-07-22 00:18:32 -0500 (Tue, 22 Jul 2014) $
;;;;  $LastChangedBy: liang $
;;;;   Purpose: SME mapping object
;;;; --------------------------------------------------------------------------
(in-package :sme)


;;; The SME object encapsulates the data of a comparison. 

(defclass sme ()
  ((name
     :documentation "The name of this copy of the Structure-Mapping Engine"
     :type string  :accessor name  :initarg :name  :initform "")
   (id :documentation "Integer for sorting SME's"
     :type integer  :reader id :initarg :id)
   (base
     :documentation "The base description to match"
     :type description  :accessor base  :initarg :base)
   (target
     :documentation "The target description"
     :type description  :accessor target  :initarg :target)
   (mhs
     :documentation "The current set of match hypotheses"
     :type list  :accessor mhs)
   (new-mhs
     :documentation "New match hypothesis queue"
     :type list  :accessor new-mhs)
   (root-mhs
     :documentation "The root match hypotheses"
     :type list  :accessor root-mhs)
   (mappings
     :documentation "The set of mappings found so far"
     :type list  :accessor mappings  :initform nil)
   (new-mappings
     :documentation "The newest set of mappings"
     :type list  :accessor new-mappings  :initform nil)
   (kernel-mappings
     :documentation 
     "The mappings derived from the roots.  Can be accessed as either 
kernel-mappings (preferred) or root-mappings."
     :type list  :accessor kernel-mappings :accessor root-mappings
     :initform nil)
   (mappings-cache
     ;; Note: This cache must always contain all the mappings in
     ;;        the SME, because the structural evaluator depends on it.
     :documentation "Cache of mappings generated in this SME."
     :type list  :accessor mappings-cache  :initform nil)
   (mapping-parameters
     :documentation "The mapping parameters used for mapping.  See Parameters class."
     :accessor mapping-parameters 
     :initform (make-instance 'parameters) :initarg :mapping-parameters) 
   (mapping-counter
     :documentation "Unique id for mappings for this SME"
     :initform 0  :accessor mapping-counter)
   (mh-counter
     :documentation "Unique id for match hypothoses for this SME"
     :initform 0  :accessor mh-counter)
   (timeclock
     :documentation
     "Clock for this SME, indicating the passage of interesting events."
     :initform 0  :accessor timeclock)
   (base-timestamp
     :documentation "The last time the base was examined."
     :initform 0  :accessor base-timestamp)
   (target-timestamp
     :documentation "The last time the target was examined."
     :initform 0  :accessor target-timestamp)
   (base-mh-table
     :documentation "The MHs indexed by base item"
     :accessor base-mh-table  :initform (make-hash-table :test 'eq))
   (target-mh-table
     :documentation "The MHs indexed by target item"
     :accessor target-mh-table  :initform (make-hash-table :test 'eq))
   (last-mapped ;; ***** make sure this is updated correctly
     :documentation "When was mapping last done"
     :type integer  :accessor last-mapped  :initform 0)
   (unused-score
     :documentation "Sum of MH's not in current mappings"
     :type float  :accessor unused-score  :initarg :unused-score
    :initform 0.0)
   (match-constraints 
    :type list :accessor match-constraints :initform nil 
    :initarg :match-constraints
    :documentation "The list of constraints as passed into SME")
   (match-filters
    :type list :accessor match-filters :initform nil
    :initarg :match-filters
    :documentation "Filters created based on the list of match constraints.")
   (sought-solutions
    :type list :accessor sought-solutions :initform nil
    :documentation "A list of solutions found based on match-constraints.")
   (version
     :documentation "The version of sme."
     :reader version  :initform *sme-version*)
   (kernel-plateau?
    :documentation
    "True if two or more of the top kernels mappings have same score."
    :accessor kernel-plateau? :initform nil)
   (rerep-opportunities 
    :accessor rerep-opps :initarg :rerep-opps :initform nil)
   (opportunity-counter
    :accessor opp-counter :initarg :opp-counter :type integer :initform 0)
   (rerep-suggestions
    :accessor rerep-sugs :initarg :rerep-sugs :initform nil)
   (debugging
     :documentation "Debugging being done on SME as feature list."
     :type list  :accessor debugging  :initform *default-debugging-features*))
   (:documentation
     "An SME is an object representing a comparison of 
      two descriptions.  An SME can be created using
      define-sme."))

(defmacro sme? (possible-sme)
   `(typep ,possible-sme 'sme))

(defmethod print-object ((sme sme) stream)
   (format stream "<SME ~A>" (id sme)))

(defmethod initialize-instance :after ((instance sme) &rest initargs)
   "Initialize various SME values."
   (declare (ignore initargs))
   (setf (mhs instance) nil)
   (setf (new-mhs instance) nil)
   (setf (root-mhs instance) nil)
   instance)

(defmethod vocabulary ((thing sme)) (vocabulary (base thing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging support
;; 

(defmethod debugging-on? ((sme sme) debugging-feature)
   (member debugging-feature (debugging sme)))

(defun show-sme-parameters (sme &optional (stream *standard-output*))
   "Print out the current mapping paramters for SME."
   (with-slots (allow-entity-supported-inferences? greedy-cutoff greedy-max-#)
     (mapping-parameters sme)
     (format stream "~%Parameters for ~A:" sme)
     (format stream "~%     Max # mappings: ~A" greedy-max-#)
     (format stream "~%     Cutoff %: ~A" greedy-cutoff)
     (format stream "~%  Entity-supported inferences allowed: ~A"
       (if allow-entity-supported-inferences? "Yes" "No"))
     (format stream "~%     Unused score: ~A" (unused-score sme))
     (format stream "~%     # MHs: ~A" (length (mhs sme)))
     (format stream "~%     # mappings: ~A" (length (mappings sme)))
     (when (mappings sme)
        (format stream "~% Mappings:")
        (dolist (m (mappings sme))
           (format stream "~%   ~A: SES = ~A, ~D CIs" m (score m)
             (length (inferences m)))))))

;;; print-debug
;;; ----------------------------------------------------------------------------
(defparameter *sme-debug-stream* *standard-output* "where the debug goes")
(defparameter *sme-debug-flags* nil 
  "a set conditional debugging flags, similar to (debugging <sme>),
  but controlable by macros")

(defmacro with-debug-stream (stream &body body)
   "sets the SME debug stream limited to the scope of the body"
   `(let ((*sme-debug-stream* ,stream))
       ,@body))

(defmacro with-instrumentation (&body body)
   "Turns on instrumentation during the matchs"
   `(let ((*sme-debug-flags* (cons :instrumented *sme-debug-flags*)))
       ,@body))

(defgeneric print-debug (sme fmt-string &rest vars)
  (:documentation "When SME has debugging turned on, treat the
    rest of statement as format print to *standard-out*.  If
    <sme> is omitted, defaults to *sme*."))

(defgeneric print-debug-if (sme conditions fmt-string &rest vars)
  (:documentation "When SME has debugging flags that include on
   of the listed conditions (which is a list of keywords), treat
   the rest of statement as a format print to *standard-out*.  If
   <sme> is omitted, defaults to *sme*."))

(defmethod print-debug ((sme sme) (fmt-string string) &rest vars)
   (when (debugging sme)
      (apply #'format *standard-output* fmt-string vars)))

(defmethod print-debug ((fmt-string string) (var1 t) &rest other-vars)
   "Sneaky method of omitting SME."
   (when (debugging *sme*)
      (apply #'format *standard-output* fmt-string var1 other-vars)))

(defmethod print-debug-if ((sme sme) (conditions list) (fmt string) &rest vars)
   (when (and (listp (debugging sme))
              (intersection conditions (debugging sme)))
      (apply #'format *standard-output* fmt vars)))

(defmethod print-debug-if ((conditions list) (fmt string) (var1 t) &rest 
                           other-vars)
   (when (and (listp (debugging *sme*))
              (intersection conditions (debugging *sme*)))
      (apply #'format *standard-output* fmt var1 other-vars)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setting up an environment for comparison
;;;
;;; The global variable *sme* should always be bound to the SME in use,
;;; so that it doesn't have to be passed around.  The procedure in-sme
;;; sets this global, and the macro with-sme lambda-binds it, so that it
;;; holds within the scope of a computation. 

(defun in-sme (s) (setq *sme* s))

(defmacro with-sme (sme &rest body)
  `(let ((*sme* ,sme)) ,@ body))
    
(defparameter *sme-type* 'sme 
  "the default sme class created when calling define-sme")

;;; Some external systems subclass SME, so that they can redefine
;;; some of its methods with respect to the KR system that is using it.
;;; This macro enables such systems to specify that redefinition is in
;;; force for some context within their code.  

(defmacro with-sme-type (sme-type &body body)
   "Sets the default SME the given class"
   `(let ((*sme-type* ,sme-type))
       ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constructors
;;; (define-sme base target :name :copy-dgroups?) creates an SME.
;;; N.B. The default globals are defined in param, see documentation there.

(defun define-sme (base target 
                        &key name copy-dgroups? 
                        (cutoff *default-greedy-cutoff*)
                        (max-# *default-greedy-max-#*)
                        (compute-reverse-inferences?
                         *default-compute-reverse-inferences?*)
                        (allow-entity-supported-inferences? 
                         *default-allow-entity-supported-inferences?*)
                        (block-most-out-of-mapping-contributions? 
                         *default-block-most-out-of-mapping-contributions?*)
                        (greedy-merge-overlap-only?
                         *default-greedy-merge-overlap-only?*)
                        (allow-probability? *default-allow-probability?*)
                        (probability-as-utility? *default-probability-as-utility?*)
                        (ci-support-scores-global? 
                         *default-ci-support-scores-global?*)
                        (enforce-1to1-minimal-ascension?
                         *default-enforce-1to1-minimal-ascension?*)
                        (use-less-greedy-greedy-merge?
                         *default-use-less-greedy-greedy-merge?*)
                        (sme-type *sme-type*))
  "Define (and create) an SME class object, using the given  
   base and target descriptions.  Sets the value of global variable
   *sme* to the created SME object, and returns that value.  
   If copy-dgroups? is non-nil, the base and target descriptions are 
   duplicated rather than used as-is.  Default values for cutoff and 
   max-# are given in *default-greedy-cutoff* and *default-greedy-max-#*.
   define-sme is usually called to create an SME object suitable for 
   calling either the match or incremental-match mapping routines."
  (declare (type description base target))
  (when copy-dgroups?
    (setq base (duplicate base))
    (setq target (duplicate target)))
   (let* ((sme (make-instance sme-type
                 :base base :target target
                 :id (get-new-sme-id) :name name))
          (parameters (mapping-parameters sme)))
     (setf (greedy-cutoff parameters) cutoff)
     (setf (greedy-max-# parameters) max-#)
     (setf (compute-reverse-inferences? parameters)
       compute-reverse-inferences?)
     (setf (allow-entity-supported-inferences? parameters)
       allow-entity-supported-inferences?)
     (setf (allow-probability? parameters) allow-probability?)
     (setf (probability-as-utility? parameters) probability-as-utility?)
     (setf (block-most-out-of-mapping-contributions? parameters) 
       block-most-out-of-mapping-contributions?)
     (setf (use-less-greedy-greedy-merge? parameters)
       use-less-greedy-greedy-merge?)
     (setf (greedy-merge-overlap-only? parameters) greedy-merge-overlap-only?)
     (setf (ci-support-scores-global? parameters) ci-support-scores-global?)
     (setf (enforce-1to1-minimal-ascension? parameters)
       enforce-1to1-minimal-ascension?)
     (link-base base sme)
     (link-target target sme)
     (in-sme sme)
     sme))

(defun clone-current-sme ()
  ;; For debugging
  (define-sme (base *sme*) (target *sme*)))

(defmethod emaps ((sme sme))
   "Return list of entity-based match hypotheses."
   (remove-if-not 'entity? (mhs sme)))

(defmethod new-emaps ((sme sme))
  "Returns set of new entity MHs."
  (remove-if-not 'entity? (new-mhs sme)))

;;; Linking is used to ensure that the timestamps for the
;;; base and target correspond to that of the SME, so that
;;; only new elements are processed in incremental matching.

(defmethod link-base ((base description)(sme sme))
  (setf (base-timestamp sme) (timeclock sme)))

(defmethod link-target ((target description)(sme sme))
  (setf (target-timestamp sme) (timeclock sme)))

;;;------------------------------------------------------------
;;; Utilities for getting vocabulary and description files.
;;;------------------------------------------------------------
;;; These are used by the tty-oriented interface (see charsme.lsp)
;;; included with the code.

(defun available-vocabulary-files (&optional (path *sme-vocabulary-path*))
  "Returns a list of all vocabulary files available in the directory.
   If the directory is not given, it defaults to *sme-vocabulary-path*."
  (files-fitting path (concatenate 'string "."
				   *sme-vocabulary-extension*)))

(defun available-description-files (&optional (path *sme-description-path*))
  "Returns a list of all description (dgroup) files available in the directory.
   If the directory is not given, it defaults to
   *sme-description-path*."
  (files-fitting path (concatenate 'string "."
				   *sme-description-extension*)))
  

;;; Find a list of files fitting the proper description.
;;;
(defun files-fitting (pathname &rest file-extensions)
  "Create a list of files in the given directory with the given
extension.  The extension should include the period (.), and 
the pathname should end with a pathname separator."
  (mapcan #'(lambda (file-extension)
              (mapcar #'file-namestring
                      (directory (format nil "~A*~A"
                                         pathname file-extension))))
     file-extensions))
