;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: param.lsp
;;;;    System: SME v3
;;;;   Version: 1.0
;;;;    Author: Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: Sep 17, 1997 by Ron Ferguson
;;;;  $LastChangedDate: 2014-09-21 20:03:34 -0500 (Sun, 21 Sep 2014) $
;;;;  $LastChangedBy: forbus $
;;;;   Purpose: Mapping parameter object for SME.
;;;; --------------------------------------------------------------------------
(in-package :sme) 

;;; Parameters used by by SME.
;;; When an SME is created, the global variables below are the defaults.
;;; Each instance of SME 
;;;
;;; Almost all of the parameters are now fixed.  The only three that
;;; are varied these days are
;;; *defalt-compute-reverse-inferences?*, which is often turned on when looking
;;;    for differences.
;;; *default-greedy-cutoff*, *default-greedy-max-#*: Tightening/loosening constraints
;;;     on # of mappings produced, very rarely done.  
;;; *default-allow-probability*, *default-probability-as-utiilty*: Support ongoing
;;; experiments in integrating probability with analogical processing.  

(defvar *default-same-functor* 0.0005 
  "Local score for MHs with identical functors.")
(defvar *default-greedy-cutoff* 0.8 
  "When performing greedy merge algorithm, gmaps must score at
   least this fraction of the top-scoring gmap's score in order
   to be kept.")
(defvar *default-greedy-max-#* 3 
  "When performing greedy merge algorithm, no more than this number
   of global mappings will be created.")
(defvar *default-same-function* 0.0002
  "Local score for MHs with nonidentical function matches.")
(defvar *default-trickle-down* 8.0 
  "Trickle-down score multiplier.")
(defvar *default-max-local-score* nil
  "The maximum score an individual match hypothesis may be given.")
(defvar *default-functor-trickle-down* nil ;; default no
  "Trickle down to functor MH's?") ;; this is an experiment, default is off.
(defvar *default-minimal-ascension-multiplier* 0.8 
  "Used to reduce the functor score when local alignment is due to minimal ascension")
(defvar *default-allow-out-of-mapping-score-contributions?* t ;; Default is t
  "Determines whether MH's outside the mapping can contribute to the mapping's score")
(defvar *default-block-most-out-of-mapping-contributions?* t ;; Default is nil
  "Determines whether most MH's outside of a mapping are blocked from supporting the mapping")
(defvar *default-greedy-merge-overlap-only?* nil ;;Default is nil
  "An experimental new version of greedy merge which requires kernels to overlap")
(defvar *default-ci-support-scores-global?* nil ;;Default is nil
  "Use global information when computing candidate inference support scores?")
(defvar *default-use-less-greedy-greedy-merge?* t ;;Default is nil
  "Use a modified form of greedy merge allowing addition of 
   consistent kernels from other mappings?")
(defvar *default-enforce-1to1-minimal-ascension?* t 
  "When set to true the 1 to 1 correspondence constraint will be enforced for functors")
(defvar *default-compute-reverse-inferences?* nil
  "Compute CI's from the target to the base?")
(defvar *default-allow-entity-supported-inferences?* t
  "Allow candidate inferences to be computed based on entity matches?
   Very extrapolative, and hence risky, by comparison to demanding more overlap.
   However, it is essential for carryover situations, consequently the default is T.")
(defvar *default-allow-probability?* nil
  "Whether to allow sme to use probability information or not.")
(defvar *default-probability-as-utility?* nil
  "Whether to use probability as utility or as another
  dimention. See SAGE page in qrgwiki for more info.")


;;; The parameters object holds the values in force for a particular comparsion.
;;; Since we sometimes compare against SME runs done over a decade or more ago,
;;; it is important to be able to replicate.
;;;
;;; Parameters are added to SME very, very rarely.  But when one is, it is vital
;;; to update this object, he print-object and lisp-form methods appropriately.
;;; Otherwise the appropriate record-keeping will not be done!

(defclass parameters ()
    ((greedy-cutoff
      :documentation "During greedy merge algorithm, Gmaps
        must score at least this fraction of the top-scoring Gmap's 
        score in order to be kept."
      :accessor greedy-cutoff :initform *default-greedy-cutoff* 
      :initarg :greedy-cutoff :type number)
     (greedy-max-#
      :documentation "For greedy merge, no more than this number of Gmaps
         are created." 
      :accessor greedy-max-# :initform *default-greedy-max-#*
      :initarg :greedy-max-# :type number)
     (same-function
      :documentation "For trickle-down scoring, local score of MHs with 
         nonidentical functors (such as some function matches)."
       :accessor same-function :initform *default-same-function*
       :initarg :same-function :type number)
     (same-functor
       :documentation "For trickle-down scoring, local score of MHs with
         identical functors (most expression matches)."
       :accessor same-functor :initform *default-same-functor*
       :initarg :same-functor :type number)
     (trickle-down
       :documentation "For trickle-down scoring, the trickle-down
          multiplier."
       :accessor trickle-down :initform *default-trickle-down*
       :initarg :trickle-down :type number)
     (max-local-score
       :documentation "For trickle-down scoring, the local maximum
          score of an MH. Nil means there is no maximum."
       :accessor max-local-score :initform *default-max-local-score*
      :initarg :max-local-score :type number)
     (compute-reverse-inferences?
      :accessor compute-reverse-inferences?
      :initform *default-compute-reverse-inferences?*
      :initarg :compute-reverse-inferences?
      :documentation "When t, CI's can be computed from the target to the base.")
     (allow-entity-supported-inferences?
       :accessor allow-entity-supported-inferences? 
       :initform *default-allow-entity-supported-inferences?*
       :initarg :allow-entity-supported-inferences?)
     (functor-trickle-down?
       :accessor functor-trickle-down? 
       :initform *default-functor-trickle-down* :initarg :functor-trickle-down?)
     (minimal-ascension-multiplier
       :accessor minimal-ascension-multiplier 
      :initform *default-minimal-ascension-multiplier*
      :initarg :minimal-ascension-multiplier)
     (allow-out-of-mapping-score-contributions?
      :accessor allow-out-of-mapping-score-contributions?
      :initarg :allow-out-of-mapping-score-contributions?
      :initform *default-allow-out-of-mapping-score-contributions?*
      :documentation "When non-NIL, score of mapping = sum of all mh's in it.")
     (block-most-out-of-mapping-contributions?
      :accessor block-most-out-of-mapping-contributions?
      :initarg :block-most-out-of-mapping-contributions?
      :initform *default-block-most-out-of-mapping-contributions?*
      :documentation "When t, final score of mapping = sum of all mh's in it (as above) + 
                      initial mh score trickle-down limited to mostly structurally consistent parents.")
     (use-less-greedy-greedy-merge?
      :accessor use-less-greedy-greedy-merge?
      :initarg :use-less-greedy-greedy-merge?
      :initform *default-use-less-greedy-greedy-merge?*
      :documentation "When t, use a modified form of greedy merge allowing addition of 
                      consistent kernels from other mappings.")
     (greedy-merge-overlap-only?
      :accessor greedy-merge-overlap-only?
      :initarg :greedy-merge-overlap-only?
      :initform *default-greedy-merge-overlap-only?*
      :documentation "When t, uses an experimental form of greedy merge which requires kernels
                      to have at least one common mh before they can be merged.")
     (enforce-1to1-minimal-ascension?
      :accessor enforce-1to1-minimal-ascension?
      :initform *default-enforce-1to1-minimal-ascension?*
      :initarg :enforce-1to1-minimal-ascension?
      :documentation "When t, functor correspondences are created for relations matched via minimal ascension.")
     (allow-probability?
      :accessor allow-probability?
      :initarg :allow-probability?
      :initform nil
      :documentation "Multiplies an mh's structural score by the prob of the expr's in it.")
     (probability-as-utility?
      :accessor probability-as-utility?
      :initarg :probability-as-utility?
      :initform nil
      :documentation "When t, weights a MH's structural score by base and target probabilities,
                      so that higher-probability expressions contribute more to the score.")
     (ci-support-scores-global?
      :accessor ci-support-scores-global?
      :initarg :ci-support-scores-global?
      :initform nil
      :documentation "Use global mapping info when calculating candidate inference support scores?")
     (unique-attribute-values?
      :accessor unique-attribute-values?
      :initarg :unique-attribute-values?
      :initform *unique-attribute-values?*
      :documentation "Are occurrences of attribute values unique across a dgroup?"))
   (:documentation 
    "This class stores the mapping parameters of a particular SME or
      set of SME objects.  Although the default values are set by 
      a set of globals, once an SME is created its mapping parameters
      may be changed independently from any other SME objects through
      a slot containing an object of this class."))

(defmethod print-object ((object parameters) stream)
   "Print out a very short representation of a parameter set."
   (format stream "<Params: (Mapping: ~D,~D%) (Trickle: ~2F,~2F,~2F,~A,~2F,~A)>"
      (greedy-max-# object) (round (* 100 (greedy-cutoff object)))
     (same-functor object) (same-function object) (trickle-down object)
     (functor-trickle-down? object)
     (minimal-ascension-multiplier object)
     (allow-out-of-mapping-score-contributions? object)))

(defmethod lisp-form ((object parameters))
   "Return a Lisp list giving all the essential elements of the parameters."
   `((:greedy-cutoff ,(greedy-cutoff object))
     (:greedy-max-# ,(greedy-max-# object))
     (:same-function ,(same-function object))
     (:same-functor ,(same-functor object))
     (:trickle-down ,(trickle-down object))
     (:max-local-score ,(max-local-score object))
     (:functor-trickle-down? ,(functor-trickle-down? object))
     (:minimal-ascension-multiplier ,(minimal-ascension-multiplier object))
     (:compute-reverse-inferences?
      ,(compute-reverse-inferences? object))
     (:allow-out-of-mapping-score-contributions?
      ,(allow-out-of-mapping-score-contributions? object))
     (:enforce-1to1-minimal-ascension?
      ,(enforce-1to1-minimal-ascension? object))
     (:allow-entity-supported-inferences? 
      ,(allow-entity-supported-inferences? object))
     (:block-most-out-of-mapping-contributions?
      ,(block-most-out-of-mapping-contributions? object))
     (:use-less-greedy-greedy-merge?
      ,(use-less-greedy-greedy-merge? object))
     (:allow-probability?
      ,(allow-probability? object))
     (:greedy-merge-overlap-only?
      ,(greedy-merge-overlap-only? object))
     (:probability-as-utility?
      ,(probability-as-utility? object))
     (:ci-support-scores-global?
      ,(ci-support-scores-global? object))
     (:unique-attribute-values?
      ,(unique-attribute-values? object))
     ))

(defun mapping-parameters-from-plist (list)
  (apply #'make-instance
         (cons 'parameters (filter-obsolete-parameters list))))

(defparameter *obsolete-sme-parameters* '(:use-limited-greedy-merge?)
  "Parameters that can be found in old dehydrated SMEs, but are no longer used.")

(defun filter-obsolete-parameters (parameter-plist)
  "Some parameters have been eliminated over time, but prior dehydrated SMEs
   still mention them.  So this procedure filters those out."
  (cond ((null parameter-plist) nil)
        ((member (car parameter-plist) *obsolete-sme-parameters*)
         (cddr parameter-plist))
        (t (cons (car parameter-plist)
                 (cons (cadr parameter-plist)
                       (filter-obsolete-parameters (cddr parameter-plist)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Tools for testing older and newer parameter sets

(defun install-sme-1989-journal-defaults ()
  "Sets the SME default params to the values we hope to 
   use for the journal paper."
  (setq *default-max-local-score* nil
    *default-use-less-greedy-greedy-merge?* t
    *default-block-most-out-of-mapping-contributions?* t))

(defmacro with-sme-1989-journal-defaults (&body body)
  `(let ((*default-max-local-score* nil)
         (*default-use-less-greedy-greedy-merge?* t)
         (*default-block-most-out-of-mapping-contributions?* t))
     ,@body))

(defun install-sme-pre-1989-journal-defaults ()
  "Sets the SME default params to the values we used before
   the journal paper."
  (setq *default-max-local-score* 1.0
    *default-use-less-greedy-greedy-merge?* nil
    *default-block-most-out-of-mapping-contributions?* nil))

(defmacro with-sme-pre-1989-journal-defaults (&body body)
  `(let ((*default-max-local-score* 1.0)
         (*default-use-less-greedy-greedy-merge?* nil)
         (*default-block-most-out-of-mapping-contributions?* nil))
     ,@body))

(defun install-sme2e-defaults ()
  "Set the structural evaluation defaults to those of the
   previous version of SME."
  (setq *default-functor-trickle-down* nil
	*default-same-functor* 0.5
	*default-same-function* 0.2
	*default-trickle-down* 32.0))

(defun install-sme3b-defaults ()
  "Reset the structural evaluation defaults to those normally
   used by SME v3."
  (setq *default-functor-trickle-down* nil
	*default-same-functor* 0.005
	*default-same-function* 0.002
	*default-trickle-down* 16.0))
