;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: dehydrate.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ken Forbus
;;;;   Created: May 6, 2001 20:15:24
;;;;   Purpose: Produces a text file that can be used to reconstruct an SME remotely
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2015-09-25 17:03:33 -0500 (Fri, 25 Sep 2015) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;;; Dehydrating an SME is akin to serializing it, except that we don't try to store everything
;;;; about it.  For instance, we do not track incremental changes to case contents.
;;;; (Unless we start storing a timestamped sequence of operations along with the SME, 
;;;; this would be impossible anyway.)
;;;; The idea is to facilitate remote debugging by creating a file which will automatically
;;;; reload the current SME. This has proven useful when trying to help groups that have
;;;; embedded SME into their own code, e.g. SRI's SHAKEN knowledge capture system.

;;;; Since SME code has evolved over time, older dehydrated SMEs are unlikely to be perfectly
;;;; reproduced.  And, as noted above, incrementally extended mappings will not be reproduced
;;;; perfectly.  

(defun dehydrate-sme (sme output-file &key (incremental nil))
  (with-open-file (fout output-file :direction :output
                        :if-does-not-exist :create
                        :if-exists :supersede)
    (dehydrate-sme->stream sme fout :incremental incremental)))

(defun dehydrate-sme->stream (sme outstr &key (incremental nil))
  (let ((base-name "the-base")
        (target-name "the-target")
        (sme-name "the-sme")
        (param-name "the-mapping-params")
        (constraint-name "the-match-constraints"))
    ;; Need to tweak the print parameters to get everything
    (let ((*print-length* nil)
          (*print-level* nil)
          (*print-lines* nil)
          (*package* (find-package :cl-user))
          (*print-right-margin* 10000.))
      (with-sme sme
        (dump-sme-file-header "Dehydration" outstr)
        (format outstr "~%(in-package :data)~%")
        (dehydrate-vocabulary-info sme outstr)
        
        (cond (incremental
               (dehydrate-sme-incrementally
                base-name target-name sme-name param-name 
                (base sme) (target sme) sme outstr constraint-name))
              (t
               (dehydrate-dgroup base-name (base sme) sme outstr)
               (dehydrate-dgroup target-name (target sme) sme outstr)
               (dehydrate-match-constraints sme constraint-name outstr)
               (dehydrate-matcher sme-name param-name sme outstr)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Many systems using the sme are not setting the value of sme:*vocabulary*
;;While using the analogy-source, there is no vocabulary object
;;possibly avoid using sme:*vocabulary*, but for now use it if it is a vocabulary.

(defclass rehydrated-vocabulary (vocabulary)
  ((minimal-ascension-info :type t :initform nil
                           :initarg :minimal-ascension-info
                           :accessor minimal-ascension-info)))

(defmethod minimal-ascension-satisfied? ((base-functor t)
                                         (target-functor t)
                                         (vocabulary rehydrated-vocabulary))
  (break "~%Expected predicates in minimal-ascension-satisfied?: ~s, ~s"
         base-functor target-functor))

(defmethod minimal-ascension-satisfied? ((base-functor predicate)
                                         (target-functor predicate)
                                         (vocabulary rehydrated-vocabulary))
  (let ((bname (name base-functor))
        (tname (name target-functor)))
    (dolist (entry (minimal-ascension-info vocabulary))
      (if (or (and (equal bname (car entry))
                   (equal tname (cadr entry)))
              (and (equal bname (cadr entry))
                   (equal tname (car entry))))
        (return-from minimal-ascension-satisfied?
          (values t (cons :minimal-ascension (cddr entry))))))))

(defun dehydrate-vocabulary-info (sme fout &aux (so-far nil))
  ;; Setup the vocabulary object itself
  (format fout 
      "~%(setq *vocab* (sme::create-empty-vocabulary \"test\" '~S))"
    'rehydrated-vocabulary)
  (format fout "~%(sme::in-vocabulary *vocab*)")
  ;; Go through base and target, finding all of the functors
  (dolist (bexp (expressions (base sme)))
    ;; bexp = (<pred> . <statements with it as functor.>)
    (unless (member (car bexp) so-far) 
      (dehydrate-vocabulary-item (car bexp) fout)
      (push (car bexp) so-far)))
  (dolist (texp (expressions (target sme)))
    ;; texp = (<pred> . <statements with it as functor.>)
    (unless (member (car texp) so-far :test 'equal) 
      (dehydrate-vocabulary-item (car texp) fout)
      (push (car texp) so-far)))
  ;; Figure out which functors are ubiquitous, and make them so.
  (with-vocabulary (vocabulary (base sme))
    (dolist (pred so-far)
      (when (ubiquitous-predicate-in? pred
              (vocabulary (base sme)))
        (format fout "~%(sme::defubiquitous-predicate ~A)"
          (case-preservation-wrap-exp (name pred)))))
    (let ((minimal-ascension-constraints nil))
      (dolist (bpred so-far)
        (dolist (tpred so-far)
          (unless (or (equal bpred tpred)
                      (< (id tpred) (id bpred)))
            (multiple-value-bind (keep? info)
                (minimal-ascension-satisfied? bpred tpred *vocabulary*)
              (when keep?
                ;; info = (:minimal-ascension <superordinate> <depth> <reason>)
                ;; reformat - (<bname> <tname> <superordinate> <depth> <reason>)
                (push (list (case-preservation-wrap-exp (name bpred)) 
                            (case-preservation-wrap-exp (name tpred))
                            (case-preservation-wrap-exp (second info))
                            (third info) 
                            (fourth info)
                            (fifth info))
                      minimal-ascension-constraints))))))
        (format fout "~%(setf (sme::minimal-ascension-info *vocab*) '~A)"
          minimal-ascension-constraints))))

;;; This seems totally bogus now, but keep it for backwards compatibility"
(defvar *case-preservation-wrap-switch* t 
  "Controls whether or not to wrap case-sensitive symbols in vertical bars.")

(defun case-preservation-wrap-exp (exp)
  ;; Make this a no-op for now, just to see if this is the source of the problem
  exp)

(defun dehydrate-vocabulary-item (pred fout)
  (format fout
      "~%(sme::rehydratePredicate ~A ~S ~S :n-ary? ~S :role-relation-pos ~S :commutative? ~S :symmetric? ~S)"
    (case-preservation-wrap-exp (name pred)) (arguments pred) (pred-type pred)
    (n-ary? pred) (role-relation-pos pred) (not (null (commutative? pred))) (not (null (symmetric? pred)))))

(defun dehydrate-dgroup (name dgroup sme fout)
  (let ((*unique-attribute-values?* (unique-attribute-values?
                                     (mapping-parameters sme)))
         (probabilities? (allow-probability? (mapping-parameters sme))))
    (dehydrate-dgroup-entities-and-expressions name (reverse (entities dgroup)) 
      (reverse (top-level-expressions dgroup)) (name dgroup) fout probabilities?)))
  
(defun dehydrate-dgroup-entities-and-expressions (name entities expressions internal-name 
                                                       fout &optional probabilities?)
  (format fout "~%(let ((*unique-attribute-values?* ~S))
                        (setq ~A (sme:defdescription ~S"
    *unique-attribute-values?* name internal-name)
  (format fout "~% entities ~A" (mapcar 'entity-name entities))
  (format fout "~% expressions ~A" (mapcar 'lisp-form-wo-isas expressions))
  (if probabilities?
    (format fout "~% probabilities ~A" (mapcar #'probability expressions)))
  (format fout "~% )))"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Incremental version

(defun dehydrate-sme-incrementally (b-name t-name sme-name param-name base target
                                           sme fout constraint-name)
  (let ((old-roots nil) (new-base-roots nil) (new-target-roots nil))
    (multiple-value-setq (old-roots new-base-roots) 
      (separate-roots (base sme) (base-timestamp sme)))
    (dehydrate-dgroup-entities-and-expressions b-name (entities base) old-roots
      (name base) fout
      (allow-probability? (mapping-parameters sme)))
    
    (multiple-value-setq (old-roots new-target-roots) 
      (separate-roots (target sme) (target-timestamp sme)))
    (dehydrate-dgroup-entities-and-expressions t-name (entities target) old-roots
      (name target) fout
      (allow-probability? (mapping-parameters sme)))
    
    (dehydrate-match-constraints sme constraint-name fout)
    (dehydrate-matcher sme-name param-name sme fout)
    (dehydrate-incremental-match sme-name new-base-roots new-target-roots fout)))

(defun dehydrate-incremental-match (sme-name new-base-roots new-target-roots fout)
  (format fout " ~%(sme::match ~A) " sme-name)
  (dolist (br new-base-roots)
    (format fout "~%(sme::define-expression '~A (sme::base ~A)) " 
      (lisp-form-wo-isas br)
      sme-name))
  (dolist (tr new-target-roots)
    (format fout "~%(sme::define-expression '~A (sme::target ~A)) " 
      (lisp-form-wo-isas tr)
      sme-name))
  (format fout "~%~A" sme-name))

(defun separate-roots (dgroup time)
  (let ((old nil) (new nil))
    (dolist (root (roots dgroup))
      (if (> (timestamp root)
             time)
        (push root new)
        (push root old)))
    (values old new)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun entity-name (exp)
  (case-preservation-wrap-exp (name exp)))

(defun lisp-form-wo-isas (exp)
  (isas->attributes (lisp-form exp)))
 
(defun isas->attributes (thing)
  (cond ((null thing) nil)
        ((not (listp thing)) (case-preservation-wrap-exp thing))
        ((eq (car thing) 'data::isa) (list (case-preservation-wrap-exp (third thing))
                                           (case-preservation-wrap-exp (cadr thing))))
        (t (cons (isas->attributes (car thing))
                 (isas->attributes (cdr thing))))))

(defun intern-all-symbols (form)
  (cond ((null form) nil)
        ((symbolp form)
         (if (symbol-package form) form
           (intern form (find-package :cl-user))))
        ((not (listp form)) form)
        (t (cons (intern-all-symbols (car form))
                 (intern-all-symbols (cdr form))))))


;;no more seeker sme, match-constraints are in the base sme itself
;;;(defmethod dehydrate-match-constraints (sme name fout)
;;;  (declare (ignore sme name fout))
;;;  :documentation "For a generic sme do nothing since required-cps only
;;;     exist in the seeker-sme")
;;;
;;;(defmethod dehydrate-match-constraints ((sme seeker-sme) name fout)
;;;  (format fout "~%(setq ~A `~S)" name (match-constraints sme)))

(defmethod dehydrate-match-constraints ((sme sme) name fout)
  (format fout "~%(setq ~A '~S)" name (match-constraints sme)))

(defun dehydrate-matcher (sme-name param-name sme fout)
  (format fout "~%(setq ~A (sme:define-sme the-base the-target :sme-type '~S))"
    sme-name (type-of sme))
  (format fout "~%(setq ~A (sme::mapping-parameters-from-plist '~S))" 
    param-name (reduce #'append (lisp-form (mapping-parameters sme))))
  (format fout "~%(setf (sme::mapping-parameters ~A) ~A)"
    sme-name param-name)
  ;; have to handle the SME form more carefully, since it can be HUGE and blow stack
  (format fout "~%(setq the-results '((:sme ~S (~S ~S)"
    (version sme) (name (base sme)) (name (target sme)))
  (format fout "~%    (:base ~S) (:target ~S)"
    (name (base sme)) (name (target sme)))
  (format fout "~%    (:matches ")
  (dolist (mh (mhs sme))
    (format fout "~%      ~S" (intern-all-symbols (isas->attributes (lisp-form mh)))))
  (format fout ")~% (:mappings ")
  (dolist (m (mappings sme)) ;; Might have to decompose this one, too
    (format fout "~%       ~S" (intern-all-symbols (isas->attributes (lisp-form m)))))
  (format fout "))))"))
 
(defun compare-sme-mh-results (r1 r2)
  (let ((s1 (cdr (assoc :matches (cdddar r1))))
        (s2 (cdr (assoc :matches (cdddar r2)))))
    (values 
     (set-difference s1 s2 :test 'equal :key 'butlast)
     (set-difference s2 s1 :test 'equal :key 'butlast))))

(defun compare-sme-mapping-results (r1 r2)
  (let ((s1 (cdr (assoc  :mappings (cdddar r1))))
        (s2 (cdr (assoc :mappings (cdddar r2)))))
    (values 
     (set-difference s1 s2 :test 'basically-same-mapping?)
     (set-difference s2 s1 :test 'basically-same-mapping?))))

(defun compare-sme-kernel-mapping-results (sme1 sme2)
  ;; sme1, sme2 are SME objects
  (let ((s1 (mapcar 'lisp-form (kernel-mappings sme1)))
        (s2 (mapcar 'lisp-form (kernel-mappings sme2))))
    (set-exclusive-or s1 s2 :test 'basically-same-mapping?)))

(defun compare-rehydration-results ()
  (declare (special cl-user::the-results cl-user::the-sme))
  ;; Assumes match has been run already, and is the current SME.
  (let ((dry (cdr (assoc :mappings (cdddar cl-user::the-results))))
        (wet (mapcar 'sme::lisp-form (sme::mappings cl-user::the-sme))))
    (list (set-difference dry wet :test 'basically-same-mapping?)
          (set-difference wet dry :test 'basically-same-mapping?))))

(defun basically-same-mapping? (m1 m2)
  ;; M1, M2 are sme lisp forms of mappings
  ;; Basically the same if same MH's and same inferences
  ;; N.B. we ignore scores, since normalization schemes have
  ;; changed over time
  (and (mhs-basically-same? (fourth m1) (fourth m2))
       (inferences-basically-same? (sixth m1) (sixth m2))
       (inferences-basically-same? (eighth m1) (eighth m2))
       (equal (third m1) (third m2))))

(defvar *mhs-comparison-fuzz* 4)
(defvar *mhs-big-enough-for-fuzz* 10)

(defun mhs-basically-same? (mhs1 mhs2)
  ;; args are list of lisp-form match hypotheses
  (let ((diffs (set-exclusive-or
                mhs1 mhs2 :test 'basically-equal-mh?
                :key 'butlast)))
    (cond ((and (> (length mhs1) *mhs-big-enough-for-fuzz*)
                (> (length mhs2) *mhs-big-enough-for-fuzz*))
           (< (length diffs) *mhs-comparison-fuzz*))
          (t (not diffs)))))

(defun basically-equal-mh? (mh1 mh2) ;; s expression versions
  (or (equal-mhs-sans-score mh1 mh2)
      (and (equal (car mh1) :match-c)
           (equal (car mh2) :match-c)
           (equal (second mh1) (second mh2))
           (equal (third mh1) (third mh2))
           (equal (fourth mh1) (fourth mh2))
           (not (set-exclusive-or (fifth mh1) (fifth mh2)
                                  :test 'equal)))))

(defun equal-mhs-sans-score (mh1 mh2)
  (and (listp mh1) (listp mh2)
       (equal (car mh1) :match)
       (equal (car mh2) :match)
       (equal (second mh1) (second mh2))
       (equal (third mh1) (third mh2))))

(defun inferences-basically-same? (inf1 inf2)
  (not (set-exclusive-or inf1 inf2 :test 'equal :key 'car)))

(defun sort-into-numerical-alist (list-of-numbers)
  (let ((alist nil))
    (dolist (num list-of-numbers (sort alist #'> :key 'car))
      (let ((entry (assoc num alist :test '=)))
        (unless entry
          (setq entry (cons num 0))
          (push entry alist))
        (incf (cdr entry))))))


(defun check-dehydrations (path &optional (type "sme"))
  (declare (special cl-user::the-sme
                    cl-user::the-match-constraints
                    cl-user::the-bugs))
  (dolist (dehydration-file (directory 
                             (concatenate 'string path "*." type)))
    (load dehydration-file)
    (match-with-appropriate-filters cl-user::the-sme cl-user::the-match-constraints)
    (let ((check (compare-rehydration-results)))
      (unless (equal check '(nil nil))
        (setq cl-user::the-bugs check)
        (pprint check)
        (break (format nil "(~D ~D)" (length (car check))
                 (length (cadr check)))
               t)))))



;;;; ---------------------------------------------------------------------------
;;; END OF CODE
