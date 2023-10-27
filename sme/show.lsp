;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: show.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: Somewhere in the mists of time
;;;;  $LastChangedDate: 2014-09-04 19:31:33 -0500 (Thu, 04 Sep 2014) $
;;;;  $LastChangedBy: forbus $
;;;;   Purpose: Display routines for SME native classes
;;;; --------------------------------------------------------------------------
(in-package :sme)

;;; GLOBAL VARIABLES
;;; ----------------------------------------------------------------------------
;; Variables used by this routine defined elsewhere.

(defvar *column-increment* 8 "Used for tabbing?")

;; PC ACL lacks *print-right-margin*, so we declare it here.  
;; This fix is only partial--it doesn't affect pretty-printing.
#+aclpc (defvar *print-right-margin* 80)

;; Others don't seem to bind it.
(when (not (boundp '*print-right-margin*))
   (setq *print-right-margin* nil))

(defvar *acl-scratch-stream* (make-string-output-stream)
  "Scratch stream used for some pretty printing functions.")

(defun get-acl-scratch-stream () 
   (unless (and *acl-scratch-stream* 
                (open-stream-p *acl-scratch-stream*))
      (setf *acl-scratch-stream* (make-string-output-stream)))
   *acl-scratch-stream*)

;;; Main Procedures
;;; ----------------------------------------------------------------------------
;;; (show <sme-thing>) gives a quick summary.
;;; (show-string <sme-thing>) uses a different set of routines based on strings
;;;                            which allows different effects (e.g. side-by-side printing).
;;; (lisp-form <sme-thing>) returns a lisp version of the sme object.

;;; show methods
;;;-------------------------------------------------------------
(defgeneric show (object &optional stream)
  (:documentation "Provide quick summary description of SME object"))

(defmethod show ((sme sme) &optional (stream *standard-output*))
  "Summary of SME statistics"
   (with-slots (name base target mhs mappings timeclock) sme
     (format stream "~%SME ~A:" name)
     (format stream "~%  Base: ~A" base)
     (format stream "~%  Target: ~A" target)
     (format stream "~%  #MH's: ~D; #Mappings: ~D" (length mhs)
        (length mappings))
     (format stream "~%  Clock: ~D" timeclock))
   sme)

(defmethod show ((mh match-hypothesis) &optional (stream t))
  "Short description of match hypothesis"
   (format stream "~%MH ~A:" (name mh))
   (format stream "~%  Base item: ~A" (base-item mh))
   (format stream "~%  Target item: ~A" (target-item mh))
   (format stream "~%  Order: ~D; Incomplete?: ~A; Inconsistent?: ~A; Score: ~D."
     (order mh) (incomplete? mh) (inconsistent? mh) (score mh))
   ;; For debugging
  (format stream "~%  #nogoods: ~D; #descendants: ~D."
    (if (listp (nogoods mh)) (length (nogoods mh)) (set-size (nogoods mh)))
    (if (listp (descendants mh)) (length (descendants mh)) (set-size (descendants mh))))
  mh)

(defmethod show ((m mapping) &optional (stream t))
  "Short description of a mapping"
  (format stream "~%Mapping ~A:" (id m))
  (format stream "~%Score = ~D, ~D inferences, ~D reverse inferences."
    (score m) (length (inferences m)) (length (reverse-inferences m)))
  (format stream "~%Roots:")
  (dolist (root (root-mhs m))
    (show root stream)))  

(defmethod show ((vocabulary vocabulary) &optional (stream *standard-output*))
  "A brief description of a vocabulary"
  (format stream "~%Vocabulary ~A:" (name vocabulary))
  (format stream "~%  #Predicates: ~D." (length (predicates vocabulary)))
  vocabulary)

(defmethod show ((dgroup description) &optional (stream *standard-output*))
   "A brief description of a dgroup's contents."
   (format stream "~%Description ~A:" (name dgroup))
   (format stream "~%  #Entities: ~D; #Expressions: ~D; Timestamp: ~D."
     (length (entities dgroup)) (length (get-dgroup-expressions dgroup))
     (timeclock dgroup))
   (format stream "~% Vocabulary: ~A."
     (name (vocabulary dgroup)))
   dgroup)

(defmethod show ((pred predicate) &optional (stream *standard-output*)) 
   "Describe a single predicate."
   (format stream "~% Predicate ~A:" (name pred))
   (format stream
     "~%  Type: ~A; Commutative?: ~A; N-ary?: ~A; Set?: ~A; Script?: ~A."
     (pred-type pred) (commutative? pred) (n-ary? pred)
     (set? pred) (script? pred))
   (format stream "~% arguments: ~A."
     (arguments pred))
   (format stream "~%  Vocabulary: ~A." (vocabulary pred))
   pred)

(defmethod show ((expr expression) &optional (stream *standard-output*))
   "Describe a single expression."
   (format stream "~% Expression ~A:" (name expr))
   (format stream "~%  ID: ~A; Predicate: ~A; Order: ~A."
     (id expr) (predicate expr) (order expr))
   (format stream "~%  Args: ~A." (arguments expr))
   expr)

(defmethod show ((entity entity) &optional (stream *standard-output*)) 
   "Describe a single entity."
   (format stream "~% Entity ~A:" (name entity))
   (format stream "~%  Description: ~A; Timestamp: ~D."
     (description entity) (timestamp entity))
   entity)

;;; show-string generic function
;;; ----------------------------------------------------------------------------
(defgeneric show-string (sme-object &optional stream &key)
  (:documentation
   "A key display method for SME.  Almost all sme class
objects -- dgroups, match hypotheses, etc., are printout
out in a sensible way when passed to this routine."))

(defmethod show-string ((sme sme) &optional (stream *standard-output*)
               &key (kernels-only? nil) (ci-info :show))
  (dolist (mapping (mappings sme))
    (show-string mapping stream :kernels-only? kernels-only?
      :ci-info ci-info)))

(defmethod show-string ((dgroup description)
               &optional (stream *standard-output*) &key)
   (let ((*print-right-margin* (if (null *print-right-margin*) 65
                                 (round (* *print-right-margin* 0.7)))))
      (format stream "~A~%" (name dgroup))
      #-aclpc (format stream "~A"
                (string-predicatize
                  (write-to-string (lisp-form dgroup) :pretty t)))
      #+aclpc (pprint-expression-list (cdr (lisp-form dgroup)) stream)))

(defmethod show-string ((mh match-hypothesis) &optional (stream *standard-output*) &key)
  "Display match hypothesis to stream in two columns."
   (dpprint (lisp-form (base-item mh))
     (lisp-form (target-item mh)) stream))

(defmethod show-string ((mh commutative-match-hypothesis)
               &optional (stream *standard-output*) &key)
  "Display commutative match hypothesis to stream in two columns."
  (dpprint (lisp-form (base-item mh))
        (lisp-form (target-item mh)) stream)
  mh)

(defmethod show-string ((mapping mapping) 
               &optional (stream *standard-output*)
               &key (kernels-only? nil) (ci-info :show))
  "Display mapping to stream in two columns."
   (format stream "~%;; Mapping ~A: Score ~7,4F~%"
     (id mapping) (score mapping))
   (cond (kernels-only?
           (dolist (mh (sort (copy-list (root-mhs mapping)) #'order>))
              (show-string mh stream)))
         (t      
           (dolist (mh (sort (copy-list (mhs mapping)) #'order>))
              (show-string mh stream))))
   (terpri stream)
   (format stream "~%Inferences:")
   (with-slots (inferences) mapping
    (cond ((null inferences)
           (format stream " None."))
          ((eql ci-info :count)
           (format stream " ~D." (length inferences)))
          (t
            (dolist (inference inferences)
               (show-string inference stream :ci-info ci-info)))))
   (terpri stream))

(defmethod show-string ((object-list list) &optional (stream *standard-output*)
                        &key)
   (dolist (object object-list)
      (show-string object stream)))

(defmethod show-string ((ci candidate-inference) 
                        &optional (stream *standard-output*) 
                        &key (ci-info :show))
   "Print out information about the candidate inferences.
    For CI-INFO keyword, :SHOW means just print inferences,
    and :DETAILS gives the support and extrapolation scores."
   (case ci-info
     ((:show :details) 
      #+aclpc
      (pprint-expression-list (list (lisp-form ci)) stream)
      #-aclpc
      (pprint (lisp-form ci) stream))
     (t (error "Unsupported CI-INFO keyword: ~A" ci-info)))
   (when (eql ci-info :details)
      (format stream "~%Support = ~D; Extrapolation = ~D."
        (support-score ci) (extrapolation-score ci))))
     
(defmethod show-mhs ((sme sme) &key (ordering #'mh<))
  (format *standard-output* "~% ~D Match Hypotheses:"
       (length (mhs sme)))
  (mapc #'show-string (sort (copy-list (mhs sme)) ordering)))

#+aclpc
(defun pprint-expression-list (exprs ostream)
   "Print the set of expressions in sensible format"
   (let ((sstream (get-acl-scratch-stream))
         (strlist nil))
      (cg:set-right-margin sstream *print-right-margin*)
      (dolist (expr exprs)
         (pprint expr sstream)
         (setq strlist 
           (remove-null-strings 
             (listify-string 
               (string-predicatize (get-output-stream-string sstream)))))
         (dolist (str strlist) 
            (format ostream "~%~A" str)))))

#+aclpc
(defun pprint-expression-list-strings (expr)
   "Print the set of expressions in sensible format"
   (let ((sstream (get-acl-scratch-stream))
         (strlist nil))
      (cg:set-right-margin sstream *print-right-margin*)
      (pprint expr sstream)
      (remove-null-strings 
        (listify-string 
          (string-predicatize 
            (get-output-stream-string sstream))))))


(defun show-mappings (&optional (sme *sme*))
  (let ((*package* (find-package :cl-user)))
    (dolist (m (mappings sme) (mappings sme))
      (show-string m))))

;;; Utility Routines Used By the show-string Routines.
;;; ----------------------------------------------------------------------------
;; Attempted to use indirect addressing on format string--could
;;  not get it working.  Thus took a longer approach.

(defun double-print (string1 string2 tabpos ostream &aux fstring)
  (setq string1 (or string1 "")) ;; correct for nulls.
  (setq string2 (or string2 ""))
  (setq fstring (format nil "~~%~~A ~~~D,~DT || ~~A" tabpos *column-increment*))
  (format ostream fstring string1 string2))

(defun double-print-list (slist1 slist2 &optional (ostream t))
  (let ((tabpos (max (+ 2 (max-slength slist1)) 
          (if (null *print-right-margin*)
              40
            (round (/ *print-right-margin* 2))))))
    (do* ((string1 (car slist1) (car slist1))
       (string2 (car slist2) (car slist2))
       (slist1 (cdr slist1) (cdr slist1))
       (slist2 (cdr slist2) (cdr slist2)))
      ((and (null string1) (null string2)) nil)
      (double-print string1 string2 tabpos ostream))))

;; Find the maximum length of string needed
;;  to accomodate the string.
(defun max-slength (stringlist)
  (reduce #'(lambda (x y)
           (max x (length y)))
       stringlist :initial-value 0))

;; The following follows a simple FSA-based algorithm:
;;   If the letter is after a letter, don't change mode.
;;   If a letter is after a left paren, leave in upper case mode.
;;   If a letter is after a space, leave in lower-case mode.
;;
;;  This actually boils down to:
;;    Hit a space-> go to upcase mode.
;;    Hit a left paren-> go to downcase mode.
;;
(defun string-predicatize (instring &aux mode)
  (setq mode :lowercase)
  (map 'string #'(lambda (char)
             (cond
               ((eq char #\() (setq mode :uppercase))
               ((eq char #\space) (setq mode :lowercase)))
             (cond
               ((eq mode :lowercase) (char-downcase char))
               ((eq mode :uppercase) (char-upcase char))
               (t (error "Bug in string-predicatize"))))
       instring))
               
;; Print two sexprs side by side.

#-aclpc
(defun dpprint (sexpr1 sexpr2 ostream)
  (let ((*print-right-margin* 
      (if (null *print-right-margin*)
          40 
        (round (/ *print-right-margin* 2)))))
    (double-print-list
     (listify-string (string-predicatize 
                        (write-to-string sexpr1 :pretty t)))
     (listify-string (string-predicatize 
                        (write-to-string sexpr2 :pretty t)))
       ostream)))

#+aclpc ;; a little trickier.
(defun dpprint (sexpr1 sexpr2 ostream)
   (let ((sstream (get-acl-scratch-stream)))
      (cg:set-right-margin sstream 
         (if (null *print-right-margin*) 
            40
           (round (/ *print-right-margin* 2))))
      (let ((strlist1 nil) (strlist2 nil))
         (pprint sexpr1 sstream)
         (setq strlist1
           (listify-string 
             (string-predicatize 
                (remove-null-strings (get-output-stream-string sstream)))))
         (pprint sexpr2 sstream)
         (setq strlist2
           (listify-string 
             (string-predicatize 
                (remove-null-strings (get-output-stream-string sstream)))))
         (double-print-list strlist1 strlist2 ostream))))

(defun listify-string (instring &key (separator #\newline))
  (let ((string-len (length instring))
	(string-list nil)
	(pos-end 0))
    (do ((pos-start 0 (1+ pos-end)))
	((= pos-end string-len)  (reverse string-list))
      (setq pos-end (or (position separator instring :start pos-start)
			string-len))
      (push (subseq instring pos-start pos-end) string-list))))

(defun remove-null-strings (string-list)
   "Remove any null strings from the list"
   (remove "" string-list :test 'string=))

;;; lisp-form.  Generating lisp forms from SME expressions
;;; ----------------------------------------------------------------------------
(defgeneric lisp-form (sme-object)
  (:documentation "Return the given object formatted as a Lisp 
      structured list of symbols."))

(defmethod lisp-form ((entity entity)) (user-form entity))
(defmethod lisp-form ((pred predicate)) (name pred))
(defmethod lisp-form ((exp expression))
  (cons (lisp-form (predicate exp))
	(mapcar #'(lambda (arg-entry)
		  (lisp-form (cdr arg-entry)))
		(arguments exp))))

(defmethod lisp-form ((list list)) (mapcar #'lisp-form list))
(defmethod lisp-form ((sym symbol)) sym)
(defmethod lisp-form ((num number)) num)
(defmethod lisp-form ((str string)) str)

(defmethod lisp-form ((dgroup description))
  (cons ':and
	(mapcar #'lisp-form
		(reverse (roots dgroup)))))

(defmethod lisp-form ((mh match-hypothesis))
  (list ':match (lisp-form (base-item mh))
	(lisp-form (target-item mh))
	(score mh)))

;; New lisp form for commutatiave mhs.
(defmethod lisp-form ((cmh commutative-match-hypothesis))
  (list ':match-c
	(lisp-form (base-item cmh))
	(lisp-form (target-item cmh))
	(score cmh)
	(mapcar #'lisp-form (possible-children cmh))))

(defmethod lisp-form ((map mapping))
  (list ':mapping (id map) (score map) 
	;; Put score first since there
	;; could be alot of matches
	(mapcar #'lisp-form (mhs map))
        ':inferences (mapcar #'ci-details (inferences map))
        ':reverse-inferences (mapcar #'ci-details (reverse-inferences map))))

(defmethod lisp-form ((sme sme))
  (list `(:sme ,(version sme)  (,(name (base sme))  ,(name (target sme))))
	`(:base ,(name (base sme)))
	`(:target ,(name (target sme)))
	`(:matches ,(mapcar #'lisp-form (mhs sme)))
	`(:mappings ,(mapcar #'lisp-form (mappings sme)))))

(defmethod lisp-form ((ci candidate-inference))
   "The structure of the candidate inference as a list."
  (lisp-form (form ci)))

(defun ci-details (ci)
  (list (lisp-form ci)
        :support (support-score ci)
        :extrapolation (extrapolation-score ci)))
