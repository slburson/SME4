;;;; -*- Mode: Lisp; Mode: Outline; -*-
;;;; ------------------------------------------------------------
;;;; File name: tools
;;;;    System: SME 
;;;;   Version: 4
;;;;    Author: Ron Ferguson
;;;;  Modified: Saturday, July 31, 1999 at 08:17:15 by ferguson
;;;; ------------------------------------------------------------
;;; These function provide various analyses of characteristics of
;;;  the mapping engine.
;;;
(in-package :sme)

(defun analyze-sme (&optional (sme *sme*) (stream t))
  "Return a number of interesting facts about the sme."
  (declare (type sme sme))
  (format stream "NAME: ~A  ID: ~A~%" (name sme) (id sme))
  (format stream "Number of MHs: ~A~%" (length (mhs sme)))
  (format stream "Inconsistent MHs: ~A~%" (count-number 'inconsistent? (mhs sme)))
  (format stream "Incomplete MHs: ~A~%" (count-number 'incomplete? (mhs sme)))
  (format stream "Number of Kernels: ~A~%" (length (root-mappings sme)))
  (format stream "Number of of inconsistent kernels: ~A"
	  (count-number 'inconsistent?
			(root-mappings sme))))


;;;------------------------------------------------------------
;;; Categorizing Sets of mhs
;;;------------------------------------------------------------

;; If a given predicate is expressed many times in a description,
;;  it can lead to a bloating of the number of MHs.  One way to
;;  see if this is happening is to give a list of the number of
;;  MHs by predicate number and type.  Note also that all function
;;  predicates can match to one another, so they are listed as well.
;;  MHs are categoried by the predicate of their BASE expression, except
;;  in the case of mixed function MHs, where the important consideration
;;  is whether the function mapped is mixed or unmixed.
(defun numbers-of-kernels-by-predicate (&optional (sme *sme*))
  "Return list of numbers of kernels by predicate."
  (declare (type sme sme))
  (let ((pred-table (make-hash-table))
	(complete-table (make-hash-table))
	(consistent-table (make-hash-table))
	(score-table (make-hash-table))
	(order-table (make-hash-table))
	(alist nil)
	(mh nil))
    (dolist (kernel (root-mappings sme))
      (setq mh (car (root-mhs kernel)))
      (unless (or (entity? mh) (null (functor-mh mh)))
	(let ((pred (predicate (base-item mh))))
	(incf (gethash pred pred-table 0))
	(when (not (inconsistent? mh))
	  (incf (gethash pred consistent-table 0)))
	(when (not (incomplete? mh))
	  (incf (gethash pred complete-table 0)))
	;; Increment average score.
	(incf (gethash pred score-table 0)
	      (score kernel))
	(incf (gethash pred order-table 0) 
	      (order (car (root-mhs kernel)))))))
    (maphash #'(lambda (pred val)
		 (push
		  (list (name pred)
			(/ (gethash pred score-table 0) val) ;; ave score.
			(/ (gethash pred order-table 0) val)
			val
			(gethash pred consistent-table 0)
			(gethash pred complete-table 0))
		  alist))
	     pred-table)
    (sort alist #'> :key 'cadr)))

(defun numbers-of-kernels-by-predicate-in-mapping (&optional mapping)
  "Return the list of numbers of kernels by predicate.  If mapping is
   not given, defaults to the first mapping of the current SME."
  (unless mapping (setq mapping (car (mappings *sme*))))
  (let ((pred-table (make-hash-table))
	(complete-table (make-hash-table))
	(consistent-table (make-hash-table))
	(score-table (make-hash-table))
	(order-table (make-hash-table))
	(alist nil))
    (dolist (mh (root-mhs mapping))
      (unless (or (entity? mh) (null (functor-mh mh)))
	(let ((pred (predicate (base-item mh))))
	  (incf (gethash pred pred-table 0))
	  (when (not (inconsistent? mh))
	    (incf (gethash pred consistent-table 0)))
	  (when (not (incomplete? mh))
	    (incf (gethash pred complete-table 0)))
	  (incf (gethash pred score-table 0) 
		(apply #'+ (mapcar #'score (descendants mh))))
	  (incf (gethash pred order-table 0) (order mh)))))
    (maphash #'(lambda (pred val)
		 (push 
		  (list (name pred)
			(/ (gethash pred score-table 0) val)
			(/ (gethash pred order-table 0) val)
			val
			(gethash pred consistent-table 0)
			(gethash pred complete-table 0))
		  alist))
	     pred-table)
    (sort alist #'> :key 'cadr)))

;;;------------------------------------------------------------
;;; Return all the kernels with a given name for predicate.
;;;------------------------------------------------------------
(defun all-kernels-with-pred (pred-name &optional (sme *sme*))
   "Return a list of kernels which have a root match containing the given 
    predicate."
   (declare (type symbol pred-name)
            (type sme sme))
   (let ((pred nil) (results nil))
      (dolist (kernel (root-mappings sme))
         (setq pred (predicate (base-item (car (root-mhs kernel)))))
         (when (eql pred-name (name pred))
            (push kernel results)))
      results))

(defun numbers-of-mhs-by-predicate (&optional (sme *sme*))
   "Return alist of numbers of mhs by predicate, including
    consistent and complete MHs."
   (declare (type sme sme))
   (let ((pred-table (make-hash-table))
         (complete-table (make-hash-table))
         (consistent-table (make-hash-table))
         (alist nil))
      (dolist (mh (mhs sme))
         (unless (or (entity? mh) (null (functor-mh mh)))
            (let ((pred (predicate (base-item mh))))
               (incf (gethash pred pred-table 0))
               (when (not (inconsistent? mh))
                  (incf (gethash pred consistent-table 0)))
               (when (not (incomplete? mh))
                  (incf (gethash pred complete-table 0))))))
      (maphash #'(lambda (pred val)
                   (push
                    (list (name pred) val
                      (gethash pred consistent-table 0)
                      (gethash pred complete-table 0))
                    alist))
        pred-table)
      (sort alist #'> :key 'cadr)))

(defun kernels-by-score-and-top-predicate (&optional (sme *sme*))
  "Returns a list of all the kernels in order of score, 
    with their top predicate given."
   (flet ((score-and-pred (kernel)
            (list (score kernel) 
              (name (predicate (base-item (car (root-mhs kernel))))))))
     (let* ((kernels (root-mappings sme))
            (score-list (mapcar 'score-and-pred kernels)))
        (sort score-list #'> :key 'car))))
 
(defun numbers-of-mhs-by-predicate-type (&optional (sme *sme*))
   "Return the number of MHs in the current SME, separated
    by predicate type."
   (declare (type sme sme))
   (let ((total-mhs 0)
         (entity-mhs 0)
         (functor-mhs 0)
         (function-mhs 0)
         (mixed-function-mhs 0)
         (relation-mhs 0)
         (attribute-mhs 0)
         (commutative-mhs 0)
         (pred nil))
      (dolist (mh (mhs sme))
         (cond
          ((entity? mh)
           (incf entity-mhs))
          ((null (functor-mh mh)) ;; functor-mhs don't have functor-mhs.
           (incf functor-mhs))
          (t
           (setq pred (predicate (base-item mh)))
           (incf total-mhs)
           ;; Check for predicate type.
           (cond ((function? pred)
                  (incf function-mhs)
                  (when (not (eq pred (predicate (target-item mh))))
                     (incf mixed-function-mhs)))
                 ((eql (pred-type pred) :relation)
                  (incf relation-mhs))
                 ((eql (pred-type pred) :attribute)
                  (incf attribute-mhs))
                 (t
                  (warn "Unknown predicate type ~A for MH ~A."
                    (pred-type pred) mh)))
           ;; Look for commutative predicates as well.
           (when (commutative? pred)
              (incf commutative-mhs)))))
      ;; Now return an alist of the numbers.
      (list (cons :total total-mhs)
        (cons :entity entity-mhs)
        (cons :functor-mhs functor-mhs)
        (cons :function function-mhs)
        (cons :mixed-function mixed-function-mhs)
        (cons :relation relation-mhs)
        (cons :attribute attribute-mhs)
        (cons :commutative commutative-mhs))))

;;;------------------------------------------------------------
;;; count-number
;;;------------------------------------------------------------
(defun count-number (predicate item-list)
  "Count number of times predicate is true in list."
  (let ((counter 0))
    (dolist (item item-list)
      (when (funcall predicate item) (incf counter)))
    counter))

;;;------------------------------------------------------------
;;; Trace Functions for SME
;;;------------------------------------------------------------
(defparameter *sme-key-functions* 
  '(match incremental-match extend-mhs greedy-cream greedy-merge 
     evaluate extend-all-mappings calculate-mapping-inferences)
  "Key subfunctions of sme.")

(defun trace-sme ()
   "Trace a set of key functions within the SME mapping engine."
   (eval `(trace ,@*sme-key-functions*)))

(defun untrace-sme ()
   "Untrace the set of key functioned traced by TRACE-SME."
   (eval `(untrace ,@*sme-key-functions*)))

;;;------------------------------------------------------------
;;; Return the predicates in a list-form SME expression.
;;;------------------------------------------------------------
(defun predicates-in-list-expr (expr)
  (cond
   ((listp expr)
    (cons (car expr)
	  (mapcan 'predicates-in-list-expr (cdr expr))))
   (t nil)))

;;;;;;;;;;;;;;;;;;
;;; More debugging code
;;;;;;;;;;;;;;;;;;

(defmethod intern-integrity-check ((thing sme))
   (nconc (intern-integrity-check (base thing))
     (intern-integrity-check (target thing))
     (mh-intern-integrity-check (mhs thing))))

(defun mh-intern-integrity-check (mhs)
   (let ((table nil))
      (dolist (mh mhs)
         (let* ((lisp-key (list (lisp-form (base-item mh))
                           (lisp-form (target-item mh))))
               (entry (assoc lisp-key table
                        :test 'equal)))
            (unless entry
               (setq entry (cons lisp-key nil))
               (push entry table))
            (push mh (cdr entry))))
      (remove-if-not #'(lambda (entry)
                         (> (length (cdr entry)) 2)) table)))
                

(defmethod intern-integrity-check ((thing description))
   (let ((table nil))
      (dolist (e (entities thing))
         (let* ((lisp-form (lisp-form e))
                (entry (assoc lisp-form table :test 'equal)))
            (unless entry
               (setq entry (cons lisp-form nil))
               (push entry table))
            (push e (cdr entry))))
      (dolist (e (expressions thing))
         (let* ((lisp-form (lisp-form e))
                (entry (assoc lisp-form table :test 'equal)))
            (unless entry
               (setq entry (cons lisp-form nil))
               (push entry table))
            (push e (cdr entry))))
      (remove-if-not #'(lambda (x) (> (length (cdr x)) 1)) table)))
