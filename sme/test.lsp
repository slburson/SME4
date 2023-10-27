;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: test.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: Feb 20, 1998
;;;;  $LastChangedDate: 2014-08-06 12:30:35 -0500 (Wed, 06 Aug 2014) $
;;;;  $LastChangedBy: forbus $
;;;;   Purpose: Support for regression testing
;;;; --------------------------------------------------------------------------
(in-package :sme)

;;; These procedure are designed to test SME by:
;;;   * comparing the results of SME matches with previously known
;;;      valid results
;;;   * benchmarking sets of SME matches
;;;   * allowing new tests to be constructed as needed.
;;; 
;;; Each test is written via defsmetest, and saved as
;;;  a sme-test class object.  New tests can either be 
;;;  added manually (using the defsmetest macro) or 
;;;  generated from known valid matches using 
;;;  construct-sme-test.
;;; 
;;; Individual tests may be run via run-sme-test <sme-test-object>,
;;;  or via run-sme-tests, which runs all currently loaded
;;;  SME tests.  sme-shakedown runs a set of standard tests.
;;;
;;; After the test is run, the test object is updated to show
;;;  if the test was passed, how low (in seconds) the mapping
;;;  took, and the list of discrepencies.
;;;
;;; (defsmetest <test name> <vocab filename>
;;;      <base filename> <target filename>
;;;    (:<sme parameter keyword> <value>)
;;;    (:mapping <ordinal>
;;;       (:<mapping parameter keyword> <value>)
;;;    ...)
;;;  
;;; (sme::run-sme-test <test object>)
;;;    Run a single SME test, comparing results to known values.
;;;   
;;; (sme::construct-sme-test <sme> <test name>)
;;;    Given a known good SME object, construct a DefSMETest test 
;;;    that stores its parameter values.  May not correctly construct
;;;    base and dgroup filenames. 
;;;
;;; (sme-shakedown)
;;;    Run a set of known good tests in regression.lsp
;;;
(defvar *sme-tests* nil "List of all available SME tests.")

(defvar *detailed-sme-test-results* nil "Cache of test and SME used to generate results, for further probing.")

(defvar *cache-detailed-sme-test-results?* t "Cache detailed sme test results by default.")

;;; TEST METHODS
;;; ----------------------------------------------------------------------------
;;; Test methods form the basis of the tester.  They are used both to 
;;;   retrieve the actual values of generated SME's and mappings, and
;;;   to construct new tests from existing SME's and mappings.
;;; New test dimensions can be added by adding new entries to either
;;;   of the following tables, where the car is a keyword for that
;;;   test, and the cdr is a function that can be called to retrieve
;;;   that value.  All values should be generated in a form comparable
;;;   by EQUAL.
;;;
(defparameter *sme-test-value-methods* 
  (list
    (cons :mapping-parameters 
          #'(lambda (sme) (lisp-form (mapping-parameters sme))))
    (cons :mh-count      #'(lambda (sme) (length (mhs sme))))
    (cons :mapping-count #'(lambda (sme) (length (mappings sme))))
    (cons :kernel-count  #'(lambda (sme) (length (kernel-mappings sme))))
    ))

(defparameter *mapping-test-value-methods*
  (list
    (cons :mh-count #'(lambda (mapping) (length (mhs mapping))))
    (cons :score 'score)
    (cons :root-score 'root-score)
    (cons :entity-mhs 
          #'(lambda (mapping)
              (mapcar #'(lambda (mh) `(,(user-form (base-item mh))
                                       ,(user-form (target-item mh))))
                (entity-mhs mapping))))
    (cons :root-mhs
          #'(lambda (mapping)
              (mapcar #'(lambda (mh) `(,(user-form (base-item mh))
                                       ,(user-form (target-item mh))))
                (root-mhs mapping))))
    (cons :root-mh-count #'(lambda (mapping) (length (root-mhs mapping))))
    (cons :cis #'(lambda (mapping) (lisp-form (inferences mapping))))
    ))

;;; return-time macro
;;; ----------------------------------------------------------------------------
(defmacro return-time (&rest forms)
   "Evaluative the form and return the match time in seconds.
    Does not print out information to standard out, as Lisp's
    TIME function does."
   `(let ((start-time (get-internal-run-time)))
       ,@forms
       (* 1.0 (/ (- (get-internal-run-time) start-time)
                 internal-time-units-per-second))))

;;; sme-test class definition
;;; ----------------------------------------------------------------------------
(defclass sme-test ()
    ((name
       :documentation "The name of this test."
       :accessor name :initarg :name)
     (base-filename
      :documentation "Filename for the base description."
      :accessor base-filename :initarg :base-filename)
     (target-filename
      :documentation "Filename for the target description."
      :accessor target-filename :initarg :target-filename)
     (vocabulary-filename
      :documentation "Filename for the vocabulary description."
      :accessor vocabulary-filename :initarg :vocabulary-filename)
     (passed?
      :documentation "Was the test passed?"
      :accessor passed? :initform :untested)
     (execution-time
      :documentation "Time required to execute test (without checking)."
      :accessor execution-time :initform nil)
     (sme-tests
      :documentation "Tests on the SME object itself."
      :accessor sme-tests :initform nil :initarg :sme-tests)
     (mapping-tests
      :documentation "Tests on the mappings as a list of lists."
      :accessor mapping-tests :initform nil :initarg :mapping-tests)
     (results
      :documentation "Keyed list of results for this test."
      :accessor results :initform nil :initarg :results))
   (:documentation
    "A single test for SME."))

(defmethod print-object ((object sme-test) stream)
   "Print out the SME test object."
   (format stream "<SME Test: ~A>" (name object)))

(defmacro defsmetest (name vocabulary base target &rest forms)
   "Create and return an SME test object."
   `(expand-defsme-test ,name ,vocabulary ,base ,target ',forms))

(defun expand-defsme-test (name vocab base target forms)
   (let ((mapping-forms nil) (sme-forms nil)
         (sme-test nil))
      (setq mapping-forms
        (loop
          for test in forms
          when (eql (car test) :mapping)
          collect test))
      (setq sme-forms 
        (set-difference forms mapping-forms :test #'equal))
      (setq sme-test
        (make-instance 'sme-test
          :name name
          :base-filename base
          :target-filename target
          :vocabulary-filename vocab
          :sme-tests sme-forms
          :mapping-tests mapping-forms))
      (push sme-test *sme-tests*)
      sme-test))

(defun load-sme-tests (test-file &optional (path *sme-path*))
   "Load the set of SME tests in the given test file, and set 
   *sme-tests* to those tests."
   (setq *sme-tests* nil)
   (qrg:load-file path test-file)
   (setq *sme-tests* (nreverse *sme-tests*))
   *sme-tests*)

;;; sme-shakedown (entry point)
;;; ----------------------------------------------------------------------------
(defun sme-shakedown (&optional (sme-type 'sme))
   "Run SME on a set of prototypical matches, and check to see if the
    results are the same as those previously obtained."
  (if *cache-detailed-sme-test-results?*
    (setq *detailed-sme-test-results* nil))
  (let ((*default-use-less-greedy-greedy-merge?* nil)
        (*default-block-most-out-of-mapping-contributions?* nil))
   (load-sme-tests "regression")
   (run-sme-tests sme-type)))
 
(defun run-sme-tests (&optional (sme-type 'sme))
   "Simply run all the sme test currently in *sme-tests*.
    Prints report at end detailing which tests failed, if any."
   (format t "~%~%")
   (let* ((num-tests (length *sme-tests*))
          (*sme-vocabulary-path* (qrg:append-qrg-path *sme-path* "vocab"))
          (*sme-description-path* (qrg:append-qrg-path *sme-path* "dgroup"))
          (failed-tests
           (loop
            for test in *sme-tests*
            do (run-sme-test test sme-type)
            when (not (passed? test)) collect test)))
      (cond ((null failed-tests)
             (format t "~% Passed all ~D tests.~%" num-tests))
            (t
             (format t "~% Passed ~D of ~D tests.~%"
               (- num-tests (length failed-tests)) num-tests)
             (format t " Failed tests: ~A~%" failed-tests)))
      (format t " Total match time: ~A s~%"
        (reduce #'+ (mapcar 'execution-time *sme-tests*)))
      *sme-tests*))

(defmacro with-test-parameters (sme-test &rest body)
   "locally sets the sme parameters to the ones specified in the test"
  `(let ((*default-same-functor* (or (get-parameter-value :same-functor ,sme-test)
                                     *default-same-functor*))
         (*default-same-function* (or (get-parameter-value :same-function ,sme-test)
                                      *default-same-function*))
         (*default-trickle-down* (or (get-parameter-value :trickle-down ,sme-test)
                                     *default-trickle-down*))
         (*default-max-local-score* (or (get-parameter-value :max-local-score ,sme-test)
                                        *default-max-local-score*))
         (*default-functor-trickle-down* 
          (get-parameter-value :functor-trickle-down? ,sme-test))
         (*default-greedy-cutoff* (or (get-parameter-value :greedy-cutoff ,sme-test) 
                                      *default-greedy-cutoff*))
         (*default-greedy-max-#* (or (get-parameter-value :greedy-max-\# ,sme-test)
                                     *default-greedy-max-#*))
         (*default-use-less-greedy-greedy-merge?*
          (or (get-parameter-value :use-less-greedy-greedy-merge? ,sme-test)
              *default-use-less-greedy-greedy-merge?*))
         (*default-block-most-out-of-mapping-contributions?*
          (or (get-parameter-value :block-most-out-of-mapping-contributions?
                                   ,sme-test)
              *default-block-most-out-of-mapping-contributions?*)))
       ,@body))

(defun run-sme-test (sme-test &optional (sme-type 'sme))
   "Run a single SME mapping test."
   (declare (type sme-test sme-test))
   (format t "~%   Running test ~A..." sme-test)
   (with-slots (vocabulary-filename base-filename target-filename) sme-test
     (in-vocabulary (vocabulary-from-file vocabulary-filename))
     (with-test-parameters sme-test
      (let* ((sme (define-sme (dgroup-from-file base-filename)
                    (dgroup-from-file target-filename) :sme-type sme-type)))
         (cl-user::gc)
         (setf (execution-time sme-test)
               (return-time (match sme)))
        (format t " (~A s)" (execution-time sme-test))
        (if *cache-detailed-sme-test-results?*
          (push (cons sme-test sme) *detailed-sme-test-results*))
         (dolist (test (sme-tests sme-test))
            (validate-sme-test sme sme-test (car test) (cadr test)))
         (dolist (test-set (mapping-tests sme-test))
            (let ((mapping (nth (1- (second test-set)) (mappings sme))))
               (if (null mapping)
                  (warn "No mapping #~A" (second test-set))
                  (dolist (test (cddr test-set))
                     (validate-mapping-test mapping sme-test 
                       (car test) (cadr test)))))))))
   (setf (passed? sme-test) (null (results sme-test)))
   sme-test)

(defun validate-sme-test (sme sme-test test-keyword expected-value)
   "Check to see if the expected value is the one obtained."
   (let ((actual-value
           (funcall (get-sme-value-fn test-keyword) sme)))
      (unless (equal-test-values expected-value actual-value)
         (warn-sme-test test-keyword expected-value actual-value)
         (store-test-mismatch sme-test test-keyword
           expected-value actual-value))))



(defun get-parameter-value (value-key sme-test)
   (second (assoc value-key
          (second (assoc :mapping-parameters (sme-tests sme-test))))))
           
;;; warn-sme-test
;;; ----------------------------------------------------------------------------
;;;  If the value is a list, given a warning based on set differences.
;;;  Otherwise, simply indicate the differing values.

(defgeneric warn-sme-test (test-keyword expected actual)
  (:documentation "Warn user of discrepency."))

(defmethod warn-sme-test ((test-keyword t) (expected t) (actual t))
   (warn "Expected ~A value to be ~A, but is ~A instead."
       test-keyword expected actual))

(defmethod warn-sme-test ((test-keyword t) (expected list) (actual list))
   (warn "Mismatch in expected ~A value:" test-keyword)
   (let ((missing (set-difference expected actual :test 'equal))
         (extras (set-difference actual expected :test 'equal)))
      (terpri *error-output*)
      (when missing
         (format *error-output* "   Missing values: ~A~%" missing))
      (when extras
         (format *error-output* "   Extra values: ~A~%" extras))))

(defun validate-mapping-test (mapping sme-test test-keyword expected-value)
   "Check to see if expect mapping parameter is the one obtained."
   (let ((actual-value
           (funcall (get-mapping-value-fn test-keyword) mapping)))
      (unless (equal-test-values expected-value actual-value)
         (warn-sme-test test-keyword expected-value actual-value)
         (store-test-mismatch sme-test test-keyword expected-value
            actual-value))))

;;; Constructing New Tests
;;; ----------------------------------------------------------------------------
;;; Given an existing SME mapping engine, we can automatically construct
;;;  a defsmetest form by simply storing all the values for all the
;;;  existing test methods.  The following procedure, construct-sme-test,
;;;  does this.

(defun construct-sme-test (&optional (sme *sme*) (name "test"))
   "For the given SME, return a set of test clauses for the match."
   (let ((clauses   ;; test clauses for sme as whole.
           (mapcar #'(lambda (test-key)
                       (construct-sme-test-clause (car test-key)))
             *sme-test-value-methods*))
         (map# 0))
      (setf clauses ;; add clauses for all individual mappings.
            (append clauses
              (mapcar #'(lambda (mapping)
                          (construct-mapping-test mapping (incf map#)))
                (mappings sme))))
      ;; Construct defsmetest form.
      `(defsmetest ,name ,(name (vocabulary (base sme))) 
         ,(name (base sme)) ,(name (target sme))
         ,@clauses)))

(defun construct-sme-test-clause (test-keyword &optional (sme *sme*))
   "Construct a single test clause from an existing SME."
   `(,test-keyword
     ,(funcall (get-sme-value-fn test-keyword) sme)))

(defun construct-mapping-test (mapping n)
   "For the given mapping, return a set of test clauses."
   (let ((clauses nil))
      (dolist (test-key (reverse (mapcar #'car *mapping-test-value-methods*)))
         (push (construct-mapping-test-clause test-key mapping) clauses))
      `(:mapping ,n ,@clauses)))

(defun construct-mapping-test-clause (test-keyword mapping)
   "Construct a single test clause for the given mapping."
   `(,test-keyword
     ,(lisp-form (funcall (get-mapping-value-fn test-keyword) mapping))))

;;; Equality Checking
;;; ----------------------------------------------------------------------------
;;; We use a slightly modified type of equality for testing results.
;;;  Values are equal if they are EQUAL, but also (if lists) if they
;;;  contain the same elements.
(defgeneric equal-test-values (value1 value2)
  (:documentation "Equal for the purpose of the test?."))

(defmethod equal-test-values ((value1 t) (value2 t))
   "Compare two different test values."
   (equal (lisp-form value1) (lisp-form value2)))

(defmethod equal-test-values ((value1 list) (value2 list))
   "List comparisons are on the basis of sets."
   (if (or (eq :reversible (car value1))
           (eq :reversible (car value2))) ;; kludge for magi tests.
      (equal-reversible-sets (cdr value1) (cdr value2))
      (equal-sets value1 value2)))

(defun equal-sets (value1 value2)
   "True if items are the same set."
   (null (or (set-difference value1 value2 :test 'equal)
             (set-difference value2 value1 :test 'equal))))

(defun equal-reversible-sets (value1 value2)
   "True if first and second items are reversible and the same set."
   (or (equal-sets value1 value2)
       (equal-sets value1 
        (mapcar #'(lambda (item) (list (second item) (first item)))
          value2))))

(defmethod equal-test-values ((value1 number) (value2 number))
   "Numeric comparisons are fuzzied to 0.001"
   (> 0.001 (abs (- value1 value2))))


(defun get-sme-value-fn (test-keyword)
   "Retrieve the value function for SME test values.  Error if none."
   (let ((value-function (cdr (assoc test-keyword *sme-test-value-methods*))))
      (when (null value-function)
         (error "No SME Test value function for ~A" test-keyword))
      value-function))

(defun get-mapping-value-fn (test-keyword)
   "Retrieve the value function for mapping test values.  Error if none."
   (let ((value-function (cdr (assoc test-keyword *mapping-test-value-methods*))))
      (when (null value-function)
         (error "No SME Mapping Test value function for ~A" test-keyword))
      value-function))

;;; Utility Routines
;;; ----------------------------------------------------------------------------
(defun store-test-mismatch (sme-test test-key test-value actual-value)
   "Store the mismatch in the results section of the SME Test."
   (push `(,test-key :expected ,test-value :actual ,actual-value)
     (results sme-test)))
