;; -*- mode: lisp -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; MARS: implementation of operators
;;
;; last edited 2/4/94, by kdf

(in-package :common-lisp-user)

;;; class definitions

(defclass mars ()
  ((name
   :documentation "name"
   :type string
   :initarg :name
   :reader name)
   ;; inputs to the system
  (example
   :documentation "example to draw from, as an input"
   :type sme::description
   :initarg :example
   :reader example)
  (problem
   :documentation "problem to be solved"
   :type sme::description
   :initarg :problem
   :reader problem)
  (goal
   :documentation "pattern that must be filled in for solution"
   :type list
   :initarg :goal
   :reader goal)
  (kb 
   :documentation "knowledge base"
   :type sme::description
   :initarg :kb
   :reader kb)
  ;; internal parts
  (sme
   :documentation "matcher"
   :type sme::sme
   :initarg :sme
   :reader sme)
  (problem-space
   :documentation "problem space"
   :type problem
   :initarg :problem-space
   :reader problem-space)
  (example-memory
   :documentation "dgroup representing working memory used to hold example"
   :type sme::description
   :initarg :example-memory
   :reader example-memory)
  (working-memory
   :documentation "dgroup used as scratchpad" 
   :type sme::description
   :initarg :working-memory
   :reader working-memory)   
  (answer
   :documentation "trace of search"
   :type list
   :initform nil
   :accessor answer)
  (status
   :documentation "flag indicating the state of the system"
   :type symbol
   :initform :initial
   :accessor status)
  (n-examined
   :documentation "number of states examined when problem solving"
   :type integer
   :initform 0
   :accessor n-examined)
  (working-memories
   :documentation "dgroups built up in the course of processing"
   :initform nil
   :accessor working-memories)
  (debugging?
   :documentation "debugging flag"
   :initform nil
   :initarg :debugging? 
   :accessor debugging?)
  (states
   :documentation "list of search states generated"
   :type list
   :initform nil
   :accessor states)))

(defmethod print-object ((mars mars) stream)
  (format stream "<mars ~a>" (name mars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mars* nil)
(defvar *mars-vocabulary* nil)
(defvar *mars-kb* nil)
(defvar *mars-match-filter* '((identicalFunctions)))

(defun mars-init ()
  ;; reloads the extremely minimal background knowledge
  (let ((sme::*sme-vocabulary-path* *mars-pathname*)
        (sme::*sme-vocabulary-extension* "vcb")
        (sme::*sme-description-path* *mars-pathname*)
        (sme::*sme-description-extension* "dgr"))
  (setq *mars-vocabulary* (sme::vocabulary-from-file "cpad"))
  (setq *mars-kb* (sme::dgroup-from-file "kb"))))

(defun in-mars (mo) (setq *mars* mo)) 

(defvar *default-debugging* t)

(defmacro with-mars (mo &rest forms)
  `(let* ((*mars* ,mo)
          (*debug-cps* (mars-debugging *mars*)))
     ,@ forms))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(special *default-same-functor*
                      *default-same-function*
                      *default-trickle-down*
                      sme::*default-same-functor*
                      sme::*default-same-function*
                      sme::*default-trickle-down*)))

;;; Always call mars-init before anything else

(defun create-mars (name goal problem example kb
                       &optional (debugging? *default-debugging*))
   (let ((wm (make-instance 'sme::description
               :name (format nil "~a wm" (sme::name example))
               :vocabulary *mars-vocabulary*))
         (em (make-instance 'sme::description
               :name (format nil "~a example memory" (sme::name example))
               :vocabulary *mars-vocabulary*)))
      (in-mars
        (make-instance 'mars
          :name name
          :goal goal
          :problem problem
          :example example
          :sme (sme::define-sme em problem) ;; not quite right?
          :problem-space (make-mars-problem-space name)
          :example-memory em
          :working-memory wm
          :kb kb
          :debugging? debugging?))
      *mars*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; state abstraction

(defclass mars-state ()
  ((current-goal
    :documentation "the goal of this state"
    :type list
    :initarg :current-goal
    :reader current-goal)
   (goal-stack
    :documentation "stack of goals to return to if achieved."
    :type list
    :initarg :goal-stack
    :reader goal-stack)
   (difficulty
    :documentation "estimate of difficulty"
    :type float
    :initarg :difficulty
    :reader difficulty)
   ;; pointers between states to record the search
   (previous
    :documentation "previous state, if any"
    :type t
    :initarg :previous
    :initform nil
    :accessor previous)
   (extensions
    :documentation "next states, if any"
    :type list
    :initform nil
    :accessor extensions)
   (notes
    :documentation "information placed on state by problem solver"
    :type list
    :initform nil
    :accessor notes)
   (killed?
    :documentation "killed by loop detection?"
    :type t
    :initform nil
    :accessor killed?)))

(defmethod print-object ((state mars-state) stream)
  (format stream "<state ~a>" (current-goal state)))

(defmethod same-state? ((arg1 t) (arg2 t)) nil)

(defmethod same-state? ((arg1 mars-state) (arg2 mars-state))
  (and (equal (current-goal arg1) (current-goal arg2))
       (equal (goal-stack arg1) (goal-stack arg2))))

(defun make-new-state (current-goal goal-stack
            &optional (*mars* *mars*))
  (make-instance 'mars-state
     :current-goal current-goal
     :goal-stack goal-stack
     :difficulty (calculate-goal-difficulty
            current-goal goal-stack)))

(defmethod show ((state mars-state) &optional (stream *standard-output*))
  (format stream "~%   ~d: try ~a."
    (difficulty state) (current-goal state))
  ;; short form (format stream "~%   [~a]" (mapcar #'car (goal-stack state)))
  (cond ((null (goal-stack state)) (format stream "~%    []"))
  (t (format stream "~%    [~a" (car (goal-stack state)))
     (dolist (goal (cdr (goal-stack state)))
     (format stream "~%     ~a" goal))
     (format stream "]"))))

(defun looping-state? (state)
  ;; in this system, looping shows up by duplicate use-equation
  ;; activities.  look for those explicitly.
  (duplicate-use-equation-activities? (goal-stack state) nil))

(defun duplicate-use-equation-activities? (stack previous)
  (cond ((null stack) nil) ;; safe
  ((eq (caar stack) 'use-equation)
   (if (member (third (car stack)) 
         previous :test #'equal)
       t ;; yep, same equation used.
     (duplicate-use-equation-activities?
      (cdr stack) (cons (third (car stack)) previous))))
  (t (duplicate-use-equation-activities?
      (cdr stack) previous))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; debugging flags

(defmacro mars-debugging (&rest forms)
  `(when (debugging? *mars*) ,@ forms))

(defmacro serious-mars-debugging (&rest forms)
  `(when (eq (debugging? *mars*) :serious) ,@ forms))

