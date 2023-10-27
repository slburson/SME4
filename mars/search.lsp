;; -*- mode: lisp; -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;; cps, the classical problem solver
;;; last edited: 1/29/93, by kdf

;;; copyright (c) 1993, kenneth d. forbus, northwestern university,
;;; and johan de kleer, the xerox corporation.
;;; all rights reserved.

;;; this program implements the classical "problem space"
;;; model of ai problem solving.

#|
a problem is specified by an initial state, a set of operators that
act on states to produce new states, and a means of recognizing when
the goal has been achieved.  in this implementation the problem struct
holds this information.  to provide maximum flexiblity, we assume
nothing about states, but define the problem space in terms of a set
of procedures.
|#

(in-package :common-lisp-user)

(defvar *debug-cps* nil) ;; prints extra information for debugging.

(defstruct (problem (:print-function
                     (lambda (pr str ignore)
                       (declare (ignore ignore))
                       (format str "<problem: ~a>" (pr-name pr))))
        (:conc-name pr-))
  name  ;; something recognizable by user (person or system)
  (goal-recognizer nil)
  ;; procedure which returns nil if argument state isn't a goal
  (operator-applier nil)
  ;; finds all ways an operator can apply to a state. 
  ;; result is a list ((<operator instance> . <new state>)...)
  (operators nil) ;; list of operators which may be used.
  (states-identical? nil)
  ;; takes two states and returns nil if they are not identical
  (path-filter nil)
  ;; optional, returns nil if the given path doesn't make sense.
  ;; allows encoding of extra domain-specific constraints.
  (distance-remaining nil)
  ;; optional, returns estimate of distance from state to goal.
  (state-printer nil)  ;; produces a string describing a state.
  (solution-element-printer nil)
  ;; takes an operator instance and a state, and produces a string
  ;; suitable for human consumption.  used in displaying results.
  )

(defstruct (path (:print-function
                  (lambda (inst str ignore)
                    (declare (ignore ignore))
                    (format str "<path ~a>" 
                      (path-current inst)))))
  (pr nil)            ; the problem it is part of.
  (current nil)       ; the current state
  (so-far nil)        ; alternating states and operator instances
  (distance nil))     ; used in advanced versions

;;;; cps using breadth-first search
;;; returns three values, the final state, the path, and the number
;;; of states examined.

(defun bsolve (initial pr)
  (do ((queue (list (make-path :current initial
             :so-far (list initial)
             :pr pr))
        (append (cdr queue) new-paths))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined))) ;gather statistics
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps* 
  (format t "~% cps: found goal state: ~a"
    (funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))  
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~% cps: state explored: ~a"
        (funcall (pr-state-printer pr) (path-current (car queue))))
      (format t "~% cps: new operator instances:")
      (print-new-paths new-paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; extend paths using domain-specific procedures

(defun extend-path (path &aux new-paths new-path pr)
  (setq pr (path-pr path))
  (dolist (op (pr-operators pr) new-paths)
   (dolist (op-pair (funcall (pr-operator-applier pr)
           (path-current path)
           op))
     ;; there can be more than one instantiation of the operator
     ;; for each state, hence this inner loop.
     (setq new-path (make-path
         :pr (path-pr path)
         :current (cdr op-pair) ;new state
         :so-far (cons (cdr op-pair) 
           (cons (car op-pair) ;op instance
                 (path-so-far path)))))
     (unless (path-has-loop? new-path) ;avoid loops
       (unless (and (pr-path-filter pr)
        ;use domain guidance if available
        (funcall (pr-path-filter pr) new-path))
         (push new-path new-paths))))))

(defun path-has-loop? (ipath)
  ;;go backwards down path to see if a state is
  ;;duplicated.  must skip over operator instances.
  (do ((path (cddr (path-so-far ipath)) (cddr path))
       (state (path-current ipath))
       (pr (path-pr ipath)))
      ((null path))
    (if (funcall (pr-states-identical? pr) state (car path))
  (return t))))

(defun print-new-paths (new-paths)
  (dolist (new-path new-paths)
    (format t "~%  ~a" (cadr (path-so-far new-path))))
  (format t "."))

(defun print-answer (path &optional (stream *standard-output*)
       &aux rpath pr)
  (setq rpath (reverse (path-so-far path))
  pr (path-pr path))
  (format stream "~%initial state: ~a."
    (funcall (pr-state-printer pr) (car rpath)))
  (do ((path (cdr rpath) (cddr path))
       (step 1 (1+ step)))
      ((null path) (format stream "~% done."))
    (format stream "~%~d.  ~a" step
      (funcall (pr-solution-element-printer pr)
         (cadr path) (car path)))))
