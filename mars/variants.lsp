;; -*- mode: lisp; -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;; different search strategies
;;; last edited: 1/29/93, kdf

;;; copyright (c) 1986-1991, kenneth d. forbus, northwestern university,
;;; and johan de kleer, the xerox corporation.
;;; all rights reserved.

(in-package :common-lisp-user)

;; small variations in bsolve suffice to
;; implement a variety of search strategies.

(defun dsolve (initial pr)
  (do ((queue (list (make-path :current initial
             :so-far (list initial)
             :pr pr))
        ; small change, large difference!
        (append new-paths (cdr queue)))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined)))
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps*
  (format t "~%found goal state: ~a"
    (funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))  
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~%state explored: ~a"
        (funcall (pr-state-printer pr) (path-current (car queue))))
      (format t "~%new operator instances:")
      (print-new-paths new-paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; best-solve

(defun best-solve (initial pr)
  (unless (and (pr-distance-remaining pr)
               (or (functionp (pr-distance-remaining pr))
                   (and (symbolp (pr-distance-remaining pr))
                        (fboundp (pr-distance-remaining pr)))))
    (error "~%distance estimation procedure must be defined first!"))
  (do ((queue
  (list (make-path :current initial :so-far (list initial) :pr pr
      :distance (funcall (pr-distance-remaining pr)
             (list initial))))
        ;merge new paths, keeping queue sorted by distance to goal.
  (let ((nqueue (cdr queue)))
    (dolist (path new-paths nqueue)
    (setf (path-distance path) ; estimate distance
    (funcall (pr-distance-remaining pr) (path-so-far path)))
    (setq nqueue
    (merge 'list (list path) nqueue ; insert in order
           #'< :key #'(lambda (x) (path-distance x)))))))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined)))
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps*
  (format t "~%found goal state: ~a"
    (funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))  
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~%state explored: ~a"
        (funcall (pr-state-printer pr)  (path-current (car queue))))
      (format t "~%new operator instances:")
      (print-new-paths new-paths))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; beam solve

(defun beam-solve (initial pr &optional (n 3))
  (do ((queue
  (list (make-path :current initial :so-far (list initial)
      :pr pr :distance (list initial)))
  (let ((nqueue (cdr queue)))
    (dolist (path new-paths)
      (setf (path-distance path) ; estimate distance
      (funcall (pr-distance-remaining pr) (path-so-far path)))
      (setq nqueue (merge 'list (list path) nqueue ; insert in order
        #'<
        :key #'(lambda (x) (path-distance x)))))
    (when (> (length nqueue) n) ;; clips all but first n
      (setf (cdr (nthcdr (1- n) nqueue)) nil))
    nqueue))
       (new-paths nil nil)
       (number-examined 1 (1+ number-examined)))
      ((null queue))
    (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
      (when *debug-cps*
  (format t "~%found goal state: ~a"
    (funcall (pr-state-printer pr) (path-current (car queue)))))
      (return (values (car queue) number-examined)))  
    (setq new-paths (extend-path (car queue)))
    (when *debug-cps*
      (format t "~%state explored: ~a"
        (funcall (pr-state-printer pr) (path-current (car queue))))
      (format t "~%new operator instances:")
      (print-new-paths new-paths))))
