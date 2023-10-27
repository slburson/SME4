;; -*- mode: lisp -*-

;;;; MARS: driver loop
;;

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (proclaim '(special *state*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; defining a mars problem space

(defun mars-goal-recognizer (state)
  (declare (ignore state))
  ;; goal must be the name of a quantity whose value is to be found.
  ;; ignore the state, the question is can an appropriate
  ;; answer to the original goal be found in the description dgroup?
  (let ((goal (goal *mars*)))
    (multiple-value-bind (known? value exp)
        (lookup-nvalue goal (working-memory *mars*)) ;; was problem
      (declare (ignore value exp))
      known?)))

(defun make-mars-problem-space (name)
  (make-problem
   :name (format nil "~a ps" name)
   :goal-recognizer 'mars-goal-recognizer
   :operator-applier 'mars-operator-applier
   :operators (list 'find-relevant-operator)
   :state-printer
   #'(lambda (state) (format nil "~a" (current-goal state)))
   :solution-element-printer
   #'(lambda (state activity)
       (declare (ignore state))
       (format nil "~a" activity))
   :states-identical? 'same-state?
   :path-filter #'(lambda (path) (looping-state? (path-current path)))
   :distance-remaining 'difficulty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; initial driver loop

(defun try (&optional (*mars* *mars*))
 (with-mars *mars*
  (setf (status *mars*) :working)
   (mars-debugging (format t "~%reading the problem.."))
   (read-the-problem *mars*)
   (mars-debugging (format t "~%skimming the example..")) 
   (skim-the-example *mars*)
   (serious-mars-debugging
    (format t "~% ~d interpretations of the example:"
      (length (sme::mappings (sme *mars*))))
    (sme::show-mappings (sme *mars*)))
   (mars-debugging (format t "~%starting in.."))
   (let ((initial-state
           (make-new-state `(find-nvalue ,(goal *mars*))
             nil)))
      (push initial-state (states *mars*))
      (multiple-value-bind (answer n-examined)
          (debug-solve initial-state (problem-space *mars*))
         (cond (answer
                 (setf (answer *mars*) answer)
                 (setf (status *mars*) :solved)
                 (setf (n-examined *mars*) n-examined)
                 answer) 
               (t (setf (status *mars*) :failed)
                 (setf (n-examined *mars*) n-examined)
                 nil))))))

(defun explain (&optional (*mars* *mars*))
  (case (status *mars*)
  (:working (format t "~% was interrupted for some reason."))
  (:failed (format t "~% failed to find a value for ~a"
       (goal *mars*)))
  (t (print-answer (answer *mars*))
     (format t "~%  ~d states examined."
       (n-examined *mars*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; helpers for debugging

(defun debug-solve (initial pr) ;; a version of best-solve
   (declare (special *state*))
   (unless (and (pr-distance-remaining pr)
                (or (functionp (pr-distance-remaining pr))
                    (and (symbolp (pr-distance-remaining pr))
                         (fboundp (pr-distance-remaining pr)))))
      (error "~%distance estimation procedure must be defined first!"))
   (do ((queue
          (list (let ((p (make-path :current initial
                           :so-far (list initial)
                           :pr pr)))
                   (setf (path-distance p)
                         (funcall (pr-distance-remaining pr)
                           (path-current p)))
                   p))
          ;merge new paths, keeping queue sorted by distance to goal.
          ;;;   (let ((nqueue (cdr queue)))
          ;;;     (dolist (path new-paths nqueue)
          ;;;     (setf (path-distance path) ; estimate distance
          ;;;     (funcall (pr-distance-remaining pr)
          ;;;        (path-current path)))
          ;;;     (setq nqueue
          ;;;     (merge 'list (list path) nqueue ; insert in order
          ;;;            #'< :key #'(lambda (x) (path-distance x))))))
          ;;;;; experiment: try simple depth-first search
          (nconc new-paths (cdr queue))
          )
        (new-paths nil nil)
        (number-examined 1 (1+ number-examined)))
       ((null queue))
     (when *debug-cps*
      (format t "~%===== ~d =============" number-examined))
      ;;      (if (= number-examined 143) (break)) ;; for old breaking point
      ;;      (show (path-current (car queue)))
      (let ((*state* (path-current (car queue)))) ;; for debugging
         (when (funcall (pr-goal-recognizer pr) (path-current (car queue)))
            (when *debug-cps*
               (format t "~%found goal state: ~a"
                 (funcall (pr-state-printer pr)
                   (path-current (car queue)))))
            (return (values (car queue) number-examined)))  
         (setq new-paths (debug-extend-path (car queue)))
         )
      ;;    (format t "~% -----")
      ;;    (format t "~% extensions: ")
      ;;    (dolist (path new-paths) (show (path-current path)))
      ;;    (format t "~% -----")
      ))

(defun debug-extend-path (path &aux new-paths new-path pr)
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
     (push (cdr op-pair) (states *mars*))
     (setf (previous (cdr op-pair)) (path-current path))
     (push (cdr op-pair) (extensions (path-current path)))
     (cond ((path-has-loop? new-path) ;avoid loops
      (setf (killed? (cdr op-pair)) :loop))
     ((and (pr-path-filter pr)
      ;; use domain guidance if available
           (funcall (pr-path-filter pr) new-path))
      (setf (killed? (cdr op-pair)) :domain-filter))
     (t (push new-path new-paths))))))
