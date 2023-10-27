;; -*- mode: lisp -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; MARS: implementation of goals
;;
;; last edited 2/8/94, by kdf

(in-package :common-lisp-user)

;; goals in MARS are one of the following forms:

;; (find-value <quantity> <eqn>) try to find the value of a quantity.
;; this can be immediately satisfied by finding a numerical value in  
;; the working memory.  if not, the find-equation goal is the next
;; step.

;; (find-equation <quantity>) try to find an equation that mentions
;; <quantity>.  this goal is moot if a numerical value has already been
;; found for <quantity>.  otherwise, it does three things:
;;
;; 1. looks for an equation in the working memory that is relevant.  if
;;    if finds one, it queues a (try-equation <eqn> <quantity>) goal.
;;
;; 2. with lower priority, it queues a (get-example-info <quantity>
;; <mapping>) goal for the current mapping.
;; 

;;(find-mapping <goal>) uses

;; (get-example-info <expression> <mapping>) 

;; (use-equation <equation> <quantity>) uses 

;; (find-values-for <eqn> <values-needed>)

;; (find-global-equations) calculates the global equations for the cycle
;; by making and using closed-world assumptions.

;; states in the problem space correspond to working on a particular goal.
;; the solution criterion is when there is an expression in the solution
;; dgroup that provides a floating-point value for the goal expression. 

;; states are prioritized, using a two-level scheme.
;;  the top level priority is the type of goal.
;;   goals that involve using current information are higher priority
;;    than goals that involve looking at the example.
;;  the 2nd level priority is the difficulty of the goal for that type
;;   of goal.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; priority scheme for goals

(defun calculate-goal-difficulty (current-goal goal-stack)
  (+ (* 100.0 (length goal-stack)) ;; small part
     (case (car current-goal)
     (use-equation 100.0) ;; doing something
     (find-nvalue 200.0)
     (find-equation-for 500.0)
     (examine-cis-for 1000.0)
     (get-example-info 1000.0)
     (t 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; procedures associated with find-nvalue goals

(defun find-nvalue-achieved? (goal &optional (*mars* *mars*))
  ;; format is (find-nvalue <q> . <goal stack>)
  (multiple-value-bind (known? value exp)
      (lookup-nvalue (cadr goal) (working-memory *mars*))  ;; was problem
    (declare (ignore value exp))
    known?))

(install-achieved-procedure find-nvalue find-nvalue-achieved?)

(defun find-nvalue-failed? (goal &optional (*mars* *mars*))
  (declare (ignore goal))
  nil) ;; can't tell, really.

(install-failure-procedure find-nvalue find-nvalue-failed?)

(defun try-find-nvalue (goal &optional (*mars* *mars*))
  (mars-debugging (format t "~%  looking for ~a.." (cadr goal)))
  (multiple-value-bind (known? value exp)
   ;; if here, have already looked in the problem. 
   ;; next, check the kb
   (lookup-nvalue (cadr goal) (kb *mars*))
   (cond (known?
    (mars-debugging (format t "found it."))
    ;; install the assertion in the problem dgroup
    (push `(:found (= ,(cadr goal) ,value) :via-kb)
    (notes *state*))
    (sme::define-expression (sme::user-form exp)
          (working-memory *mars*)) ;; was problem
    (values t `(found (= ,(cadr goal) ,value) :via-kb)
      nil))
   (t (push `(:nvalue-not-found ,(cadr goal))
      (notes *state*))
      nil))))

(install-try-procedure find-nvalue try-find-nvalue)

(defun find-nvalue-suggestions (current-goal goal-stack
               &optional (*mars* *mars*))
  ;; major suggestion at this point: find some equation that constrains it.
  (let ((new-goal `(find-equation-for ,(cadr current-goal))))
    (list (cons new-goal (make-new-state new-goal goal-stack)))))

(install-suggestions-procedure find-nvalue find-nvalue-suggestions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; the find-equation-for goal.
;;
;; form: (find-equation-for <q>)
;;
;; three methods:
;; 1. find it in the problem.
;; 2. find it in the kb.
;; 3. look for an equation in the example.

(defun find-equation-for-failure? (goal)
  (declare (ignore goal)) nil)

(install-failure-procedure find-equation-for find-equation-for-failure?)

(defun find-equation-for-achieved? (goal &optional (*mars* *mars*))
  ;; because we want to push the goal of using the equation if we find it,
  ;; postpone looking until we try the goal.
  (declare (ignore goal))
  nil)

(install-achieved-procedure find-equation-for find-equation-for-achieved?)

(defun try-find-equation-for (goal &optional (*mars* *mars*))
   (let* ((q (cadr goal))
          (eqns (nconc 
                  (find-equations-mentioning q (working-memory *mars*))
                  ;; was problem
                  (find-equations-mentioning q (kb *mars*)))))
      (cond (eqns
              (mars-debugging (format t "~% found ~d equations for ~a."
                                  (length eqns) q)
                (serious-mars-debugging
                 (let ((count 0))
                    (dolist (eqn eqns)
                       (format t "~%  ~d. ~a" (incf count) eqn)))))
     (values t `(found equations ,@ eqns)
       `(((get-example-info ,q)) ;; just in case
         ,@ (mapcar #'(lambda (eqn)
             `(,@ (mapcar #'(lambda (var)
            `(find-nvalue ,var))
              (unknown-eqn-antecedents
               q eqn))
            (use-equation ,q ,eqn)))
         eqns))))
    (t (push `(:no-equation-locally ,q) (notes *state*))
       (values nil nil)))))

(install-try-procedure find-equation-for try-find-equation-for)

(defun find-equation-for-suggestions (goal goal-stack
             &optional  (*mars* *mars*))
  ;; okay, here's where we have to go to the example
  (let ((new-goal `(get-example-info ,(cadr goal))))
    (list (cons new-goal (make-new-state new-goal goal-stack)))))

(install-suggestions-procedure find-equation-for
             find-equation-for-suggestions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; the use-equation goal.
;;
;; form: (use-equation <q> <eqn>)
;; 

(defun use-equation-failed? (goal)
  (declare (ignore goal))
  nil)

(install-failure-procedure use-equation use-equation-failed?)

(defun use-equation-achieved? (goal &optional (*mars* *mars*))
  ;; well, if we already have a value for the goal parameter,
  ;; it's kind of moot.
    (lookup-nvalue (cadr goal) (working-memory *mars*))) ;; was problem

(install-achieved-procedure use-equation use-equation-achieved?)

(defun try-use-equation (goal &optional (*mars* *mars*))
  ;; in nike zenspeak, "just do it"
  (multiple-value-bind (solved? value antecedents)
     (solve-for (second goal) (third goal))
     (cond (solved? ;; install result & indicate success
      (mars-debugging
       (format t "~%  found ~a = ~a." (second goal) value))
      (sme::define-expression
       `(implies ,(if (cdr antecedents)
          `(and ,@ antecedents)
        (car antecedents))
           (nvalue ,(second goal) ,value))
       (working-memory *mars*)) ;; was problem
      (values t `(derived (= ,(second goal) ,value)
        via ,(third goal)) nil))
     (t (push `(:failed-to-solve ,(second goal) ,(third goal)
               ,antecedents)
        (notes *state*))
        (values nil nil nil))))) 

(install-try-procedure use-equation try-use-equation)

(defun use-equation-suggestions (goal goal-stack)
  (declare (ignore goal goal-stack)) nil)
;; use-equation just shouldn't fail

(install-suggestions-procedure use-equation use-equation-suggestions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; the get-example-info goal
;;
;; format: (get-example-info <q>)
;;
;; this is the interesting one.
;;
;; assumptions: initial mappings have already been made.
;; 
;; 1. get from example all expressions that mention q.
;; 2. install them in the  example (was working) memory. 
;; 3. extend the mappings.
;; 4. for each mapping, queue an (examine-cis-for <q> <ci dgroup> <mapping>)

(defun get-example-info-failed? (goal) 
   (lookup `(already-read-example-info-for ,(cadr goal)) 
     (working-memory *mars*))) 

(install-failure-procedure get-example-info get-example-info-failed?)

(defun get-example-info-achieved? (goal)
  (declare (ignore goal))
  nil)

(install-achieved-procedure get-example-info get-example-info-achieved?)

(defun do-get-example-info (goal &optional (*mars* *mars*))
   (let* ((q (cadr goal))
          (any? nil)
          (flag `(already-read-example-info-for ,q)))
      (unless (lookup flag (working-memory *mars*))
         (mars-debugging
           (format t "~%   seeing if example can help with ~a.." q))
         ;; augment the working memory of the example
         (setq any? (grab-example-info-for q))
         (sme::define-expression flag (working-memory *mars*))
         (cond (any? ;; extend the current mappings
                 (mars-debugging
                   (format t "~%  extending interpretations.."))  
                 (sme::incremental-match-with-appropriate-filters (sme *mars*)
                  *mars-match-filter*)
                 (process-prospective-mapping-activities
                  q (find-prospective-mapping-activities
                     q (sme::mappings (sme *mars*)))))
               (t (mars-debugging
                    (format t "~%    no interps help with ~a." q))
                 (push `(:no-interps-for ,q ,(sme::mappings (sme *mars*)))
                   (notes *state*))
                 (values nil nil nil))))))

(defun entity-of (qref) (cadr qref))
;; simplifying assumption, true of cyclepad at least.

(install-try-procedure get-example-info do-get-example-info)

(defun get-example-info-suggestions (goal goal-stack)
  (declare (ignore goal goal-stack))
  nil)

(install-suggestions-procedure get-example-info get-example-info-suggestions)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grab-example-info-for (q &optional (*mars* *mars*)
                               &aux mh titem (any? nil))
   (dolist (mapping (sme::mappings (sme *mars*)) any?)
      (setq titem
        (sme::fetch-item (entity-of q) (problem *mars*)))
      (unless titem
         (mars-debugging
           (format t "~%    weird: ~a doesn't contain match for ~a!"
             mapping (entity-of q)))
         (return nil))
      (setq mh (sme::mh-containing-target-item titem mapping))
     (when mh 
       (mars-debugging 
        (format t "~%    ~a might help, ~a <-> ~a" mapping
          (entity-of q) (sme::user-form (sme::base-item mh))))
       (let ((example-q (list (car q) ;; asn re form of quantities!
                              (sme::user-form (sme::base-item mh)))))
         ;; augment the example memory with information about
         ;; the corresponding quantity
         (dolist (expr (expressions-mentioning
                        example-q (example *mars*)))
           (let ((already?
                  (sme::fetch-item (sme::user-form expr)
                                   (example-memory *mars*))))
             (cond (already?
                    (mars-debugging
                     (format t "~%     [already read ~a.]"
                       (sme::user-form expr))))
                   (t (mars-debugging
                       (format t "~%  from example: ~a"
                         (sme::user-form expr)))
                      (setq any? t)     
                      (sme::define-expression
                          (sme::user-form expr)
                          (example-memory *mars*))))))))))

(defun find-prospective-mapping-activities (q mappings
                                             &optional (*mars* *mars*))
   ;; examines each mapping, seeing if it included a correspondence
   ;; for the target quantity.  if it does, it might have some candidate
   ;; inferences worth exploring, so make note of this prospect.
   (let ((activities nil))
      (dolist (mapping mappings activities)
         (let ((titem (sme::fetch-item q (problem *mars*))))
            (cond ((null titem)
                   (serious-mars-debugging
                    (format t "~%      ignore ~a, no mh for ~a."
                      mapping q))
                   nil)
                  (t ;; ***** the necessity of this next line suggests
                    ;; ***** a rude timing error somewhere!
                    (sme::calculate-mapping-inferences mapping)
                    (push
                      `((examine-cis-for
                          ,q ,(dgroup-for-cis (format nil "cis for ~a from ~a"
                                                q mapping)
                                mapping)
                          ,mapping)) activities)))))))

(defun process-prospective-mapping-activities (q prospects 
                                                &optional (*mars* *mars*))
   (unless prospects
      (mars-debugging
        (format t "~%    no interps relevant for ~a." q))
      (return-from process-prospective-mapping-activities
        (values nil nil nil)))
   (mars-debugging
     (format t "~%    ~d interps relevant for ~a."
       (length prospects) q))
   ;; also cache the dgroups for ci's
   (dolist (prospect prospects)
      ;   (serious-mars-debugging
      ;    (format t "~%potentially relevant mapping:")
      ;    (sme::show-string (fourth (car prospect))))
      (push (third (car prospect))
        (working-memories *mars*)))
   (values t `(found potential mappings
                ,@ (mapcar #'(lambda (x) (fourth (car x)))
                     prospects))
     prospects))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the examine-cis-for goal
;;
;; format: (examine-cis-for <q> <dgroup> <mapping>)
;;
;; this goal looks through the candidate inferences to see if
;; they contain any usable equations for q.
;;

(defun examine-cis-for-failure? (goal)
   (lookup `(already-examined-cis-for ,(cadr goal)) 
     (working-memory *mars*))) 

(install-failure-procedure examine-cis-for examine-cis-for-failure?)

(defun examine-cis-for-achieved? (goal)
  (declare (ignore goal)) nil) ;; same reason

(install-achieved-procedure examine-cis-for examine-cis-for-achieved?)

(defun do-examine-cis-for (goal)
  (let* ((q (cadr goal))
         (ci-dgroup (third goal))
         (mapping (fourth goal))
         (flag `(already-examined-cis-for ,q)))
     (unless (lookup flag (working-memory *mars*)) ;; don't do redundant work
        ;; first see if we can grab an easy answer for it via modeling asns.
        (sme::define-expression flag (working-memory *mars*))
        (multiple-value-bind (found-it? value reason)
            (find-modeling-asn-derivation-for q ci-dgroup)
           (when found-it? ;; ***** smarter student would verify!
              (sme::define-expression `(derived-by
                                        (nvalue ,q ,value)
                                        ,reason)
               (working-memory *mars*)) ;; was problem
              (return-from do-examine-cis-for
                (values t `(assumed (= ,q ,value) via ,reason) nil))))
        (multiple-value-bind (eqns eqn-exprs)
            (find-equations-mentioning q ci-dgroup)
          (declare (ignore eqn-exprs))
           (unless eqns
              (serious-mars-debugging
               (format t "~%   [no new equations for ~a from ~a..]"
                 q ci-dgroup))
              (return-from do-examine-cis-for (values nil nil nil)))
           (values t `(imported equations from ,mapping ,@ eqns)
             (mapcar
               #'(lambda (eqn)
                   (mars-debugging
                    (format t "~%   for ~a, maybe ~a.." q eqn))
                   ;; design choice: should check if useful, by looking for
                   ;; constraining relations.  but to begin with, just go with
                   ;; it.  this models a really bad student.
                   ;; first, copy the equation to the problem
                   ;; (and, yes, w/o the antecedents!)
                   ;; ****** wrapped equation around the equation
                   ;; ******  being added, so that an equation brought
                   ;; ******  into the problem will be recognized as such
                   ;; ******  even if used later.
                   (sme::define-expression `(equation ,eqn) 
                    (working-memory *mars*))
                   ;;;; not sure if this is the "right" thing
                   (sme::define-expression `(equation ,eqn) 
                    (problem *mars*))
                   (nconc (mapcar #'(lambda (var)
                                      `(find-nvalue ,var))
                            (unknown-eqn-antecedents q eqn))
                     (list `(use-equation ,q ,eqn))))
               eqns))))))
   
(defun old-do-examine-cis-for (goal)
  (let ((q (cadr goal))
  (ci-dgroup (third goal))
  (mapping (fourth goal))
  (ma-reason nil)
  (eqn-reason nil)
  (eqn-nexts nil)
  (eqns-found nil))
    (multiple-value-bind (found-it? value reason)
       (find-modeling-asn-derivation-for q ci-dgroup)
       (when found-it? ;; ***** smarter student would verify!
       ;; grab numerical answer via modeling assumption
       (sme::define-expression `(derived-by
               (nvalue ,q ,value)
               ,reason)
             (problem *mars*))
       (setq ma-reason `(assumed (= ,q ,value) via ,reason))))
    (dolist (eqn-entry (lookup '(equation . ?rest) ci-dgroup))
     (let ((eqn (copy-tree (sme::user-form (car eqn-entry)))))
       (unless (sme::fetch-item eqn (problem *mars*)) ;; not already there
    ;; design choice: should check if useful, by looking for
    ;; constraining relations.  but to begin with, just go with
    ;; it.  this models a really bad student.
    ;; first, copy the equation to the problem
    ;; (and, yes, w/o the antecedents!)
    ;; ***** need to isolate the subset of correspondences it relies on.
  (sme::define-expression `(implies ,ci-dgroup ,eqn)
        (problem *mars*))
  ;; eqn = (equation <actual equation>)
  (push (cadr eqn) eqns-found)
  (mars-debugging
     (format t "~%   imported ~a.." eqn))
  (when (occurs-in? q eqn) ;; queue up current for use
        (push (nconc (mapcar #'(lambda (var)
               `(find-nvalue ,var))
           (unknown-eqn-antecedents q (cadr eqn)))
         (list `(use-equation ,q ,(cadr eqn))))
        eqn-nexts)))))
    (cond (eqns-found
     (setq eqn-reason
     `(imported equations from ,mapping ,@ eqns-found))
     (push eqn-reason (notes *state*)))
    (t (push `(:no-new-equations ,q ,ci-dgroup ,mapping)
       (notes *state*))))
    (cond (ma-reason
     (cond (eqn-reason
      (values t `(and ,ma-reason ,eqn-reason)
        eqn-nexts))
     (t (values t ma-reason nil))))
    (eqn-reason
     (values t eqn-reason eqn-nexts))
    (t (values nil nil nil)))))


(install-try-procedure examine-cis-for do-examine-cis-for)

(defun examine-cis-for-suggestions (goal goal-stack)
  (declare (ignore goal goal-stack)) nil)
;; if there was something to do, it was taken care of above. 

(install-suggestions-procedure examine-cis-for examine-cis-for-suggestions)




