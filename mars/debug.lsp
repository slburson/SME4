;; -*- mode: lisp; -*- 

;;;; MARS: debugging routines
;;
;; last edited 2015, by kdf

(in-package :common-lisp-user)

(defun show-em (&optional (*mars* *mars*))
  (pprint (sme::lisp-form (example-memory *mars*))))

(defun show-ex (&optional (*mars* *mars*))
  (pprint (sme::lisp-form (example *mars*))))

(defun show-pr (&optional (*mars* *mars*))
  (pprint (sme::lisp-form (problem *mars*))))

(defun show-wm (&optional (*mars* *mars*))
  (mapc #'(lambda (dgroup)
      (pprint (sme::lisp-form dgroup)))
  (working-memories *mars*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; checking to see if ci's fully exploited

(defun equations-not-in-problem (ci-dgroup &optional (*mars* *mars*))
  (let ((result nil))
    (dolist (eqn-entry (lookup '(equation . ?x) ci-dgroup) result)
     (unless (sme::fetch-item (sme::user-form (car eqn-entry))
            (working-memory *mars*))
       (push (car eqn-entry) result)))))

(defun unused-ci-equations (&optional (*mars* *mars*)
              &aux (count 0))
  (dolist (wm (working-memories *mars*))
   (format t "~%for ~a:" wm)
   (dolist (eqn-expr (equations-not-in-problem wm *mars*))
     (format t "~% ~d: ~a" (incf count)
       (sme::user-form eqn-expr)))))

(defun nvalues-sought (&optional (*mars* *mars*))
  (let ((result nil))
    (dolist (state (states *mars*) result)
     (if (eq (car (current-goal state))
       'find-nvalue)
   (pushnew (cadr (current-goal state))
      result :test #'equal)))))

(defun nvalues-failed (&optional (*mars* *mars*))
  (let ((losers nil))
    (dolist (q (nvalues-sought *mars*) losers)
     (multiple-value-bind (known? value expr)
         (lookup-nvalue q (working-memory *mars*))
       (declare (ignore expr value))
    (unless known? (push q losers))))))

(defun nvalues-derived (&optional (*mars* *mars*))
   (let ((winners nil))
      (dolist (q (nvalues-sought *mars*) winners)
         (multiple-value-bind (known? value expr)
             (lookup-nvalue q (working-memory *mars*))
           (declare (ignore expr))
           (when known? (push (cons q value) winners))))))

(defun nvalues-given (&optional (*mars* *mars*))
  (let ((givens nil))
    (dolist (headed-alist (lookup `(given (nvalue ?q ?v))
                                  (working-memory *mars*)) givens)
      ;; (<exp> . <alist>)
      (push (cons (cdr (assoc '?q (cdr headed-alist)))
                  (cdr (assoc '?v (cdr headed-alist))))
            givens))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; test cases

(defun duh ()
  (mars-init) ;; Ensures vocabulary already defined
  (create-mars "test: detecting known value"
     '(mass-flow foo)
     (sme::define-description "duh problem" nil
            '((nvalue (mass-flow foo) 3.1415)))
     (sme::define-description "duh example" nil nil) ;; no need
     (sme::define-description "duh kb" nil nil)) ;; ah yup 
  (try))

(defun duh2 ()
  (mars-init) 
  (create-mars "test: extracting values from the kb"
     '(molar-mass air)
     (sme::define-description "duh problem 2" nil '())
     (sme::define-description "duh example" nil nil) ;; no need
     *mars-kb*)
  (try))

(defun duh3 ()
  (mars-init) 
  (create-mars "test: using an equation from the problem"
     '(nu-thermal cycle)
     (sme::define-description "duh problem 3" nil
            '((equation (= (nu-thermal cycle)
            (/ (net-work cycle)
               (q-in cycle))))
              (nvalue (net-work cycle) 10.0)
              (nvalue (q-in cycle) 100.0)))
     (sme::define-description "duh example" nil nil) ;; no need
     *mars-kb*)
  (try))

(defun duh4 ()
  (mars-init) 
  (create-mars "test: using an equation from the problem"
     '(nu-thermal cycle)
     (sme::define-description "duh problem 4" nil
            '((nvalue (net-work cycle) 10.0)
              (nvalue (q-in cycle) 100.0)))
     (sme::define-description "duh example" nil nil) ;; no need
     (sme::define-description "duh4 kb" nil
            '((equation (= (nu-thermal cycle)
               (/ (net-work cycle)
                  (q-in cycle))))))
     )
  (try))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; analogy examples

(defun atest1 (&optional (debugging? t))
  (mars-init) 
  (sme::with-description-file-information *mars-pathname* "dgr"
    (create-mars "test: simple heater analogical problem"
                 '(q heater2)
                 (sme::dgroup-from-file "heater-pr")
                 (sme::dgroup-from-file "heater-ex")
                 *mars-kb*
                 debugging?)
    (try)))

(defun atest2 (&optional (debugging? t))
  (mars-init) 
  (sme::with-description-file-information *mars-pathname* "dgr"
    (create-mars "test: simple compressor analogical problem"
                 '(t c2-out)
                 (sme::dgroup-from-file "comp-pr")
                 (sme::dgroup-from-file "comp-ex")
                 *mars-kb*
                 debugging?)
    (try)))

(defun atest3 (&optional (debugging? t))
  (mars-init) 
  (sme::with-description-file-information *mars-pathname* "dgr"
    (create-mars "test: simple turbine analogical problem"
                 '(shaft-work turbine2)
                 (sme::dgroup-from-file "turb-pr")
                 (sme::dgroup-from-file "turb-ex")
                 *mars-kb*
                 debugging?)
    (try)))
   
(defun atest4 (&optional (debugging? t))
  (mars-init) 
  (sme::with-description-file-information *mars-pathname* "dgr"
    (create-mars "test: simple gas turbine analogical problem"
     '(nu-thermal cycle)
     (sme::dgroup-from-file "gasturpr")
     (sme::dgroup-from-file "gasturex")
     *mars-kb*
     debugging?)
    (try)))


