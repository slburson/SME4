;; -*- mode: lisp -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; sme description dumper for cyclepad
;;
;;; last edited by yusuf on wed oct 12 14:46:27 1994

;; highly experimental

(in-package :cpad)

;; goal is to produce dgroup files from cyclepad analyses.

;; ***** need to add: conditions under which parameters
;; ***** exist.

(defun userize (form)
  (cond ((null form) nil)
  ((symbolp form) (intern (symbol-name form)
        :common-lisp-user))
  ((consp form) (cons (userize (car form))
          (userize (cdr form))))
  ((stringp form)
   (substitute #\-  #\   form))
  (t form)))

(defun dump-cyclepad-dgroup (filename problem? goal)
  (with-open-file (fout filename :direction :output)
   (let ((*package* (find-package :common-lisp-user))) 
    (dump-cyclepad-dgroup-header fout problem? goal)
    (dump-cyclepad-structural-information fout problem? goal)
    (dump-cyclepad-component-information fout problem? goal)
    (dump-cyclepad-stuff-information fout problem? goal)
    (dump-cyclepad-numerical-information fout problem? goal)
    (finish-cyclepad-dgroup-dump fout problem? goal))))

(defun dump-cyclepad-dgroup-header (fout problem? goal)
  (format fout ";; -*- mode: lisp -*-~%~%")
  (format fout "~%;;;; cyclepad dgroup dump -- ~a, ~a"
    (datestamp-string) (timestamp-string))
  (format fout "~%;;; ~a, with goal to find ~a."
    (if problem? "problem to be solved"
      "worked example")
    (thstring goal))
  (format fout "~%~%(in-package :common-lisp-user)")
  (format fout "~%(sme::defdescription ~a"
    (userize (cyclepad-name *cyclepad*))))

(defun finish-cyclepad-dgroup-dump (fout problem? goal)
  (if problem?
      (format fout "~% (goal-parameter ~a)" (car goal)))
  (format fout "~% )"))

(defun dump-cyclepad-structural-information (fout problem? goal)
  ;; dump list of parts
  (format fout "~%;; parts list")
  (dolist (part (cons :cycle (cycle-components)))
   (format fout "~% ~a"
     (userize (car (fetch `(,(type-of-part part) ,part . ?roles))))))
  ;; dump list of stuffs
  (format fout "~%;; stuffs list") 
  (dolist (stuff (cycle-stuffs))
   (format fout "~% (thermodynamic-stuff ~a)" (userize stuff))))

(defun dump-cyclepad-component-information (fout problem? goal
              &aux (counter 0))
  ;; find flow processes
  (format fout "~%;; physical processes") 
  (dolist (heat-flow (fetch '(heat-flow . ?x)))
   (format fout "~% ~a" (userize heat-flow)))
  (dolist (fluid-flow (fetch '(fluid-flow . ?x)))
   (format fout "~% ~a" (userize fluid-flow)))
    (dolist (fluid-flow (fetch '(compression . ?x)))
   (format fout "~% ~a" (userize fluid-flow)))
   (dolist (fluid-flow (fetch '(expansion . ?x)))
   (format fout "~% ~a" (userize fluid-flow)))
   (format fout "~%;; modeling assumptions.")
   (dolist (part (cons :cycle (cycle-components)))
     (dolist (asn-type (modeling-assumptions-for part))
       (let ((form (list asn-type part)))
   (if (true? form)
       (format fout "~% ~a" (userize form))
     (if (false? form)
         (format fout "~% ~a" (userize `(:not ,form)))))))))

(defun dump-cyclepad-stuff-information (fout problem? goal)
  (format fout "~% ;; substance and phase information") 
  (dolist (stuff (cycle-stuffs))
   (format fout "~% ~a" (userize (make-substance-form-for stuff)))
   (format fout "~% ~a" (userize (make-phase-form-for stuff)))))

;;;; dumping information about parameters

;; some of cyclepad's background information must be filtered out, since
;; it will be supplied by some other means.

(defvar *parameter-types-filtered* '(cv cp gamma molar-mass r))

(defun list-parameters ()
  (mapcar #'cadr (remove-if-not #'true? (fetch '(parameter ?x)))))

(defun list-equations ()
  (remove-if-not #'true? (fetch '(equation ?x))))

(defun dump-cyclepad-numerical-information (fout problem? goal)
  (format fout "~%;; numerical values")
  (dolist (p (if problem? ;; use cycle parameters only
     (select-parameters-for-problem-dgroup goal)
         (select-parameters-for-example-dgroup goal)))
    (multiple-value-bind (value form)
       (get-value p)
       (when (and (floatp value)
      (not (member (car p) *parameter-types-filtered*)))
  (pprint (get-abstracted-nvalue-rationale form)
    fout))))
  (format fout "~%;; rationale for equations")
  (dolist (eqn (if problem?
       (select-equations-for-problem-dgroup)
     (select-equations-for-example-dgroup goal)))
    (when (and (consequences-of eqn)
         (not (member (car (cadr (cadr eqn)))
          *parameter-types-filtered*)))
    (pprint (get-abstracted-equation-rationale eqn)
      fout))))

;;;; filter criteria for problems and examples

(defun select-parameters-for-problem-dgroup (goal)
  ;; any assumptions provided by the user should be fair game.
  (let ((assumed-parameters nil))
    (dolist (p (list-parameters) assumed-parameters)
     (multiple-value-bind (value form)
       (get-value p)
       (when (and (floatp value)
      (assumption? form))
       (push p assumed-parameters))))))
  

(defun select-parameters-for-example-dgroup (goal)
  ;; only select those implicated in the goal.
  (cons (car goal)
  (mapcar #'car 
    (subset-of-antecedents-satisfying
     #'(lambda (ante) (eq (cadr ante) 'nvalue)) goal))))

(defun select-equations-for-problem-dgroup (&aux (eqns nil))
  ;; only want those involving the cycle itself, as a way of
  ;; simulating what an other-than-totally-brain-dead student would do.
  (dolist (eqn (fetch-trues '(equation ?eqn)) eqns)
   (when (contains-symbol? (cadr eqn) :cycle)
   (push eqn eqns))))
  
(defun select-equations-for-example-dgroup (goal)
  (subset-of-antecedents-satisfying
   #'(lambda (ante) (eq (car ante) 'equation)) goal))

(defun contains-symbol? (form sym)
  (cond ((null form) nil)
  ((eq form sym) t)
  ((not (listp form)) nil)
  (t (or (contains-symbol? (car form) sym)
         (contains-symbol? (cdr form) sym)))))

;;;; procedures that actually dump nvalue and equation information

(defun get-abstracted-nvalue-rationale (form)
  (let ((proposal (antecedents-of form)))
    (userize
     (translate-from-nvalues
     (if proposal
         (let ((antes (antecedents-of (car proposal))))
   `(derived-by ,form ,(if (cdr antes) `(and ,@ antes)
             (car antes))))
       `(given ,form))))))

(defun translate-from-nvalues (form)
  (cond ((null form) nil)
  ((listp form)
   (cond ((and (listp (cdr form))
         (eq (cadr form) 'nvalue))
    `(nvalue ,(car form) ,(third form)))
         ((eq (car form) '==) (list 'equation (cons '= (cdr form))))
         ((or (eq (car form) 'saturation-table-for)
        (eq (car form) 'superheated-table-for))
    (list (car form) (cadr form))) ;; lose the datastructure
         ((eq (cadr form) 'members)
    `(members ,(car form) (the-set ,@ (third form))))
         ((eq (cadr form) 'cwa)
    `(cwa ,(car form) (the-set ,@ (third form))))
         (t (cons (translate-from-nvalues (car form))
      (translate-from-nvalues (cdr form))))))
  (t form)))
       
(defun get-abstracted-equation-rationale (form)
  (let ((antes (process-structural-antecedents (antecedents-of form))))
    (userize
     `(:implies ,(if (cdr antes)
        `(:and ,@ antes)
      (car antes))
         ,form))))

(defun process-structural-antecedents (thing)
  (apply #'nconc ;; because some might expand into multiple expressions
   (mapcar #'(lambda (form)
         (cond ((eq (cadr form) 'members)
         `((members ,(car form) (the-set ,@ (third form)))))
         ((eq (cadr form) 'cwa)
          `((cwa ,(car form) (the-set ,@ (cdr form))))) 
         (t (list form))))
     thing)))

;;; dumping quantity types

(defun lexicalize-quantity-types (&optional (stream *standard-output*))
  (dolist (qt *quantity-types*)
    (format stream "~%(sme:defpredicate ~a (thing) function)"
      (quantity-type-form qt))))

(defun lexicalize-modeling-asns  (&optional (stream *standard-output*))
  (dolist (asn-type
     (remove-duplicates
      (apply #'append
       (mapcar #'(lambda (type)
             (third
              (car
               (fetch `(,type modeling-assumptions
                  ?asns)))))
         (types-of-components)))))
    (format stream "~%(sme:defpredicate ~a (component) attribute)"
      asn-type)))

(defun lexicalize-component-types (&optional (stream *standard-output*))
  (dolist (type (types-of-components))
    (format stream "~%(sme:defpredicate ~a (thing) attribute)"
      type)))

;;;; clim stuff

(defvar *sme-dump-pathname* "~/example.dgr")

(clim::define-command (sme-dgroup-dump
           :menu "dump dgroup"
           :command-table analyze
           :name "dump an sme dgroup") ()
 (let ((goalq (menu-select-parameter
         (sort (possible-dependent-parameters)
         #'number-sort-order)
         "please select the goal parameter"))
       (problem? nil)
       (stream *standard-output*))
   (clim:accepting-values
    (stream :own-window '(:right-margin (10 :character))
      :exit-boxes '((:exit "<    ok    >") (:abort "<  cancel  >"))
      :label "please specify the sme dgroup")
    (format stream "~%")
    (setq problem? (clim:accept 'clim:boolean
        :prompt "is this a problem to be solved?"
        :default problem?))
    (format stream "~%")
    (setq *sme-dump-pathname*
    (clim:accept 'clim:string
           :prompt "file"
           :default *sme-dump-pathname*)))
   (multiple-value-bind (value goal-form)
     (get-value goalq)
   (dump-cyclepad-dgroup
    *sme-dump-pathname* problem? goal-form))))
   

          






