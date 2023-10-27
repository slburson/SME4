;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: abstraction.lsp
;;;;    System: SME V4
;;;;    Author: Ken Forbus
;;;;   Created: June 21, 2003 13:25:06
;;;;   Purpose: Construct minimal abstractions
;;;; ---------------------------------------------------------------------------
;;;;  modified: Tuesday, July 22, 2008 at 15:40:17 by forbus
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;; A minimal abstraction is the overlap computed by a mapping.
;;; Design issue: Do we leave in the base entities, the target entities,
;;;  cons up new entities, or install variables?
;;; This version creates variables.  Easy to change.

(defmethod minimal-abstraction ((mapping t)) nil)

(defmethod minimal-abstraction ((mapping sme))
  (minimal-abstraction (car (mappings mapping))))

(defmethod minimal-abstraction ((mapping mapping))
  (let ((*entity-table* (cons nil nil)))
    (declare (special *entity-table*))
    (mapcar #'construct-abstraction-for-mh
      (remove-if-not 'root-mh? (mhs mapping)))))

(defun construct-abstraction-for-mh (mh)
  (declare (special *entity-table*))
  (cond ((expression-mh? mh)
         (cons (if (and (listp (rationale mh))
                        (eq (car (rationale mh)) :minimal-ascension))
                 (cadr (rationale mh))
                 (construct-abstraction-for-functor-mh (functor-mh mh)))
               (mapcar 'construct-abstraction-for-mh (arguments mh))))
        ((entity-mh? mh)
         (let ((entry (assoc (base-item mh) (cdr *entity-table*)
                             :test 'equal)))
           (cond (entry (cdr entry))
                 (t (let ((var (make-abstraction-variable (base-item mh))))
                      (push (cons mh var) (cdr *entity-table*))
                      var)))))
        (t (error "Unknown MH type: ~A" mh))))

(defun construct-abstraction-for-functor-mh (mh)
  (cond ((eq (base-item mh) (target-item mh))
         (lisp-form (base-item mh)))
        (t ;; Otherwise, hard to know what to do.
         ;; Pressure/temperature case for example.
         ;; We'll use the target, in that that is the preferred
         ;; direction for candidate inferences
         (lisp-form (target-item mh)))))

(defun make-abstraction-variable (thing)
  ;; Actually tempting to not intern these, since they
  ;; are localized.  But they're a bit ugly, so we'll intern
  ;; them until such time as we find any performance issue.
  (intern (format nil "?~A" (lisp-form thing))
          (find-package :data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Saving base abstractions as files

(defun concrete-abstraction->dgroup-file (mapping name file-out
                                                  &key (which :base))
  (let ((sme (sme mapping))
        (entity-table nil)
        (entities (ecase which
                    (:base (list-entities-in-mapping mapping
                                                        :which :base))
                    (:target (list-entities-in-mapping mapping
                                                          :which :target))
                    (:generalize (list-entities-in-mapping mapping
                                                           :which :base)))))
    (when (eq which :generalize)
      (setq entity-table
        (construct-generalized-entity-table-from-mapping mapping)))
    (with-open-file (fout file-out
                          :direction :output
                          :if-exists :supersede)
      (let ((*print-right-margin* 10000))
        (format fout ";;; ~A Abstraction from ~A, ~A/~A"
          which mapping (name (base sme)) (name (target sme))))
      (format fout "~%~%(in-package :cl-user)~%")
      (format fout "~%(sme::defDescription ~A" name)
      (format fout "~% entities ~S"
        (if entity-table (sublis entity-table entities) entities))
      (format fout "~% expressions (")
      (dolist (exp
               (if (eq which :generalize)
                 (sublis entity-table
                         (list-expressions-in-mapping mapping
                                                      :which :base))
                 (list-expressions-in-mapping mapping :which which)))
        (format fout "~%   ~S" exp))
      (format fout "~%   ))~%"))))

(defun construct-generalized-entity-table-from-mapping (mapping)
  ;; Since we're just plugging into the base, we build the alist
  ;; using base entities.
  (let ((alist nil))
    (dolist (mh (mhs mapping) alist)
      (when (entity-mh? mh)
        (unless (equal (name (base-item mh))
                       (name (target-item mh)))
          ;; Don't substitute if identical entities
          ;; (Unique internal name assumption, standard)
          (push (cons (name (base-item mh))
                      (gentemp "genent" (find-package :cl-user)))
                alist))))))

(defun list-entities-in-mapping (mapping &key (which :base))
  (let ((entities nil))
    (dolist (mh (mhs mapping) entities)
      (when (entity-mh? mh)
        (push (sme::lisp-form
               (if (eq which :base)
                 (sme::base-item mh)
                 (sme::target-item mh)))
              entities)))))

(defun list-expressions-in-mapping (mapping &key (which :base))
  (let ((expressions nil))
    (dolist (mh (mhs mapping) expressions)
      (when (expression-mh? mh)
        (push (sme::lisp-form 
               (if (eq which :base)
                 (sme::base-item mh)
                 (sme::target-item mh)))
              expressions)))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
