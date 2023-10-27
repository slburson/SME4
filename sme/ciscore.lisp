;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                             -*-
;;;; ------------------------------------------------------------------------------
;;;; File name: ciscore.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Kenneth D. Forbus
;;;;   Created: Aug 24, 1997
;;;;  $LastChangedDate: 2013-06-03 18:38:43 -0500 (Mon, 03 Jun 2013) $
;;;;  $LastChangedBy: aml758 $
;;;;   Purpose: Candidate inference evaluation
;;;; ------------------------------------------------------------------------------

(in-package :sme)

;;;; Structural evaluation of candidate inferences

;;;; Entry point

(defun structurally-evaluate-candidate-inference (ci)
  (let ((params (mapping-parameters ci)))
    (setf (ci-shadow-cache ci) (compute-ci-shadow-structure ci))
    (setf (extrapolation-score ci)
      (compute-ci-extrapolation-score ci))
    (if (ci-support-scores-global? params)
      (setf (support-score ci)
        (ci-global-support-score ci))
      (setf (support-score ci)
        (compute-ci-support-score ci)))))

;;; Computation of support and extrapolation scores

(defmethod compute-ci-support-score ((ci candidate-inference))
   (compute-ci-scores (mhs ci) (mapping-parameters ci))
   (reduce #'+ (mapcar 'ci-score (mhs ci)) :initial-value 0))

(defmethod compute-ci-extrapolation-score ((ci candidate-inference))
   (let ((pieces (append (ci-shadow-cache ci) (mhs ci))))
      (compute-ci-scores pieces (mapping-parameters ci))
      (let ((inside-score (reduce '+ (mapcar 'ci-score (mhs ci)) :initial-value 
                            0))
            (outside-score (reduce '+ (mapcar 'ci-score (ci-shadow-cache ci))
                                   :initial-value 0)))
         (/ outside-score
            (+ inside-score outside-score)))))

;;;; Creating the shadow structure cache
;; The shadow cache is used in computing the extrapolation score.
;; It holds the score information for the "outside" of the inference.
;; Since relatively few base items are likely to be in a candidate inference
;; at any particular time, replicating the relevant structural relationships
;; for the outside seems cheaper in storage than adding a CI-score field to every 
;; predicate, entity, and expression.

(defclass ci-shadow-entry ()
    ((key :initform nil :initarg :key :accessor key
      :documentation "Item in the base")
     (ci-score :initform 0.0 :initarg :ci-score :accessor ci-score
      :documentation "Score as part of CI")
     (children :initform nil :initarg :children :accessor children
      :documentation "Shadow entry or MH's for arguments/predicate")))

(defmethod order ((thing ci-shadow-entry))
   (let ((item (key thing)))
      (cond ((expression? item) (order item))
            (t 0))))

;;;; Generating the CI shadow cache

(defun compute-ci-shadow-structure (ci)
   (multiple-value-bind (entry cache)
       (compute-ci-shadow-structure-for (form ci)
        (mapping ci) nil)
      (declare (ignore entry))
      (sort cache #'(lambda (x y) (> (order x)
                                     (order y))))))

(defun compute-ci-shadow-structure-for (form mapping cache
                                         &aux current-entry)
   (cond ((setq current-entry
            (find form cache :test 'equal :key 'key))
          (values current-entry cache))
         ((and (setq current-entry
                 (mh-containing-target-item form mapping))
               (mh? current-entry))
          (values current-entry cache))
         (t ;; must create it
           (setq current-entry
             (make-instance 'ci-shadow-entry :key form))
           (push current-entry cache)
           (cond ((listp form)
                  ;; Part of CI, so map to get children
                  (multiple-value-bind (children new-cache)
                      (compute-ci-shadow-list form mapping cache)
                     (declare (ignore new-cache))
                     (setf (children current-entry) children)
                     (values current-entry cache)))
                 ((expression? form) ;; deal with children
                  (multiple-value-bind (children new-cache)
                      (compute-ci-shadow-list
                       (cons (predicate form)
                         (mapcar #'cdr (arguments form)))
                       mapping cache)
                     (declare (ignore new-cache))
                     (setf (children current-entry) children))
                  (values current-entry cache))
                 (t (values current-entry cache))))))

(defun compute-ci-shadow-list (forms mapping cache &aux children)
   (dolist (constituent forms (values children cache))
      (multiple-value-bind (child new-cache)
          (compute-ci-shadow-structure-for 
           constituent mapping cache)
         (push child children)
         (setq cache new-cache))))

(defun display-ci-shadow-cache (ci &optional (stream *standard-output*))
   (dolist (entry (ci-shadow-cache ci))
      (format stream "~%~A~%  ~A, ~A~%  {~A}"
        (key entry)
        (order entry)
        (ci-score entry)
        (children entry))))

;;;; Manipulating scores in the CI shadow cache

(defun clear-ci-scores (list-of-things)
   (dolist (entry list-of-things) (setf (ci-score entry) 0.0)))

(defun compute-ci-scores (list-of-things mapping-parameters)
   ;; Assumes list-of-things is sorted, in decreasing order
   (add-ci-local-evidence list-of-things mapping-parameters)
   (add-ci-trickle-down list-of-things mapping-parameters)
   list-of-things)

;;; Local evidence computation

(defun add-ci-local-evidence (list-of-things mapping-parameters)
   (dolist (entry list-of-things)
      (add-ci-local-evidence-for entry mapping-parameters)))

(defmethod add-ci-local-evidence-for ((entry ci-shadow-entry)
                                      (mapping-parameters parameters))
   (setf (ci-score entry) (same-functor mapping-parameters)))

(defmethod add-ci-local-evidence-for ((mh match-hypothesis)
                                      (mapping-parameters parameters))
   (with-slots (target-item base-item) mh
     (with-slots (same-functor same-function) mapping-parameters
       (cond ((or (not (expression? target-item)) 
                  (not (expression? base-item)))
              (setf (ci-score mh) same-functor))
             ((and (function? (predicate target-item))
                   (not (eq (predicate target-item)(predicate base-item))))
              (setf (ci-score mh) same-function))
             (t (setf (ci-score mh) same-functor))))))

;;; Trickle-down computation

(defun add-ci-trickle-down (list-of-things mapping-parameters)
   ;; Assumes list-of-things is sorted by order
   (dolist (entry list-of-things)
      (add-ci-trickle-down-for entry mapping-parameters)))

(defmethod key ((mh match-hypothesis)) (base-item mh))

(defun add-ci-trickle-down-for (entry mapping-parameters)
   (let ((children (children entry))
         (max-score (max-local-score mapping-parameters))
         (trickle-down (trickle-down mapping-parameters)))
      (when children
         (let ((td-score (* trickle-down (ci-score entry))))
            (dolist (child-entry children)
               (when (or (not (predicate? (key child-entry)))
                         (functor-trickle-down? mapping-parameters))
                  (add-ci-score-evidence entry td-score max-score)))))))

(defun add-ci-score-evidence (entry amount max-score)
  (setf (ci-score entry)
    (min-without-nils max-score (+ (ci-score entry) amount))))

;;;; Lovett's experimental alternate method for computing support score.
(defun ci-global-support-score (ci)
  (apply #'+ (mapcar #'score (descendants ci))))

(defmethod descendants ((ci candidate-inference))
  (let ((descendants (descendants (car (mhs ci))))
        (others (apply #'append 
                       (mapcar #'descendants (cdr (mhs ci))))))
    (dolist (other others descendants)
      (pushnew other descendants :test #'eq))))
     
;;For backwards compatibility
(defun ci-support-score (ci)
  (ci-global-support-score ci))

;;;;;; END OF FILE


   
   
   

