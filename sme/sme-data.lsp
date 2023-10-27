;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: sme-data.lsp
;;;;    System: CogSketch
;;;;    Author: Andrew Lovett
;;;;   Created: January 25, 2009 16:08:31
;;;;   Purpose: Code for gathering extra data about an sme
;;;; ---------------------------------------------------------------------------
;;;;  modified: Sunday, January 25, 2009 at 16:38:09 by aml758
;;;; ---------------------------------------------------------------------------

(in-package :sme)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;Top-Level Functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Info on the size of the base and target,
;;;;;;;useful for normalizing

;;SES for the base, should it be matched to itself
(defun base-size (sme)
  (self-score-dgroup (base sme) (mapping-parameters sme)))

;;SES for the target, should it be matched to itself
(defun target-size (sme)
  (self-score-dgroup (target sme) (mapping-parameters sme)))

;;Number of expressions in the base
(defun base-num-expressions (sme)
  (expression-counter (base sme)))

;;Number of expressions in the target
(defun target-num-expressions (sme)
  (expression-counter (target sme)))

;;Number of entities in the base
(defun base-num-entities (sme)
  (entity-counter (base sme)))

;;Number of entities in the target
(defun target-num-entities (sme)
  (entity-counter (target sme)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;Info on the global mappings

(defun num-mappings (sme)
  (length (mappings sme)))

(defun score-first-mapping (sme)
  (if (mappings sme)
    (score (car (mappings sme)))
    0))

(defun score-second-mapping (sme)
  (if (cadr (mappings sme))
    (score (cadr (mappings sme)))
    0))

(defun score-third-mapping (sme)
  (if (caddr (mappings sme))
    (score (caddr (mappings sme)))
    0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;Info on the kernels

(defun num-kernels (sme)
  (length (kernel-mappings sme)))


;;mean SES of kernels (sme)
(defun mean-kernel-score (sme)
  (mean (mapcar #'score (kernel-mappings sme))))

;;mean # of nogoods for a kernel
(defun mean-kernel-nogoods (sme)
  (mean (mapcar #'num-nogoods (kernel-mappings sme))))


;;Gives the ratio of the first kernels's score
;;to the second kernel's score
(defun top-second-kernel-ratio (sme)
  (if (cadr (kernel-mappings sme))
    (/ (score (car (kernel-mappings sme)))
       (score (cadr (kernel-mappings sme))))))


;;Gives the ratio of the first kernel's score
;;to the mean kernel score
(defun top-mean-kernel-ratio (sme)
  (if (kernel-mappings sme)
      (/ (score (car (kernel-mappings sme)))
         (mean-kernel-score sme))))


;;Gives the standard deviation of the kernel scores
(defun kernel-std-dev (sme)
  (std-dev (mapcar #'score (kernel-mappings sme))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Helper Fns

(defun num-nogoods (mapping)
  (set-size (nogoods mapping)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;Stat functions


;;returns the mean of a list of numbers
(defun mean (l)
  (/ (apply #'+ l) (* 1.0 (length l))))


;;returns the variance for a list of numbers
;;where variance = (standard deviation)^2
;;
;;the mean is an optional second argument
(defun variance (l &optional (m (mean l)))
  (/
   (apply #'+ (mapcar #'(lambda (x) (expt (- x m) 2))
                l))
   (1- (length l))))


;;returns the standard deviation of a list of numbers
(defun std-dev (l &optional (m (mean l)))
  (sqrt (variance l m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code