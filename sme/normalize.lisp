;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: normalize.lsp
;;;;    System: SME
;;;;   Version: 4.0
;;;;    Author: Andrew Lovett
;;;;   Created: 
;;;;  $LastChangedDate: 2014-07-21 23:56:15 -0500 (Mon, 21 Jul 2014) $
;;;;  $LastChangedBy: liang $
;;;;   Purpose: Code for normalizing SME scores
;;;; --------------------------------------------------------------------------
(in-package :sme)


;;To normalize an SME structural evaluation score, calculate the self-match score
;;for the base and target (i.e., the score for matching each dgroup to itself),
;;then calculate:
;;score * 2 / (+ base-score + target-score)
;;
;;Note that we do not actually run SME to get the self-match score, as this would
;;be less efficient and possibly less accurate.
;;
;;Also, if the mapping parameter block-most-out-of-mapping-contributions? is 
;;not set, then you may end up with out-of-mapping contributions pumping up your score,
;;resulting in a normalized score above 1.
 
(defparameter *normalize-with-geometric-mean* nil)

;;A normalized SES, given a mapping and the sme it came from
(defun normalized-mapping-score (mapping sme &key replacement-score)
  (when (null mapping)
    (return-from normalized-mapping-score 0))
  
  (if *normalize-with-geometric-mean*
      ;;instead of using arithmetic mean (x + y)/2, use
      ;;geometric mean (x + y)^(0.5) to get better continuity from
      ;;content vector and feature-only representations (MAC/FAC) to
      ;;structured representation. 
      (let* ((score (or replacement-score (if mapping (score mapping) 0)))
             (params (mapping-parameters sme))
             (divisor (* (expt (self-score-dgroup (base sme) params) 0.5)
                         (expt (self-score-dgroup (target sme) params) 0.5))))
        (if (zerop divisor)
            0
            (/ score divisor)))
      ;;arithmetic mean 
      (let* ((score (or replacement-score (if mapping (score mapping) 0)))
             (params (mapping-parameters sme))
             (divisor (+ (self-score-dgroup (base sme) params)
                         (self-score-dgroup (target sme) params)))) 
        (if (zerop divisor)
            1
            (/ (* score 2) divisor)))))

(defun max-normalized-mapping-score (mapping sme &key replacement-score)
  (when (null mapping)
    (return-from max-normalized-mapping-score 0))
  
  (let* ((score (or replacement-score (if mapping (score mapping) 0)))
         (params (mapping-parameters sme))
         (divisor (max (self-score-dgroup (base sme) params)
                     (self-score-dgroup (target sme) params)))) 
    (if (zerop divisor)
      1
      (/ score divisor))))

(defun base-normalized-mapping-score (mapping sme &key replacement-score)
  (when (null mapping)
    (return-from base-normalized-mapping-score 0))
  
  (let* ((score (or replacement-score (if mapping (score mapping) 0)))
         (params (mapping-parameters sme))
         (divisor (self-score-dgroup (base sme) params))) 
    (if (zerop divisor)
      1
      (/ score divisor))))

(defun target-normalized-mapping-score (mapping sme &key replacement-score)
    (when (null mapping)
    (return-from target-normalized-mapping-score 0))
  
  (let* ((score (or replacement-score (if mapping (score mapping) 0)))
         (params (mapping-parameters sme))
         (divisor (self-score-dgroup (target sme) params))) 
    (if (zerop divisor)
      1
      (/ score divisor))))

;;Here we allow the base and target to contribute equally to the score,
;;even if their overall sizes differ wildly from each other.
(defun even-normalized-mapping-score (mapping sme &key replacement-score)
  (when (null mapping)
    (return-from even-normalized-mapping-score 0))
  
  (let* ((score (or replacement-score (if mapping (score mapping) 0)))
         (params (mapping-parameters sme))
         (base-divisor (self-score-dgroup (base sme) params))
         (target-divisor (self-score-dgroup (target sme) params))
         
         (base-component (if (zerop base-divisor)
                           1
                           (/ score base-divisor)))
         (target-component (if (zerop target-divisor)
                             1
                             (/ score target-divisor))))
    (/ (+ base-component target-component) 2.0)))
 

;;The score a dgroup should get if you match it to itself.
(defun self-score-dgroup (dgroup params)
  (let ((hash (make-hash-table :test #'equal))
        (entities (entities dgroup))
        (exps (apply #'append (mapcar #'cdr (expressions dgroup)))))
    (apply #'+ (mapcar #'(lambda (item) 
                           (self-score-item item params hash))
                 (append entities exps)))))


(defun top-level-exps (dgroup)
  (let ((exps (apply #'append (mapcar #'cdr (expressions dgroup)))))
    (remove-if #'(lambda (exp) (not (null (parents exp)))) exps)))


(defun self-score-item (item params hash)
  (with-slots (trickle-down max-local-score) params
    (let ((score (gethash (user-form item) hash)))
      (cond
       (score
        score)
       (t
        (let ((score (self-match-score item params)))
          (incf score
                (apply #'+ (mapcar #'(lambda (parent) 
                                       (* (self-score-item parent params hash)
                                          trickle-down))
                             (parents item))))
          (setf score (min-without-nils score max-local-score))
          (setf (gethash (user-form item) hash) score)))))))

(defmethod self-match-score ((item entity) params)
  (declare (ignore params))
  0)

(defmethod self-match-score ((item expression) params)
  (if (and (allow-probability? params) (probability-as-utility? params))
    (* (same-functor params) (expt (probability item) 2))
    (same-functor params)))
  
     
  
