;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: debug.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ken Forbus
;;;;   Created: January 13, 1999 13:26:53
;;;;   Purpose: Debugging utilities
;;;; ---------------------------------------------------------------------------
;;;;  modified: Friday, January 2, 2009 at 14:07:17 by forbus
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;;;;; Utilities for estimating size of matches
;;
;; Estimate the number of potential mhs from the dgroups

(defun dgroup-expressions-by-predicate (dgroup)
   ;; Helper for the MH size estimation process
   (let ((table nil))
      (dolist (exp (get-dgroup-expressions dgroup)
                (sort table #'(lambda (e1 e2)
                                (> (cdr e1) (cdr e2)))))
         (let* ((functor (functor exp))
                (entry (assoc functor table)))
            (unless entry
               (setq entry (cons functor 0))
               (push entry table))
            (incf (cdr entry))))))

(defun estimate-mhs-by-predicate (dgroup1 dgroup2)
   (let ((product nil)
         (preds1 (dgroup-expressions-by-predicate dgroup1))
         (preds2 (dgroup-expressions-by-predicate dgroup2)))
      (dolist (entry preds1)
         (let ((other (assoc (car entry) preds2)))
            (when other
               (push (cons (car entry) (* (cdr entry) (cdr other)))
                 product))))
      (sort product #'(lambda (e1 e2)
                        (> (cdr e1) (cdr e2))))))

(defun estimate-mhs-by-predicate-exclude-ubiquitous (dgroup1 dgroup2)
   (let ((product nil)
         (preds1 (dgroup-expressions-by-predicate dgroup1))
         (preds2 (dgroup-expressions-by-predicate dgroup2)))
      (dolist (entry preds1)
        (unless (ubiquitous-predicate-in? (first entry)
                  (vocabulary dgroup1))
            (let ((other (assoc (car entry) preds2)))
               (when other
                  (push (cons (car entry) (* (cdr entry) (cdr other)))
                    product)))))
      (sort product #'(lambda (e1 e2)
                        (> (cdr e1) (cdr e2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistics about match hypotheses
;;;
;;; Useful when matches get large and one wants to know why

(defun mh# (i &optional (sme *sme*))
  (dolist (mh (mhs sme))
    (when (= i (id mh)) (return-from mh# (values mh)))))

(defun find-mh-involving (pred-name &optional (sme *sme*))
   (let* ((vocabulary (vocabulary (base sme)))
          (functor (find-predicate pred-name vocabulary)))
      (dolist (mh (mhs sme))
         (when (and (expression? (base-item mh))
                    (eq (functor (base-item mh)) functor))
            (return-from find-mh-involving (values mh))))))

(defun mh-stats-total (&optional (sme *sme*))
   (let ((order-table nil))
      (dolist (mh (mhs sme)
                (sort order-table #'(lambda (x y) (< (car x) (car y)))))
         (let* ((order (order mh))
                (entry (assoc order order-table
                         :test '=)))
            (unless entry
               (setq entry (cons order 0))
               (push entry order-table))
            (incf (cdr entry))))))

(defun mh-stats-consistent (&optional (sme *sme*))
   (let ((order-table nil))
      (dolist (mh (mhs sme)
                (sort order-table #'(lambda (x y) (< (car x) (car y)))))
         (when (and (not (inconsistent? mh))
                    (not (incomplete? mh)))
            (let* ((order (order mh))
                   (entry (assoc order order-table
                            :test '=)))
               (unless entry
                  (setq entry (cons order 0))
                  (push entry order-table))
              (incf (cdr entry)))))))

(defun mh-stats-by-predicate (&optional (sme *sme*))
  (let ((pred-table nil))
    (dolist (mh (mhs sme)
                (sort pred-table #'(lambda (x y) (> (cdr x) (cdr y)))))
      (when (expression-mh? mh)
        (let ((functor (functor (base-item mh))))
          (let ((entry (assoc functor pred-table)))
            (unless entry
              (push (setq entry (cons functor 0)) pred-table))
            (incf (cdr entry))))))))


;;;; ---------------------------------------------------------------------------

(defun plot-score-to-file (file-name &optional (sme *sme*))
   (with-open-file (outfile file-name :direction :output :if-exists :supersede)
     (plot-kernel-scores sme outfile)))

(defun plot-kernel-scores (&optional (sme *sme*) (stream t))
   (dolist (mh (sort (kernel-mappings sme) #'> :key #'score))
      (when (> (score mh) 0)
         (format stream "~A~%" (score mh)))))

(defun plot-mh-scores (&optional (sme *sme*) (stream t))
   (dolist (mh (mhs sme))
      (format stream "~A, ~A~%" (id mh) (score mh))))


;;;; ---------------------------------------------------------------------------

(defun plot-mh-sizes-to-file (file-name &optional (sme *sme*))
   (with-open-file (outfile file-name :direction :output :if-exists :supersede)
     (plot-mh-sizes sme outfile)))


(defun plot-mh-sizes (&optional (sme *sme*) (stream t))
;;;    (format stream "ID, ARGS, DESC, PARENTS, NOGOODS~%")
   (dolist (mh (mhs sme))
      (format stream "~A, ~A, ~A, ~A, ~A~%" (id mh) (length (arguments mh))
        (length (descendants mh)) (length (parents mh)) (length (nogoods mh)))))


(defun plot-kernel-data (&optional (sme *sme*))
  (let ((kernels (sort (copy-list (kernel-mappings sme)) #'score>))
        (index 0))
    (setq index 0)
    (qrg:runtime-call :plot-data :cgx 
      (mapcar #'(lambda (x) (cons (incf index) (score x))) kernels)
      :title (format nil "Kernel scores of ~A" sme))
    (setq index 0)
    (qrg:runtime-call :plot-data :cgx 
      (mapcar #'(lambda (x) (cons (incf index) (length (mhs x)))) kernels)
      :title (format nil "Kernel sizes of ~A" sme))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integrity checks

(defun find-mapping-1-to-1-violations (mapping)
  ;; Alternate computation for testing to see if 1:1 constraint
  ;; violated, to spot bugs in structural consistency computations
  (let ((base-items nil)
        (target-items nil))
    (dolist (mh (mhs mapping)
                (values (remove-if #'(lambda (entry) 
                                       (= (length (cdr entry)) 1)) base-items)
                        (remove-if #'(lambda (entry)
                                       (= (length (cdr entry)) 1)) target-items)))
      (let  ((base-entry (assoc (base-item mh) base-items))
             (target-entry (assoc (target-item mh) target-items)))
        (unless base-entry
          (push (setq base-entry (cons (base-item mh) nil))
                base-items))
        (unless target-entry
          (push (setq target-entry (cons (target-item mh) nil))
                target-items))
        (push mh (cdr base-entry))
        (push mh (cdr target-entry))))))

(defun compare-match-form (form1 form2)
  (every 'equal (butlast form1) (butlast form2)))

(defun mapping-contents-same? (m1 m2)
  (let ((f1 (cadddr (lisp-form m1)))
        (f2 (cadddr (lisp-form m2))))
    (and (null (set-difference f1 f2 :test 'compare-match-form))
         (null (set-difference f2 f1 :test 'compare-match-form)))))
    

(defun find-redundant-kernels (sme)
  (do ((queue (copy-list (sme::kernel-mappings sme))
              (cdr queue))
       (these-redundant nil nil)
       (this nil)
       (redundancies nil))
      ((null queue) redundancies)
    (setq this (car queue)
        these-redundant (list this))
    (dolist (other (cdr queue)
                   (when (cdr these-redundant)
                     (setq queue (set-difference queue these-redundant))
                     (push these-redundant redundancies)))
      (when (mapping-contents-same? this other)
        (push other these-redundant)))))

(defun find-parallel-connectivity-violations (sme)
  (let ((losers nil))
    (dolist (mh (sme::mhs sme) losers)
      (when (and (sme::expression-mh? mh)
                 (not (sme::inconsistent? mh))
                 (not (sme::incomplete? mh)))
        (cond ((not (= (length (sme::lisp-form (sme::base-item mh)))
                       (length (sme::lisp-form (sme::target-item mh)))))
               (push mh losers))
              ((some #'(lambda (arg-mh)
                         (or (sme::inconsistent? arg-mh)
                             (sme::incomplete? arg-mh)))
                     (sme::arguments mh))
               (push mh losers)))))))
                 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checkpointing datastructures

(defun take-kernel-snapshot (sme)
  (mapcar 'sme::lisp-form (sme::kernel-mappings sme)))




;;;; ---------------------------------------------------------------------------
;;; END OF CODE
