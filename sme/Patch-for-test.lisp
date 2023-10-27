;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: patch for test.lsp
;;;;    System: 
;;;;   Version: 1.0
;;;;    Author: Ken Forbus
;;;;   Created: August 9, 1999 14:54:24
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  Modified: Monday, August 9, 1999 at 15:41:45 by forbus
;;;; ---------------------------------------------------------------------------

(in-package :sme)

(defun compare-sme-tests (test1 test2)
   ;; This uses the test expressions rather than the test objects, for simplicity
   (let ((alist1 (cddddr (cdr test1)))
         (alist2 (cddddr (cdr test2))))
      ;; Hardest thing to do is to compare the mappings.
      ;; So automate that part.
      (let ((mappings1 (remove-if-not #'(lambda (entry) (eq (car entry) :mapping))
                         alist1))
            (mappings2 (remove-if-not #'(lambda (entry) (eq (car entry) :mapping))
                         alist2)))
         (setq *mappings1* mappings1 *mappings2* mappings2)
         (dolist (m1 mappings1)
            (report-smallest-mapping-difference
             (car (sort (mapcar #'(lambda (m2) (summarize-test-mapping-diffs m1 m2))
                     mappings2)
               #'(lambda (d1 d2) (< (car d1) (car d2)))))))
         )))

(defun summarize-test-mapping-diffs (m1 m2)
   (let* ((emh1 (cadr (assoc :entity-mhs (cddr m1))))
          (emh2 (cadr (assoc :entity-mhs (cddr m2))))
          (rmh1 (cadr (assoc :root-mhs (cddr m1))))
          (rmh2 (cadr (assoc :root-mhs (cddr m2))))
          (e1-not2 (set-difference emh1 emh2 :test 'equal))
          (e2-not1 (set-difference emh2 emh1 :test 'equal))
          (r1-not2 (set-difference rmh1 rmh2 :test 'equal))
          (r2-not1 (set-difference rmh2 rmh1 :test 'equal)))
      (list (+ (length e1-not2)(length e2-not1) (length r1-not2) (length r2-not1))
        (cadr m1) (cadr m2)
        e1-not2 e2-not1 r1-not2 r2-not1)))

(defun report-smallest-mapping-difference (diff-record &optional (stream t))
   (let ((m1 (cadr diff-record))
         (m2 (caddr diff-record)))
      (format stream "~%Closest fit for mapping ~A is mapping ~A: ~D differences."
        m1 m2 (car diff-record))
      (format stream "Entity matches in ~A but not ~A:" m1 m2)
      (pprint (fourth diff-record))
      (format stream "Entity matches in ~A but not ~A:" m2 m1)
      (pprint (fifth diff-record))
      (format stream "Root matches in ~A but not ~A:" m1 m2)
      (pprint (sixth diff-record))
      (format stream "Root matches in ~A but not ~A:" m2 m1)
      (pprint (seventh diff-record))))
      
          
;;;; ---------------------------------------------------------------------------
;;; END OF CODE
