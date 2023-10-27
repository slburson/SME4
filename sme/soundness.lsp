;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: soundness.lsp
;;;;    System: SME v4
;;;;    Author: Ken Forbus
;;;;   Created: June 21, 2003 11:52:09
;;;;   Purpose: Provide closer approximation to soundness rating
;;;; ---------------------------------------------------------------------------
;;;;  Modified: Saturday, June 21, 2003 at 14:10:13 by Kenneth Forbus
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;;; Some of Dedre's experiments suggest that when subjects are evaluating
;;;; soundness that they ignore purely attribute information and focus
;;;; only on relational overlap.  This procedure approximates this computation
;;;; by doing the standard structural consistency computation, but leaving out
;;;; (a) contributions from other mappings and (b) contributions from attribute
;;;; matches that do not participate in relational structure.  

(defmethod soundness-score ((mapping mapping))
  (let ((mh-queue (sort (delete-if #'(lambda (mh)
                                       (and (root-mh? mh) 
                                            (attribute-mh? mh)))
                                   (copy-list (mhs mapping)))
                        #'order>))
        (parameters (mapping-parameters (sme mapping))))
    ;; We use the ci score field as a cache to prevent corrupting
    ;; the standard structural evaluation values.
    (dolist (mh mh-queue)
      (setf (ci-score mh) (score mh))
      (setf (score mh) 0.0))
    (dolist (mh mh-queue) (mh-local-structural-evaluation mh parameters))
    (dolist (mh mh-queue) (apply-trickle-down mh parameters mh-queue))
    (dolist (mh mh-queue) (apply-role-relation-evidence mh parameters mh-queue))
    (let ((soundness 0.0)
          (contributors nil))
      ;; Contributors = alist of (<mh> . <score>) returned for debugging
      ;; purposes.
      (dolist (mh mh-queue (values soundness contributors)) 
        (incf soundness (score mh))
        (push (cons mh (score mh)) contributors)
        (setf (score mh) (ci-score mh))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code
