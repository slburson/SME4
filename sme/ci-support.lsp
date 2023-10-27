;;;; --------------------------------------------------------------------------
;;;; File name: ci-support.lsp
;;;;    System: SME
;;;;   Version: 4.0
;;;;    Author: Andrew Lovett
;;;;   Created: 
;;;;  $LastChangedDate: 2011-10-27 14:03:49 -0500 (Thu, 27 Oct 2011) $
;;;;  $LastChangedBy: aml758 $
;;;;   Purpose: Alternative code for calculating ci support
;;;; --------------------------------------------------------------------------

(in-package :sme)
 
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