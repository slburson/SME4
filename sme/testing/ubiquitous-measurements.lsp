;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: ubiquitous-measurements.lsp
;;;;    System: 
;;;;    Author: Ken Forbus
;;;;   Created: September 7, 2014 14:36:32
;;;;   Purpose: 
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2013-01-06 13:30:52 -0600 (Sun, 06 Jan 2013) $
;;;;  $LastChangedBy: forbus $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun measure-ubiquitous-bloat (path subdirs &optional (separator
                                                         #+mswindows "\\"
                                                         #+linux "/"))
  (let ((results nil))
    (dolist (subdir subdirs results)
      (push (cons subdir 
                  (measure-ubiquitous-bloat-for
                   (concatenate 'string path subdir separator)))
            results))))

(defun measure-ubiquitous-bloat-for (path &optional 
                                          (extension "sme"))
  (declare (special the-sme the-match-constraints))
  (let ((the-max 0)
        (the-min 0)
        (count 0)
        (no-bloat-count 0)
        (the-total 0)
        (*load-verbose* t))
    (dolist (file (directory (concatenate 'string path "*." extension))
                  (list no-bloat-count count the-min 
                        (float (if (> count 0) (/ the-total count) 0.0)) (float the-max)))
      
      (load file)
      (sme::match-with-appropriate-filters the-sme the-match-constraints)
      (let ((before-mhs (length (sme::mhs the-sme))))
        (load file)
        (wipe-ubiquitous-information-from-vocabulary
         (sme::vocabulary (sme::base the-sme)))
        (sme::match-with-appropriate-filters the-sme the-match-constraints)
        (let* ((after-mhs (length (sme::mhs the-sme)))
               (bloat-percent (/ (- after-mhs before-mhs)
                                 before-mhs)))
          (cond ((= before-mhs after-mhs)
                 (incf no-bloat-count))
                (t (format t "~% ~A, ~D to ~D." file before-mhs after-mhs)
                 (if (> bloat-percent the-max) (setq the-max bloat-percent))
                   (if (< bloat-percent the-min) (setq the-min bloat-percent))
                   (incf count)
                   (incf the-total bloat-percent))))))))

(defun wipe-ubiquitous-information-from-vocabulary (sme-vocabulary)
  (setf (sme::ubiquitous-predicates sme-vocabulary) nil))
                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code