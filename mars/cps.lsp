;; -*- mode: lisp; -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; simple algebra system for problem solving experiments
;;
;; based on cps from building problem solvers 

(in-package :common-lisp-user)

(defvar *algebra-path* *mars-pathname*)

(defvar *algebra-files* '("search" "variants" "match" "algebra" "simplify"))

(defun load-algebra (&key (action :load-source))
   (load-files *algebra-path* *algebra-files* :action action))




