;; -*- mode: lisp -*-

;;;; MARS: a simple analogical problem solver

(in-package :common-lisp-user)

;1. add features to *features* list
;;; ----------------------------------------------------------------------------

(pushnew :mars *features*)

;2. set up packages
;;; ----------------------------------------------------------------------------

(unless (find-package :qrg)
  (error "you must first load qrg-setup."))

;3. define "administrative" global variables
;;; ----------------------------------------------------------------------------

(defparameter *mars-pathname* (qrg::make-qrg-path "mars")
  "path for mars source code.")
(defparameter *mars-files* nil 
  "files for mars.")

;4. load other modules required
;;; ----------------------------------------------------------------------------
;;; mars uses sme
(load-qrg-defsys "v4")

;5. load the export file
;;; ----------------------------------------------------------------------------
;;; loading the export file now, instead of at load-time ensures that 
;;; we register our public symbols as early as possible.

;; no export file yet.

;6. list the system's files
;;; ----------------------------------------------------------------------------
(setq *mars-files*
      '("defs"
        "unify"
        "lookup"
        "solve"
        "ops"
        "goals"
        "readex"
        "toplevel"
        "debug"))

;7. set up package dependencies
;;; ----------------------------------------------------------------------------
;;; this needs to be done as early as possible in the loading process, so we do
;;; it here.

;; no use-package required

;8. defining mars loading function
;;; ----------------------------------------------------------------------------

(defmethod qrg::load-sys ((system (eql :mars))
                          &key (action :compile-if-newer)
                          (gui? nil) (verbose t)
                          &allow-other-keys)
  (load-file *mars-pathname* "cps" :action action)
  (load-algebra :action action)
  (qrg:load-files *mars-pathname* *mars-files* :action action :verbose verbose)
  (pushnew :mars *features*)
  :mars)

;;; ----------------------------------------------------------------------------
;;; end of code