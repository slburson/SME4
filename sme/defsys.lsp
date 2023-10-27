;;; -*- Mode: Lisp; Mode: Outline; -*-
;;;; ------------------------------------------------------------
;;;; File name: defsys.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;    Author: Ron Ferguson & Ken Forbus
;;;;  $LastChangedDate: 2015-10-09 10:10:51 -0500 (Fri, 09 Oct 2015) $
;;;;  $LastChangedBy: hinrichs $
;;;; ------------------------------------------------------------

;;;; 1. PACKAGE CREATION.
;;;; ------------------------------------------------------------
(in-package :common-lisp-user)

;;; Check for QRG package
(unless (find-package :qrg)
  (error "You must first load QRG-SETUP."))

;;; Create SME package
(eval-when (compile load eval)
  (unless (find-package :sme)
    (defpackage :sme
     #+lucid        (:use :common-lisp :lucid-common-lisp :clos :qrg)
     #+(or acl5 acl6) (:use :common-lisp :qrg )
     #+acl3.0       (:use :common-lisp :common-graphics :qrg)
     #+mcl          (:use :common-lisp :qrg)
     #+(not (or lucid allegro aclpc mcl))
                    (error "Defsys not set up for this environment.")
     )))

(eval-when (compile load eval)
  (unless (find-package :sme-project)
     (defpackage :sme-project
       #+(or mcl allegro) (:use :qrg :common-lisp :clos :sme)
       #+lucid   (:use :qrg :common-lisp :clos :sme)
       #+aclpc   (:use :qrg :common-lisp :allegro :sme))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set up packages


;;Provide the ability to move data to a separate package in the future.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :data)
    (rename-package (find-package :cl-user)
                    :common-lisp-user  ;keep name the same
                    '(:cl-user :user  ;replace standard nicknames
                               :data))))        ;add a new nickname



;;; Don't make SME symbols available in CL-USER (although the top-level
;;;  routines will be made available.
(eval-when (eval load compile)
  (in-package :sme))

;;;; 2. GLOBAL VARIABLES

(defparameter *sme-version* "v4" "Current version of SME.")
(defvar *wildcard*  "*" "Wildcard variable for this particular operating system.")

(defparameter *sme-path* (qrg:make-qrg-path *sme-version*)
   "Path for SME source code.  This directory needs to be set
    in the defsys.lsp file before it, or SME, is loaded.")

(defparameter *sme-vocabulary-path* (qrg:append-qrg-path *sme-path* "vocab")
  "The default directory for finding vocabuary files, 
   which describe the predicates used in dgroups.")

(defparameter *sme-description-path* (qrg:append-qrg-path *sme-path* "dgroup")
   "The default directory for finding cases, aka descriptions aka dgroups.")

(defvar *sme-vocabulary-extension* "vcb" "Extension used for SME vocabulary files.")

(defvar *sme-description-extension* "dgr" "Extension used for SME dgroup files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 4. LOAD EXPORT FILE

;; We load the export file before loading other defsys's.
;;  This eliminates any problems due to those defsys's using
;;  symbols from this package.
(qrg:load-file *sme-path* "export" :action :load-source :verbose nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 3. LOAD OTHER DEFSYS FILES

;;; None for sme\v4.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 5. LIST THE SYSTEM FILES

(defparameter *sme-files*
  '("cmenu" ;; REPL menu package, to support experimentation
    "sets" ;; Set implementation, used for descendants, nogoods.
    "utils" ;; Utiilty procedures
    "defs" ;; Some basic object definitions
    "pred" ;; Predicates and associated methods
    "vocabulary" ;; Vocabulary defines a set of predicates and functions                                           
    "dgroup" ;; i.e. descriptions, aka cases
    "entity" ;; Entity data structure and helpers
    "expr" ;; expression data structure and helpers
    "param" ;; Parameters governing matches.  Mostly static, but recorded
    "sme" ;; SME object itself and basic methods
    "mhs" ;; match hypotheses
    "mapping" ;; Mappings
    "se" ;; Structural evaluation
    "soundness" ;; experimental
    "sc" ;; Structural consistency
    "cinfer" ;; Candidate inference construction
    "ciscore" ;; Candidate inference evaluation
    "abstraction" ;; Producing lightweight generalizations from a match
    "filters" ;; Filter constraints, automatically applied by external systems
    "alignment" ;; Determining when items can be aligned
    "greedy-merge" ;; The greedy merge algorithm 
    "match" ;; Core of matcher and entry points
    "testing/alist-template-data" ;; Defining flexible dump formats for batch experiments
    "testing/test-stand" ;; Provides batch facility for empirical complexity analyses
    "testing/dehydrate-tools" ;; batch facility for analyzing dehydrated SMEs 
    "debug" ;; Some debugging utilities
    "show"  ;; Print routines for debugging, report generation
    "tools"     ;; Gathering statistics
    "test" ;; Regression test support
    "pidgin"   ;; SME pidgin english output.
    "charsme"  ;; REPL experimentation support
    "dehydrate" ;; Produces a file that can be sent off for remote diagnosis
    "rehydration" ;; Provides rehydratePredicate - a predicate definition method  
    "simdiff" ;; Provides simple imilarity and difference summaries for mappings 
    "normalize"  ;; New code for computing a normalized SES score; not called automatically
    "regression" ;; Basic regression tests
    ))

;;;; 6. LOAD AND COMPILE ROUTINES.

(defmethod qrg::load-sys ((system (eql :sme))
                          &key (action :compile-if-newer)
                          (gui? nil) (verbose t)
                          &allow-other-keys)
  (qrg:load-files *sme-path* *sme-files* :action action :verbose verbose)
  (pushnew :sme *features*)
  :sme)

   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of File
