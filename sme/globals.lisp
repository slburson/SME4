;;; -*- Mode: Lisp; Package: SME -*-

(in-package :sme)

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

;;; From 'expr.lisp'.
(defvar *_building-attribute-value* nil)
