;;;; -*- Mode: Lisp; Mode: Outline; -*-
;;;; ------------------------------------------------------------
;;;; File name: profile.lsp
;;;;    System: 
;;;;   Version: 
;;;;    Author: 
;;;;  Modified: Sunday, August 30, 1998 at 12:15:35 by ferguson
;;;;
;;;; Some profiling routines for SME.  Temporary.
;;;; ------------------------------------------------------------

(in-package :sme)

(defun profile-water-heat ()
  (vocabulary-from-file "language")
  (let ((sme (define-sme 
		 (dgroup-from-file "waterf")
		 (dgroup-from-file "heatf"))))
    (prof:with-profiling (:type :space)
      (match sme))))


(defun profile-sys2lang ()
  (vocabulary-from-file "sys2lang")
  (let ((sme (define-sme 
		 (dgroup-from-file "base-5")
		 (dgroup-from-file "ta-5"))))
    (prof:with-profiling (:type :space)
      (match sme))))


(defun profile-georep ()
  (vocabulary-from-file "diagram7")
  (let ((sme (define-sme 
		 (dgroup-from-file "sa01")
		 (dgroup-from-file "sa01"))))
    (prof:with-profiling (:type :time)
      (match sme))))

(defun benchmark-all ()
  "The idea here is to have a fairly general measure of
   how fast the SME match process is working.  For this
   reason, the vocabulary load is made smaller by having
   several matches done one after another."
  (vocabulary-from-file "language")
  ;; Heat and water flow
  (let ((base (dgroup-from-file "waterf"))
	(target (dgroup-from-file "heatf")))
    (dotimes (x 10)
      (match (define-sme base target))))
  ;; Solar system and atom.
  (let ((base (dgroup-from-file "solar"))
	(target (dgroup-from-file "ruther")))
    (dotimes (x 10)
      (match (define-sme base target))))
  ;; sys2lang example
  (vocabulary-from-file "sys2lang")
  (let ((base (dgroup-from-file "base-5"))
	(target (dgroup-from-file "ta-5")))
    (dotimes (x 10)
      (match (define-sme base target))))
  ;; Anther sys2lang example.
  (let ((base (dgroup-from-file "base-17"))
	(target (dgroup-from-file "ta-17")))
    (dotimes (x 10)
      (match (define-sme base target)))))

