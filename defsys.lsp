;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: defsys.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;    Author: Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: Aug 30, 1996
;;;;  Modified: Thursday, December 6, 2007 at 11:07:33 by hinrichs
;;;;   Purpose: Load the current version of SME
;;;; --------------------------------------------------------------------------

(in-package :common-lisp-user)

#+qrg (load-qrg-defsys "v4")
#-qrg (error "Please load the QRG setup file before loading SME")
