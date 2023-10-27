;;; -*- Mode: Lisp; -*-
;;;
;;; Todo.lisp   Things that need to be handled in the current version 
;;;              of SME.
;;;
;; 1/2/96
;;
;; * Figure out whether to keep optional DGROUP-NAME argument in 
;;   B-DGROUP in the sme batch routines.  Currently not used by routine.
;;
;; 12/14/95
;; * Rewrite calculate-mh-roots in mhs.lisp.  Badly abstracted.
;;
;; 12/11/95
;;
;; * Change all the uses of the antiquated term "root mapping" to use the
;;    new term "kernel" or "kernel mapping".  Not urgent, but important.
;;
;; * Replace current ID slot setting (in mapping and other class objects)
;;    with a class slot which allows the ID to be automatically set when
;;    the object is created.  There should also be a routine that can be
;;    called to reset the ID count.  
;;
;; 11/15/95
;; 
;; ADDITION: Add EXTEND-MAPPING routine to the SME batch routines.
;;
;; MOD:  Batch routines should print to the standard output when used
;; outside the context of a SME-BATCH statement.
;;
;;
;;
