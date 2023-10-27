;; -*- mode: lisp -*-
;;;;  $lastchangeddate: 2010-09-09 17:19:49 -0500 (thu, 09 sep 2010) $
;;;;  $lastchangedby: usher $

;;;; MARS: read the example
;;
;; last edited 2/5/94, by kdf

(in-package :common-lisp-user)

;;; these procedures grok the example by setting up the example memory
;;; with the structural description of the example.

(defparameter *nonstructural-predicates*
  '(derived-by given implies and not nvalue set the-set
         work-flows-out heat-flows-out work-flows-in heat-flows-in  
         members cwa equation * / = + - log expt sqrt sqr
         t h u cv cp net-work work-in work-out net-q q-in q-out
         mass-flow q spec-q spec-h spec-u v spec-v))


(defun skim-the-example (&optional (*mars* *mars*))
  ;;; copy over everything that isn't derived.
  (map-expressions 
   (lambda (expr)
     (unless (member (sme::name (sme::predicate expr))
                     *nonstructural-predicates*)
       (sme::define-expression (sme::user-form expr)
           (example-memory *mars*))))
   (example *mars*))
    ;;; carry out initial mapping
  (sme::match-with-appropriate-filters
   (sme *mars*) *mars-match-filter*))

(defun read-the-problem (&optional (*mars* *mars*))
  (map-expressions 
   (lambda (expr)
     (sme::define-expression (sme::user-form expr)
         (working-memory *mars*)))
   (problem *mars*)))

