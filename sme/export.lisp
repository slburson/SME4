;;;; -*- Mode: Lisp; Package: SME; -*-
(in-package :sme)
;;; ----------------------------------------------------------------------------
;;;; File name: export.lsp
;;;;    System: SME
;;;;   Version: v4.0
;;;;    Author: Ron Ferguson & Ken Forbus
;;;;  $LastChangedDate: 2015-09-25 17:03:33 -0500 (Fri, 25 Sep 2015) $
;;;;  $LastChangedBy: usher $
;;;;
;;;;  This file export the public functions of SME.
;;; ----------------------------------------------------------------------------


;;; Loading, Running and Testing SME
;;; ----------------------------------------------------------------------------

(cl::export '(*sme-path* *sme-version* load-sme load-sme-gui sme-batch-shakedown 
           sme-shakedown sme-help start-sme-gui sme-toplevel new-sme-toplevel
                     defsmetest generate-analogy-summarization))

(cl::import '(*sme-path* load-sme load-sme-gui sme-batch-shakedown sme-shakedown
           sme-help start-sme-gui sme-toplevel new-sme-toplevel defsmetest)
  :common-lisp-user)

;;; Defining SME Objects
;;; ----------------------------------------------------------------------------
;;; Note: defdescription, defpredicate, and defentity no longer exported to 
;;;   user package, since no longer needed to read dgroup or vocabulary files.

(cl::export '(defdescription define-description defentity define-entity
           define-expression defpredicate defubiquitous-predicate defsme define-sme
           declare-predicate-ubiquitous undeclare-predicate-ubiquitous
           with-creation-time-touch-on-redefinition))

;;; Working with Vocabulary and Dgroups
;;; ----------------------------------------------------------------------------
(cl::export '(*vocabulary* *sme-vocabulary-path* *sme-description-path* 
              available-description-files available-vocabulary-files
              create-empty-vocabulary define-description dgroup-from-file
              vocabulary-from-file in-vocabulary vocabulary-documentation
              vocabulary-notes with-vocabulary number-of-predicates
              find-predicate subordinate-vocabulary-from-source
              add-subordinate-vocabulary subordinates
              get-dgroup-expressions lisp-form dgroup?))
(cl::import '(*sme-vocabulary-path* *sme-description-path*) :common-lisp-user)

;;; Matching Parameters
;;; ----------------------------------------------------------------------------
(cl::export '(*default-greedy-cutoff* *default-greedy-max-#* *default-same-functor*
              *default-same-function* *default-trickle-down* *functor-trickle-down*
                                      *default-allow-entity-supported-inferences?* 
                                      *default-compute-reverse-inferences?*
                                      *default-block-most-out-of-mapping-contributions?*
                                      *default-use-less-greedy-greedy-merge?*
                                      *default-max-local-score*
                                      *default-enforce-1to1-minimal-ascension?*))

;;; Object Predicates
;;; ----------------------------------------------------------------------------
(cl::export '(function? expression? arity))

;;; Various User Tools
;;; ----------------------------------------------------------------------------
(cl::export '(vocabulary-comparison analyze-sme numbers-of-kernels-by-predicate
              numbers-of-kernels-by-predicate-in-mapping all-kernels-with-pred
              numbers-of-mhs-by-predicate kernels-by-score-and-top-predicate
              numbers-of-mhs-by-predicate-type trace-sme untrace-sme
              dehydrate-sme dehydrate-sme->stream
              summarize-similarities show-similarities
              summarize-differences show-differences))
(cl::import '(vocabulary-comparison) :common-lisp-user)

;;; Other Stuff
;;; ----------------------------------------------------------------------------

(cl::export '(*warn-on-kernel-plateaus?* *warn-on-greedy-inversion?*))


;;; Variables used to hold current SME and vocabulary.
(cl::export '(*sme* match incremental-match match-with-appropriate-filters
           incremental-match-with-appropriate-filters
           focused-incremental-match instrumented-focused-incremental-match
           start-instrumented-focused-incremental-match 
           finish-instrumented-focused-incremental-match))

;;; Object classes
(cl::export '(sme
	  description
	  predicate
	  classic-predicate
	  expression
	  mapping
	  entity
	  vocabulary
          match-hypothesis
          ;;Structural-Evaluator
          parameters
          ))

;;; Definition macros
(cl::export '(merge-descriptions
	  define-predicate
	  show-string
          with-debug-stream
          with-instrumentation
;;;          with-hpkb-sme
          with-complacent-sme
          with-vacme
          with-sme-type
          *mh-expandable-fn*
          *sme-case-name-fn*
	  ))

;;; Some macros are available in CL-USER
(cl::import '(show-string) :common-lisp-user)

;;; Class slots
(cl::export
 '(name doc-string notes ;; From documented-object
        expressions
        entities
        roots
        user-form
        functor
        arguments
        parents
        target
        base
        mhs
        commutative?
        n-ary?
        mappings
        match-constraints
        score
        pidgin-description-string
        base-item
        target-item
        plist
        ))


;;; Class predicates
(cl::export '(commutative?
          n-ary?
          same-functor
          same-function
))

;;; Normalized score function
(cl::export 'normalized-mapping-score)
(cl::export 'max-normalized-mapping-score)
(cl::export 'base-normalized-mapping-score)

;;; Testing, doc, and benchmarking routines

(cl::export '(complete-mapping? sme-dumper))
(cl::export '(files-fitting pidgin))



