(in-package :cl-user)

;;; This file was assembled from pieces of 'defsys.lsp' and 'export.lsp'.

(defpackage :qrg
  (:use :common-lisp)
  (:export #:fast-compile #:safe-compile
	   #:*data-path* #:*path-sepchar* #:make-qrg-file-name
	   #:make-qrg-path #:append-qrg-path
	   #:make-data-path
	   #:runtime-call
	   #:*trap-errors* #:trap-error))

(defpackage :sme
  #+lucid          (:use :common-lisp :lucid-common-lisp :clos :qrg)
  #+(or acl5 acl6) (:use :common-lisp :qrg)
  #+acl3.0         (:use :common-lisp :common-graphics :qrg)
  #-(or lucid acl5 acl6 acl3.0)  (:use :common-lisp :qrg)

  (:export
   ;; Loading, Running and Testing SME
   #:*sme-path* #:*sme-version* #:load-sme #:load-sme-gui #:sme-batch-shakedown 
   #:sme-shakedown #:sme-help #:start-sme-gui #:sme-toplevel #:new-sme-toplevel
   #:defsmetest #:generate-analogy-summarization

   ;; Defining SME Objects
   #:defdescription #:define-description #:defentity #:define-entity
   #:define-expression #:defpredicate #:defubiquitous-predicate #:defsme #:define-sme
   #:declare-predicate-ubiquitous #:undeclare-predicate-ubiquitous
   #:with-creation-time-touch-on-redefinition

   ;; Working with Vocabulary and Dgroups
   #:*vocabulary* #:*sme-vocabulary-path* #:*sme-description-path* 
   #:available-description-files #:available-vocabulary-files
   #:create-empty-vocabulary #:define-description #:dgroup-from-file
   #:vocabulary-from-file #:in-vocabulary #:vocabulary-documentation
   #:vocabulary-notes #:with-vocabulary #:number-of-predicates
   #:find-predicate #:subordinate-vocabulary-from-source
   #:add-subordinate-vocabulary #:subordinates
   #:get-dgroup-expressions #:lisp-form #:dgroup?

   ;; Matching Parameters
   #:*default-greedy-cutoff* #:*default-greedy-max-#* #:*default-same-functor*
   #:*default-same-function* #:*default-trickle-down* #:*functor-trickle-down*
   #:*default-allow-entity-supported-inferences?* 
   #:*default-compute-reverse-inferences?*
   #:*default-block-most-out-of-mapping-contributions?*
   #:*default-use-less-greedy-greedy-merge?*
   #:*default-max-local-score*
   #:*default-enforce-1to1-minimal-ascension?*

   ;; Object Predicates
   #:function? #:expression? #:arity

   ;; Various User Tools
   #:vocabulary-comparison #:analyze-sme #:numbers-of-kernels-by-predicate
   #:numbers-of-kernels-by-predicate-in-mapping #:all-kernels-with-pred
   #:numbers-of-mhs-by-predicate #:kernels-by-score-and-top-predicate
   #:numbers-of-mhs-by-predicate-type #:trace-sme #:untrace-sme
   #:dehydrate-sme #:dehydrate-sme->stream
   #:summarize-similarities #:show-similarities
   #:summarize-differences #:show-differences

   ;; Other Stuff
   #:*warn-on-kernel-plateaus?* #:*warn-on-greedy-inversion?*

   ;; Variables used to hold current SME and vocabulary.
   #:*sme* #:match #:incremental-match #:match-with-appropriate-filters
   #:incremental-match-with-appropriate-filters
   #:focused-incremental-match #:instrumented-focused-incremental-match
   #:start-instrumented-focused-incremental-match 
   #:finish-instrumented-focused-incremental-match

   ;; Object classes
   #:sme
   #:description
   #:predicate
   #:classic-predicate
   #:expression
   #:mapping
   #:entity
   #:vocabulary
   #:match-hypothesis
   ;;Structural-Evaluator
   #:parameters

   ;; Definition macros
   #:merge-descriptions
   #:define-predicate
   #:show-string
   #:with-debug-stream
   #:with-instrumentation
   #:with-complacent-sme
   #:with-vacme
   #:with-sme-type
   #:*mh-expandable-fn*
   #:*sme-case-name-fn*

   ;; Some macros are exported
   #:show-string

   ;; Class slots
   #:name #:doc-string #:notes ;; From documented-object
   #:expressions
   #:entities
   #:roots
   #:user-form
   #:functor
   #:arguments
   #:parents
   #:target
   #:base
   #:mhs
   #:commutative?
   #:n-ary?
   #:mappings
   #:match-constraints
   #:score
   #:pidgin-description-string
   #:base-item
   #:target-item
   #:plist

   ;; Class predicates
   #:commutative?
   #:n-ary?
   #:same-functor
   #:same-function

   ;; Normalized score function
   #:normalized-mapping-score
   #:max-normalized-mapping-score
   #:base-normalized-mapping-score

   ;; Testing, doc, and benchmarking routines
   #:complete-mapping? #:sme-dumper
   #:files-fitting #:pidgin))

(defpackage :sme-user
  #+(or mcl allegro) (:use :qrg :common-lisp :cl-user :clos :sme :qrg)
  #+lucid   (:use :qrg :common-lisp :cl-user :clos :sme :qrg)
  #+aclpc   (:use :qrg :common-lisp :cl-user :allegro :sme :qrg)
  #-(or mcl allegro lucid aclpc) (:use :qrg :common-lisp :cl-user :sme :qrg)
  (:import-from :cl-user #:gc)) ; do all implementations have this?

(defpackage :sme-data (:use))

