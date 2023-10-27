(asdf:defsystem SME
  :description "The Structure Matching Engine, from the Qualitative Reasoning Group at
Northwestern University.  See: https://www.qrg.northwestern.edu/software/sme4/index.html
This version has been forked by Gyro@sympoiesis.com and massaged to be Quicklisp-loadable,
among other things; see: https://github.com/slburson/SME4"
  :version "4.0.0"
  :licence "BSD"
  :depends-on (:fset)
  :components
  ((:module "sme"
	    ;; Using ':serial t' is less than ideal, but I'm too lazy to work out the dependencies.
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "qrg")          ; QRG utilities
	     (:file "globals")
	     (:file "cmenu")        ; REPL menu package, to support experimentation
	     (:file "sets")         ; Set implementation, used for descendants, nogoods.
	     (:file "utils")        ; Utiilty procedures
	     (:file "defs")         ; Some basic object definitions
	     (:file "pred")         ; Predicates and associated methods
	     (:file "vocabulary")   ; Vocabulary defines a set of predicates and functions
	     (:file "dgroup")       ; i.e. descriptions, aka cases
	     (:file "entity")       ; Entity data structure and helpers
	     (:file "expr")         ; expression data structure and helpers
	     (:file "param")        ; Parameters governing matches.  Mostly static, but recorded
	     (:file "sme")          ; SME object itself and basic methods
	     (:file "mhs")          ; match hypotheses
	     (:file "mapping")      ; Mappings
	     (:file "se")           ; Structural evaluation
	     (:file "soundness")    ; experimental
	     (:file "sc")           ; Structural consistency
	     (:file "cinfer")       ; Candidate inference construction
	     (:file "ciscore")      ; Candidate inference evaluation
	     (:file "abstraction")  ; Producing lightweight generalizations from a match
	     (:file "filters")      ; Filter constraints, automatically applied by external systems
	     (:file "alignment")    ; Determining when items can be aligned
	     (:file "greedy-merge") ; The greedy merge algorithm
	     (:file "match")        ; Core of matcher and entry points
	     (:file "debug")        ; Some debugging utilities
	     (:file "show")         ; Print routines for debugging, report generation
	     (:file "tools")        ; Gathering statistics
	     (:file "pidgin")       ; SME pidgin English output
	     (:file "charsme")      ; REPL experimentation support
	     (:file "dehydrate")    ; Produces a file that can be sent off for remote diagnosis
	     (:file "rehydration")  ; Provides rehydratePredicate - a predicate definition method
	     (:file "simdiff")      ; Provides simple similarity and difference summaries for mappings
	     (:file "normalize")    ; New code for computing a normalized SES score; not called automatically
	     (:file "test")         ; Regression test support
	     (:file "regression")   ; Basic regression tests
	     (:module "testing"
		      :serial t
		      :components
		      ((:file "alist-template-data")   ; Defining flexible dump formats for batch experiments
		       (:file "test-stand")            ; batch facility for empirical complexity analyses
		       (:file "dehydrate-tools"))))))) ; batch facility for analyzing dehydrated SMEs

