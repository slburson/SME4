;;; -*- mode: lisp -*-
;;;;  $LastChangedDate: 2013-01-06 13:30:52 -0600 (Sun, 06 Jan 2013) $
;;;;  $LastChangedBy: forbus $

;;;; predicates for cyclepad examples
;;
;; last edited 2/2/94, by kdf

(in-package :common-lisp-user)

(sme:defpredicate terminal (physob) attribute)
(sme:defpredicate stuff (physob) attribute)
(sme:defpredicate thermodynamic-stuff (physob) attribute)
(sme:defpredicate stuff-at (terminal physob) relation)
(sme::defpredicate connected-to (terminal terminal) relation
 :commutative? t)

(sme::defpredicate in (component) function)
(sme::defpredicate out (component) function)
(sme::defpredicate in1 (component) function)
(sme::defpredicate out1 (component) function)
(sme::defpredicate in2 (component) function)
(sme::defpredicate out2 (component) function)
(sme::defpredicate heat-source (component) function)

(sme::defpredicate work-flows-in (physob) function)
(sme::defpredicate work-flows-out (physob) function) 
(sme::defpredicate heat-flows-in (physob) function)
(sme::defpredicate heat-flows-out (physob) function) 
(sme::defpredicate work (physob) function)

(sme::defpredicate liquid (physob) attribute)
(sme::defpredicate gas (physob) attribute)
(sme::defpredicate saturated (physob) attribute)
(sme::defpredicate subcooled (physob) attribute)
(sme::defpredicate substance (stuff) attribute)
(sme::defpredicate ideal-gas (stuff) attribute)
(sme::defpredicate real-gas (stuff) attribute)
(sme::defpredicate substance-of (physob substance) relation)

;;; part types

(sme:defpredicate thermodynamic-cycle (thing) attribute)
(sme:defpredicate throttle (name in out) attribute)
(sme:defpredicate mixer (name in1 in2 out) attribute)
(sme:defpredicate splitter (name in out1 out2) attribute)
(sme:defpredicate heat-exchanger
		  (name hot-in hot-out cold-in cold-out) attribute)
(sme:defpredicate abstract-hx (name inlet outlet) attribute)
(sme:defpredicate cooler (name inlet outlet) attribute)
(sme:defpredicate heater (name inlet outlet) attribute)
(sme:defpredicate pump (name inlet outlet) attribute)
(sme:defpredicate compressor (name inlet outlet) attribute)
(sme:defpredicate turbine (name inlet outlet) attribute)
(sme:defpredicate work-converter (thing) attribute)

;; physical processes

(sme::defpredicate heat-flow (physob physob physob physob) relation)
(sme::defpredicate fluid-flow (physob physob physob physob) relation)
(sme::defpredicate compression (physob physob physob) relation)
(sme::defpredicate expansion (physob physob physob) relation)

;; modeling assumptions

(sme:defpredicate non-saturated (component) attribute)
(sme:defpredicate isobaric (component) attribute)
(sme:defpredicate isothermal (component) attribute)
(sme:defpredicate polytropic (component) attribute)
(sme:defpredicate non-polytropic (component) attribute)
(sme:defpredicate adiabatic (component) attribute)
(sme:defpredicate isentropic (component) attribute)

;;; logic stuff

(sme::defpredicate implies (proposition proposition) logical)
(sme::defpredicate members (proposition proposition) logical)
(sme::defpredicate cwa (proposition proposition) logical)
(sme::defpredicate set (name) attribute)
(sme::defpredicate the-set (proposition proposition) logical :n-ary? t)
(sme::defpredicate and (proposition proposition) logical :n-ary? t)
(sme::defpredicate or (proposition proposition) logical :n-ary? t)
(sme::defpredicate not (proposition) logical)
(sme::defpredicate derived-by (proposition proposition) relation)
(sme::defpredicate nvalue (proposition proposition) relation)
(sme::defpredicate value-of (proposition proposition) relation)
(sme::defpredicate skolem (thing) attribute) ;; really a restrictive function
(sme::defpredicate :skolem (thing) attribute) ;; really a restrictive function

;;; for equation stuff

(sme::defpredicate equation (value-expression) attribute)
(sme::defpredicate given (value-expression) attribute)
(sme::defpredicate goal-parameter (value-expression) attribute)
(sme::defpredicate = (expression expression) relation) ;; not commutative
(sme::defpredicate + (expression expression) relation :n-ary? t)
(sme::defpredicate - (expression expression) relation)
(sme::defpredicate * (expression expression) relation)
(sme::defpredicate / (expression expression) relation)
(sme::defpredicate log (expression expression) relation)
(sme::defpredicate ln (expression expression) relation)
(sme::defpredicate expt (expression expression) relation)
(sme::defpredicate sqrt (expression expression) relation)
(sme::defpredicate sqr (expression expression) relation)
(sme::defpredicate saturation-table-for (value-expression) attribute)
(sme::defpredicate superheated-table-for (value-expression) attribute)

;;; high-level things

(sme::defpredicate heat-engine (cycle) attribute)
(sme::defpredicate heat-pump (cycle) attribute)
(sme::defpredicate refrigerator (cycle) attribute)
(sme::defpredicate ideal-rankine-cycle (cycle) attribute)
(sme::defpredicate purpose (component teleo) relation)
(sme::defpredicate condenser (component) attribute)
(sme::defpredicate boiler (component) attribute)
(sme::defpredicate increase (part terminal terminal) relation)
(sme::defpredicate transform (part statement statement) relation)
(sme::defpredicate produce (part property) relation)

;;; quantity types

(sme:defpredicate spec-hg (thing) function)
(sme:defpredicate spec-hf (thing) function)
(sme:defpredicate spec-h (thing) function)
(sme:defpredicate spec-sg (thing) function)
(sme:defpredicate spec-sf (thing) function)
(sme:defpredicate delta-spec-s (thing) function)
(sme:defpredicate spec-s (thing) function)
(sme:defpredicate spec-vg (thing) function)
(sme:defpredicate spec-vf (thing) function)
(sme:defpredicate spec-v (thing) function)
(sme:defpredicate spec-ug (thing) function)
(sme:defpredicate spec-uf (thing) function)
(sme:defpredicate spec-u (thing) function)
(sme:defpredicate s (thing) function)
(sme:defpredicate n (thing) function)
(sme:defpredicate spec-q (thing) function)
(sme:defpredicate net-q (thing) function)
(sme:defpredicate q-out (thing) function)
(sme:defpredicate q-in (thing) function)
(sme:defpredicate q (thing) function)
(sme:defpredicate spec-shaft-work-isentropic (thing) function)
(sme:defpredicate spec-shaft-work (thing) function)
(sme:defpredicate shaft-work (thing) function)
(sme:defpredicate net-work (thing) function)
(sme:defpredicate work-out (thing) function)
(sme:defpredicate work-in (thing) function)
(sme:defpredicate w (thing) function)
(sme:defpredicate nu-isentropic (thing) function)
(sme:defpredicate work-ratio (thing) function)
(sme:defpredicate cop-hp (thing) function)
(sme:defpredicate cop-r (thing) function)
(sme:defpredicate nu-thermal (thing) function)
(sme:defpredicate delta-h-isentropic (thing) function)
(sme:defpredicate delta-h (thing) function)
(sme:defpredicate hout-isentropic (thing) function)
(sme:defpredicate h (thing) function)
(sme:defpredicate u (thing) function)
(sme:defpredicate r (thing) function)
(sme:defpredicate tout-isentropic (thing) function)
(sme:defpredicate t-log-mean (thing) function)
(sme:defpredicate delta-tout (thing) function)
(sme:defpredicate delta-tin (thing) function)
(sme:defpredicate delta-t (thing) function)
(sme:defpredicate psat (thing) function)
(sme:defpredicate tsat (thing) function)
(sme:defpredicate t (thing) function)
(sme:defpredicate molar-mass (thing) function)
(sme:defpredicate kgmoles (thing) function)
(sme:defpredicate flow-split (thing) function)
(sme:defpredicate mass-flow (thing) function)
(sme:defpredicate m (thing) function)
(sme:defpredicate delta-p (thing) function)
(sme:defpredicate dryness-isentropic (thing) function)
(sme:defpredicate dryness (thing) function)
(sme:defpredicate pr (thing) function)
(sme:defpredicate p (thing) function)
(sme:defpredicate v (thing) function)
(sme:defpredicate gamma (thing) function)
(sme:defpredicate hx-fiddle (thing) function)
(sme:defpredicate area (thing) function)
(sme:defpredicate density (thing) function)
(sme:defpredicate cv (thing) function)
(sme:defpredicate cp (thing) function)

;;; control predicates
(sme::defpredicate already-read-example-info-for (thing) attribute)
(sme::defpredicate already-examined-cis-for (thing) attribute)

;;; Ubiquitous predicates
(sme::defubiquitous-predicate =)
(sme::defubiquitous-predicate equation)
(sme::defubiquitous-predicate and)
(sme::defubiquitous-predicate nvalue)
(sme::defubiquitous-predicate the-set)
(sme::defubiquitous-predicate +)
(sme::defubiquitous-predicate -)
(sme::defubiquitous-predicate *)
(sme::defubiquitous-predicate /)
(sme::defubiquitous-predicate log)
(sme::defubiquitous-predicate ln)
(sme::defubiquitous-predicate expt)
(sme::defubiquitous-predicate sqrt)
(sme::defubiquitous-predicate sqr)
