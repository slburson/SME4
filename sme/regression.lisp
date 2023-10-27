;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: shkdown.lsp
;;;;    System: SME 
;;;;   Version: 4
;;;;    Author: Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: Feb 20, 1998
;;;;  $LastChangedDate: 2014-09-03 23:17:37 -0500 (Wed, 03 Sep 2014) $
;;;;  $LastChangedBy: forbus $
;;;;   Purpose: Set of shakedown tests.
;;;; --------------------------------------------------------------------------
(in-package :sme)

;; N.B. These shakedown tests assume that entity-supported-inferences are allowed.
;; This is important as a test condition because that is the more extreme test of SME.
;; It is also the default for most of our applications these days, since we can use
;; external reasoning sources to filter out inappropriate candidate inferences.

(defsmetest "Simple heat/water analogy" 
  "sys2lang" "swater" "sheat" 
  (:mapping-parameters 
   ((:greedy-cutoff 0.8) (:greedy-max-\# 3) (:same-function 0.002)
    (:same-functor 0.005) (:trickle-down 16.0) (:max-local-score nil) 
    (:functor-trickle-down? nil) 
    (:enforce-1to1-minimal-ascension? t) 
    (:allow-out-of-mapping-score-contributions? t)
    (:minimal-ascension-multiplier 0.8)
    (:probability-as-utility? nil)
    (:allow-probability? nil)
    (:greedy-merge-overlap-only? nil)
    (:block-most-out-of-mapping-contributions? t)
    (:allow-entity-supported-inferences? t)
    (:ci-support-scores-global? nil)
    (:use-less-greedy-greedy-merge? t)
    (:compute-reverse-inferences? nil)
    (:unique-attribute-values? t)))
  (:mh-count 20) 
  (:mapping-count 2)
  (:kernel-count 5) 
  (:mapping 1 
   (:mh-count 11) 
   (:score 3.1179998) 
   (:entity-mhs  cl-user::((pipe bar) (water heat) (beaker coffee) (vial ice-cube)))
   (:root-mhs 
    cl-user::(((flow beaker vial water pipe) (flow coffee ice-cube heat bar)) 
              ((greater (pressure beaker) (pressure vial)) 
               (greater (temperature coffee) (temperature ice-cube)))))
   (:root-mh-count 2) 
   (:cis 
    cl-user::((cause (greater (temperature coffee) (temperature ice-cube)) 
            (flow coffee ice-cube heat bar))
     (liquid heat) (flat-top heat) (clear coffee)
     (greater (diameter coffee) (diameter ice-cube)))))
  (:mapping 2 
            (:mh-count 11) 
            (:score 3.1179998) 
            (:entity-mhs cl-user::((beaker coffee) (vial ice-cube)
                                   (water heat)(pipe bar))) 
            (:root-mhs 
             cl-user::(((greater (diameter beaker) (diameter vial)) 
                        (greater (temperature coffee) (temperature ice-cube)))
                       ((flow beaker vial water pipe) (flow coffee ice-cube heat bar))))
            (:root-mh-count 2) 
            (:cis cl-user::((clear coffee)
                            (cause (greater (pressure coffee) (pressure ice-cube))
                                   (flow coffee ice-cube heat bar))
                            (liquid heat) (flat-top heat)))))

(defsmetest "Complex heat/water analogy" 
   "sys2lang" "waterf" "heatf" 
   (:mapping-parameters 
    ((:greedy-cutoff 0.8) (:greedy-max-\# 3) (:same-function 0.002)
     (:same-functor 0.005) (:trickle-down 16.0) (:max-local-score nil) 
     (:functor-trickle-down? nil) 
     (:enforce-1to1-minimal-ascension? t) 
     (:allow-out-of-mapping-score-contributions? t)
     (:minimal-ascension-multiplier 0.8)
     (:probability-as-utility? nil)
     (:allow-probability? nil)
     (:unique-attribute-values? t)
     (:greedy-merge-overlap-only? nil)
     (:block-most-out-of-mapping-contributions? t)
     (:allow-entity-supported-inferences? t)
     (:ci-support-scores-global? nil)
     (:use-less-greedy-greedy-merge? t)
     (:compute-reverse-inferences? nil)))
  (:mh-count 52) 
  (:mapping-count 1) 
  (:kernel-count 7) 
  (:mapping 1 
   (:mh-count 41) 
   (:score 810.056) 
   (:entity-mhs 
    cl-user::((fluid-flow heat-flow) (cs-v ice-cube) (pi pi) (cs-beak coffee)(pipe bar)))
   (:root-mhs 
    cl-user::(((prop (flow-rate pi) (diff pi)) (prop (flow-rate pi) (diff pi))) 
     ((and (active pi) (process-instance pi)) 
      (and (active pi) (process-instance pi)))
     ((i+ (amount-of cs-v) (flow-rate pi)) 
      (i+ (heat ice-cube) (flow-rate pi)))
     ((i- (amount-of cs-beak) (flow-rate pi)) 
      (i- (heat coffee) (flow-rate pi)))
     ((and (process-instance pi) (equal fluid-flow (process pi))) 
      (and (process-instance pi) (equal heat-flow (process pi))))
     ((sub (pressure cs-beak) (pressure cs-v) (diff pi)) 
      (sub (temperature coffee) (temperature ice-cube) (diff pi)))
     ((implies 
        (and (aligned pipe) 
             (greater (pressure cs-beak) (pressure cs-v)))
        (active pi))
      (implies 
        (and (aligned bar) 
             (greater (temperature coffee) (temperature ice-cube)))
        (active pi)))))
   (:root-mh-count 7) 
   (:cis 
    cl-user::((fluid-path bar)
     (implies 
       (and (process-instance pi) (equal heat-flow (process pi))) 
       (and 
            (implies 
              (and (aligned bar) 
                   (greater (temperature coffee) 
                     (temperature ice-cube)))
              (active pi))
            (implies (and (active pi) (process-instance pi)) 
              (and 
                   (sub (temperature coffee) (temperature ice-cube) 
                     (diff pi))
                   (prop (flow-rate pi) (diff pi)) 
                   (i- (heat coffee) (flow-rate pi)) 
                   (i+ (heat ice-cube) (flow-rate pi))))))))))

(defsmetest "Rutherford analogy" 
  "sys2lang" "solar" "ruther" 
  (:mapping-parameters 
   ((:greedy-cutoff 0.8) (:greedy-max-\# 3) (:same-function 0.002)
    (:same-functor 0.005) (:trickle-down 16.0) (:max-local-score nil) 
     (:functor-trickle-down? nil) 
     (:enforce-1to1-minimal-ascension? t) 
     (:allow-out-of-mapping-score-contributions? t)
    (:minimal-ascension-multiplier 0.8)
    (:probability-as-utility? nil)
    (:allow-probability? nil)
    (:unique-attribute-values? t)
    (:greedy-merge-overlap-only? nil)
    (:block-most-out-of-mapping-contributions? t)
    (:allow-entity-supported-inferences? t)
    (:ci-support-scores-global? nil)
    (:use-less-greedy-greedy-merge? t)
    (:compute-reverse-inferences? nil)))
  (:mh-count 22) 
  (:mapping-count 2) 
  (:kernel-count 6) 
  (:mapping 1 
   (:mh-count 11) 
   (:score 3.2250004) 
   (:entity-mhs cl-user::((sun nucleus) (planet electron))) 
   (:root-mhs 
    cl-user::(((attracts sun planet) (attracts nucleus electron)) 
     ((revolve-around planet sun) (revolve-around electron nucleus)) 
     ((greater (mass sun) (mass planet)) 
      (greater (mass nucleus) (mass electron)))))
   (:root-mh-count 3) 
   (:cis 
    cl-user::((cause 
       (and (greater (mass nucleus) (mass electron)) 
            (attracts nucleus electron))
      (revolve-around electron nucleus))
     (greater (temperature nucleus) (temperature electron))
     (cause (gravity (mass nucleus) (mass electron)) 
            (attracts nucleus electron)))))
  (:mapping 2 
   (:mh-count 11)
   (:score 3.1230001) 
   (:entity-mhs cl-user::((sun nucleus) (planet electron))) 
   (:root-mhs 
    cl-user::(((attracts sun planet) (attracts nucleus electron)) 
              ((greater (temperature sun) (temperature planet)) 
               (greater (mass nucleus) (mass electron)))
              ((revolve-around planet sun) 
               (revolve-around electron nucleus))))
   (:root-mh-count 3) 
   (:cis cl-user::((cause
           (and (greater (mass nucleus) (mass electron)) (attracts nucleus electron))
           (revolve-around electron nucleus))
          (cause (gravity (mass nucleus) (mass electron)) (attracts nucleus electron))))))

(defsmetest "Syslit analogy 5" 
    "sys2lang" "base-5" "ta-5" 
  (:mapping-parameters 
   ((:greedy-cutoff 0.8) (:greedy-max-\# 3) (:same-function 0.002)(:same-functor 0.005)
    (:trickle-down 16.0) (:max-local-score nil) 
     (:functor-trickle-down? nil) 
     (:enforce-1to1-minimal-ascension? t) 
     (:allow-out-of-mapping-score-contributions? t)
    (:minimal-ascension-multiplier 0.8)
    (:probability-as-utility? nil)
    (:allow-probability? nil)
    (:unique-attribute-values? t)
    (:greedy-merge-overlap-only? nil)
    (:block-most-out-of-mapping-contributions? t)
    (:allow-entity-supported-inferences? t)
    (:ci-support-scores-global? nil)
    (:use-less-greedy-greedy-merge? t)
    (:compute-reverse-inferences? nil)))
  (:mh-count 104) 
  (:mapping-count 1) 
  (:kernel-count 24) 
  (:mapping 1 
   (:mh-count 50) 
   (:score 3005.41) 
   (:entity-mhs 
    cl-user::((high high) (man1 gagrach) (karla zerdia) 
     (feathers supercomputer) (f f) (cross-bow missiles)))
   (:root-mhs 
    cl-user::(((warlike man1) (warlike gagrach)) 
     ((asset feathers) (asset supercomputer)) 
     ((weapon cross-bow) (weapon missiles)) 
     ((large cross-bow) (large missiles)) 
     ((possess karla feathers) (possess zerdia supercomputer)) 
     ((possess man1 cross-bow) (possess gagrach missiles)) 
     ((follow (offer karla feathers man1) (obtain man1 feathers)) 
      (follow (offer zerdia supercomputer gagrach) 
        (obtain gagrach supercomputer)))
     ((cause (realize karla (desire man1 feathers)) 
        (offer karla feathers man1))
      (cause (realize zerdia (desire gagrach supercomputer)) 
        (offer zerdia supercomputer gagrach)))
     ((cause (obtain man1 feathers) (equals (happiness man1) high)) 
      (cause (obtain gagrach supercomputer) 
        (equals (happiness gagrach) high)))
     ((follow (attack man1 karla) 
        (equals (success (attack man1 karla)) f))
      (follow (attack gagrach zerdia) 
        (equals (success (attack gagrach zerdia)) f)))
     ((cause (equals (happiness man1) high) 
        (promise man1 karla (not (attack man1 karla))))
      (cause (equals (happiness gagrach) high) 
        (promise gagrach zerdia (not (attack gagrach zerdia)))))
     ((follow (equals (success (attack man1 karla)) f) 
        (realize karla (desire man1 feathers)))
      (follow (equals (success (attack gagrach zerdia)) f) 
        (realize zerdia (desire gagrach supercomputer))))
     ((cause (not (used-for feathers cross-bow)) 
        (equals (success (attack man1 karla)) f))
      (cause (not (used-for supercomputer missiles)) 
        (equals (success (attack gagrach zerdia)) f)))))
   (:root-mh-count 13) 
   (:cis 
    cl-user::((follow (see zerdia gagrach) (attack gagrach zerdia)) 
     (follow (promise gagrach zerdia (not (attack gagrach zerdia))) 
             (attack gagrach (:skolem deer)))
     (happen (see zerdia gagrach)) (lives zerdia (:skolem loc1))
     (thin missiles) (medieval missiles) (wooden missiles)
     (black supercomputer) (covering supercomputer)
     (long supercomputer) (soft supercomputer) (vocal gagrach)
     (biped gagrach) (hunter gagrach) (human gagrach) (male gagrach)
     (predatory zerdia) (black zerdia) (powerful zerdia)
     (large zerdia) (hawk zerdia) (bird zerdia)))))

(defsmetest "Syslit analogy 17" 
   "sys2lang" "base-17" "ta-17" 
  (:mapping-parameters 
   ((:greedy-cutoff 0.8) (:greedy-max-\# 3) (:same-function 0.002)
    (:same-functor 0.005) (:trickle-down 16.0) (:max-local-score nil) 
     (:functor-trickle-down? nil) 
     (:enforce-1to1-minimal-ascension? t) 
     (:allow-out-of-mapping-score-contributions? t)
    (:minimal-ascension-multiplier 0.8)
    (:probability-as-utility? nil)
    (:allow-probability? nil)
    (:unique-attribute-values? t)
    (:greedy-merge-overlap-only? nil)
    (:block-most-out-of-mapping-contributions? t)
    (:allow-entity-supported-inferences? t)
    (:ci-support-scores-global? nil)
    (:use-less-greedy-greedy-merge? t)
    (:compute-reverse-inferences? nil)))
  (:mh-count 152) 
  (:mapping-count 1) 
  (:kernel-count 35) 
  (:mapping 1 
   (:mh-count 56) 
   (:score 75558.984) 
   (:entity-mhs 
    cl-user::((jones mrs-lee) (valley1 twenty-dollars1) (smith betty) (johnson norma)))
   (:root-mhs 
    cl-user::(((desirable valley1) (desirable twenty-dollars1)) 
     ((biped jones) (biped mrs-lee)) 
     ((authority jones) (authority mrs-lee)) 
     ((human jones) (human mrs-lee)) ((biped johnson) (biped norma)) 
     ((human johnson) (human norma)) ((biped smith) (biped betty)) 
     ((human smith) (human betty)) 
     ((same-time (fight smith johnson) (arrive jones)) 
      (same-time (fight betty norma) (arrive mrs-lee)))
     ((follow (discover johnson valley1) (desire johnson valley1)) 
      (follow (discover norma twenty-dollars1) 
        (desire norma twenty-dollars1)))
     ((follow (discover smith valley1) (desire smith valley1)) 
      (follow (discover betty twenty-dollars1) 
        (desire betty twenty-dollars1)))
     ((cause (divide-between jones valley1 smith johnson) 
        (belongs-to johnson valley1))
      (cause (divide-between mrs-lee twenty-dollars1 betty norma) 
        (belongs-to norma twenty-dollars1)))
     ((cause (divide-between jones valley1 smith johnson) 
        (belongs-to smith valley1))
      (cause (divide-between mrs-lee twenty-dollars1 betty norma) 
        (belongs-to betty twenty-dollars1)))
     ((follow (arrive jones) 
        (divide-between jones valley1 smith johnson))
      (follow (arrive mrs-lee) 
        (divide-between mrs-lee twenty-dollars1 betty norma)))
     ((cause (fight smith johnson) 
        (divide-between jones valley1 smith johnson))
      (cause (fight betty norma) 
        (divide-between mrs-lee twenty-dollars1 betty norma)))
     ((cause (divide-between jones valley1 smith johnson) 
        (realize johnson 
          (same-time (divide-between smith valley1 smith johnson) 
            (divide-between johnson valley1 smith johnson))))
      (cause (divide-between mrs-lee twenty-dollars1 betty norma) 
        (realize norma 
          (same-time 
            (divide-between betty twenty-dollars1 betty norma) 
            (divide-between norma twenty-dollars1 betty 
              norma)))))
     ((cause (divide-between jones valley1 smith johnson) 
        (realize smith 
          (same-time (divide-between smith valley1 smith johnson) 
            (divide-between johnson valley1 smith johnson))))
      (cause (divide-between mrs-lee twenty-dollars1 betty norma) 
        (realize betty 
          (same-time 
            (divide-between betty twenty-dollars1 betty norma) 
            (divide-between norma twenty-dollars1 betty 
              norma)))))
     ((cause 
        (and (same-time (discover smith valley1) 
               (discover johnson valley1))
             (same-time (desire smith valley1) 
               (desire johnson valley1)))
        (fight smith johnson))
      (cause 
        (and (same-time (discover betty twenty-dollars1) 
               (discover norma twenty-dollars1))
             (same-time (desire betty twenty-dollars1) 
               (desire norma twenty-dollars1)))
        (fight betty norma)))
     ((follow (divide-between jones valley1 smith johnson) 
        (and (realize smith 
               (same-time 
                 (divide-between smith valley1 smith johnson) 
                 (divide-between johnson valley1 smith johnson)))
             (realize johnson 
               (same-time 
                 (divide-between smith valley1 smith johnson) 
                 (divide-between johnson valley1 smith johnson)))))
      (follow (divide-between mrs-lee twenty-dollars1 betty norma) 
        (and (realize betty 
               (same-time 
                 (divide-between betty twenty-dollars1 betty 
                   norma)
                 (divide-between norma twenty-dollars1 betty 
                   norma)))
             (realize norma 
               (same-time 
                 (divide-between betty twenty-dollars1 betty 
                   norma)
                 (divide-between norma twenty-dollars1 betty 
                   norma))))))))
   (:root-mh-count 19) 
   (:cis 
    cl-user::((follow 
       (and (realize betty 
              (same-time 
                (divide-between betty twenty-dollars1 betty norma) 
                (divide-between norma twenty-dollars1 betty norma)))
            (realize norma 
              (same-time 
                (divide-between betty twenty-dollars1 betty norma) 
                (divide-between norma twenty-dollars1 betty norma))))
       (and (build betty (and (:skolem house1) (:skolem house2))) 
            (build norma (and (:skolem house1) (:skolem house2)))))
     (equal (loc norma) twenty-dollars1)
     (equal (loc betty) twenty-dollars1)
     (possess norma (:skolem house2)) (possess betty (:skolem house1))
     (gorgeous twenty-dollars1) (natural twenty-dollars1)
     (location twenty-dollars1) (land twenty-dollars1)
     (official mrs-lee) (sheriff mrs-lee) (male mrs-lee)
     (pioneer norma) (adventurous norma) (adult norma) (male norma)
     (pioneer betty) (adventurous betty) (adult betty) (male betty)))))

(defsmetest "Commutatives test 1" 
   "sys2lang" "ct5base" "ct5target" 
  (:mapping-parameters 
   ((:greedy-cutoff 0.8) (:greedy-max-\# 3) (:same-function 0.002)
    (:same-functor 0.005) (:trickle-down 16.0) (:max-local-score nil) 
     (:functor-trickle-down? nil) 
     (:enforce-1to1-minimal-ascension? t) 
     (:allow-out-of-mapping-score-contributions? t)
    (:minimal-ascension-multiplier 0.8)
    (:probability-as-utility? nil)
    (:allow-probability? nil)
    (:unique-attribute-values? t)
    (:greedy-merge-overlap-only? nil)
    (:block-most-out-of-mapping-contributions? t)
    (:allow-entity-supported-inferences? t)
    (:ci-support-scores-global? nil)
    (:use-less-greedy-greedy-merge? t)
    (:compute-reverse-inferences? nil)))
  (:mh-count 15)
  (:mapping-count 1)
  (:kernel-count 3) 
  (:mapping 1
   (:mh-count 9)
   (:score 0.415) 
   (:entity-mhs cl-user::((sally seth) (bill bob) (henry henrietta))) 
   (:root-mhs 
    cl-user::(((and henry bill sally) (and seth bob henrietta)) 
     ((active bill) (active bob)) 
     ((authority henry) (authority henrietta))))
   (:root-mh-count 3) 
   (:cis nil)))

(defsmetest "Commutatives test 2" 
  "sys2lang" "ct6base" "ct6target" 
  (:mapping-parameters 
   ((:greedy-cutoff 0.8) (:greedy-max-\# 3) (:same-function 0.002)
    (:same-functor 0.005) (:trickle-down 16.0) (:max-local-score nil) 
     (:functor-trickle-down? nil) 
     (:enforce-1to1-minimal-ascension? t) 
     (:allow-out-of-mapping-score-contributions? t)
    (:minimal-ascension-multiplier 0.8)
    (:probability-as-utility? nil)
    (:unique-attribute-values? t)
    (:allow-probability? nil)
    (:greedy-merge-overlap-only? nil)
    (:block-most-out-of-mapping-contributions? t)
    (:allow-entity-supported-inferences? t)
    (:ci-support-scores-global? nil)
    (:use-less-greedy-greedy-merge? t)
    (:compute-reverse-inferences? nil)))
  (:mh-count 15) 
  (:mapping-count 2) 
  (:kernel-count 5) 
  (:mapping 1 
            (:mh-count 7)
            (:score 0.575) 
            (:entity-mhs cl-user::((bob barbara) (bill biff) (sally simon))) 
            (:root-mhs 
             cl-user::(((and bill bob) (and biff barbara)) 
                       ((and sally bill) (and biff simon)) 
                       ((and bill sally bob) (and barbara simon biff))))
            (:root-mh-count 3) 
            (:cis nil))
  (:mapping 2
            (:mh-count 7)
            (:score 0.575)
            (:entity-mhs cl-user::((bill biff) (bob simon) (sally barbara)))
            (:root-mhs
             cl-user::(((and bill bob) (and biff simon))
                       ((and sally bill) (and biff barbara))
                       ((and bill sally bob) (and barbara simon biff))))
            (:root-mh-count 3)
            (:cis nil)))
