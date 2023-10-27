;;;; -*- Mode: Lisp; Mode: Outline; -*-
;;;; ------------------------------------------------------------
;;;; File name: se.lsp
;;;;    System: SME
;;;;   Version: v4
;;;;    Author: Ron Ferguson
;;;;  $LastChangedDate: 2014-07-22 00:33:20 -0500 (Tue, 22 Jul 2014) $
;;;;  $LastChangedBy: liang $
;;;;
;;;; Structural evaluator for SME v4
;;;; ------------------------------------------------------------
(in-package :sme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry point for structural evaluation

(defgeneric evaluate (sme)
  (:documentation 
   "Evaluate the structure of all the new match hypotheses in this 
    SME.  Reset the scores of the MHs and the 
    mappings (both global and kernel mappings)."))

(defmethod evaluate ((sme sme))
  (let* ((map-params (mapping-parameters sme))
         (alt-trickle-down? (block-most-out-of-mapping-contributions? map-params))
         (new-mhs (if alt-trickle-down?
                    (mhs sme)
                    (new-mhs sme)))
         (mh-queue (sort (remove-if #'mh-losing? new-mhs) #'order>))
         (all-mhs (mhs sme)))
    ;; The mh-queue has only structurally consistent and complete mh's
    ;; and are ordered from roots to entities, so that trickle-down can
    ;; be computed locally.  Start by installing local evidence 
     (dolist (mh mh-queue) (mh-local-structural-evaluation mh map-params))
     ;; Propagate local evidence via trickle-down
     (dolist (mh mh-queue) (apply-trickle-down mh map-params all-mhs))
    ;; Propagate evidence through role relations
    (dolist (mh mh-queue) (apply-role-relation-evidence mh map-params all-mhs))
    ;; Update scores for kernel mappings.
     (dolist (mapping (kernel-mappings sme))
        (mapping-structural-evaluation mapping map-params))
     ;; Update scores for global mappings. 
     (dolist (mapping (mappings-cache sme))
        (mapping-structural-evaluation mapping map-params))))

(defparameter *direct-weight/multiply-mh-score-by-utility* nil)

(defun mh-local-structural-evaluation (mh parameters)
  (declare (type match-hypothesis mh) (type parameters parameters))
  "Performs local structural evaluation of a match hypothesis.
   Returns score."
  (setf (score mh) 0.0)
  (with-slots (base-item target-item) mh
    (when (and (expression? base-item) (expression? target-item))
       (cond ((function? (predicate base-item)) ;; function match.
              (if (eq (predicate base-item) (predicate target-item))
                 (incf (score mh) (same-functor parameters))
                 (incf (score mh) (same-function parameters))))
             ;; Otherwise, relational match.
             ((and (listp (rationale mh))
                   (eq (car (rationale mh)) :minimal-ascension))
              ;; minimal ascension match is scored lower
              (let ((depth (third (rationale mh))))
                (incf (score mh) (* (expt (minimal-ascension-multiplier parameters) depth)
                                    (same-functor parameters)))))
             (t (incf (score mh) (same-functor parameters))))
      (when (allow-probability? parameters)
        (setf (score mh)
          (if (probability-as-utility? parameters)
              (if *direct-weight/multiply-mh-score-by-utility* 
                  ;;weight/multiply the mh score directly by utility/probability                  
                  (* (score mh) (* (expr-utility base-item)
                                   (expr-utility target-item)))
                (* (score mh) (expt (/ (+ (expr-utility base-item)
                                          (expr-utility target-item)) 2) 2)))
            (* (score mh) (- 1.0 (abs (- (probability base-item)
                                         (probability target-item))))))))))
  (score mh))

(defmethod expr-utility ((expr probabilistic-expression))
  (slot-value expr 'probability))

(defmethod expr-utility ((expr expression))
  (reduce 'max (mapcar 'probability (expr->top-level-parents expr))))

(defun expr->top-level-parents (expr &aux top-levels)
  (when (top-level? expr)
    (push expr top-levels))
  (dolist (p (parents expr) top-levels)
    (let ((p-top-levels (expr->top-level-parents p)))
      (when p-top-levels
        (setf top-levels (append top-levels p-top-levels))))))


(defmethod apply-trickle-down ((mh match-hypothesis) (params parameters) (mhs list) 
                               &optional global?)
   "Apply trickle down to a single match hypotheses, passing the score
    onto its child match hypotheses."
  (if (and (not global?) (block-most-out-of-mapping-contributions? params))
    (apply-trickle-down-one-to-one-constraint mh params mhs) 
    
    (with-slots (base-item target-item score) mh
      (with-slots (functor-trickle-down? same-function sme-functor
                                         trickle-down max-local-score
                                         minimal-ascension-multiplier) params
       (when (and (expression? base-item) (expression? target-item))
         ;; First trickle down to functors
         (when functor-trickle-down?
           (add-evidence (functor-mh mh) (* trickle-down score) 
                         max-local-score minimal-ascension-multiplier))
         ;; Then trickle down to arguments that are in the set of mh's given
         (dolist (argument-entry (arguments mh))
           (when (member argument-entry mhs)
             (add-evidence argument-entry (* trickle-down score) 
                           max-local-score minimal-ascension-multiplier)))
         ;; Also trickle down to commutative's children.
         ;; But don't do this if the commutative's argument slot contains children,
         ;; 'cuz then we'd be trickling down twice
         (when (and (commutative-mh? mh) (null (arguments mh)))
           (dolist (child-mh (possible-children mh))
             (when (member child-mh mhs)
               (add-evidence child-mh (* trickle-down score) max-local-score)))))))))
  


(defun apply-trickle-down-one-to-one-constraint (mh params mhs)
  "Apply trickle down to a single match hypotheses by greedily finding the maximal
   set of parent match hypotheses that do not violate the one-to-one mapping constraint."
  (let ((base-items)
        (target-items))
    (with-slots (base-item target-item score) mh
      (with-slots (functor-trickle-down? same-function sme-functor
                                         trickle-down max-local-score
                                         minimal-ascension-multiplier) params
        
        (when (and (member mh mhs)
                   (or functor-trickle-down? (not (eq (rationale mh) :functor-match))))
          (dolist (parent (sort (copy-list (parents mh)) #'> :key #'score))
            (when (and (member parent mhs)
                       (not (or (member (base-item parent) base-items)
                                (member (target-item parent) target-items))))
              
              (push (base-item parent) base-items)
              (push (target-item parent) target-items)
              (add-evidence mh (* trickle-down (score parent)) 
                            max-local-score minimal-ascension-multiplier))))))))

(defmethod apply-trickle-down ((mh role-relation-match-hypothesis)
                               (params parameters) (mhs list) &optional global?)
  (declare (ignore global?))
  ;; Pass it down to the reified entity, but not to the filler.  That happens later.
  (declare (ignore one-to-one?))
  (with-slots (base-item target-item score) mh
    (with-slots (max-local-score minimal-ascension-multiplier) params
      (when (and (expression? base-item) (expression? target-item))
          ;; No functor trickle-down for role relations.  Arbitrary design choice.
          ;; Only trickle down to the reified entity.  Multiplication has already
          ;; happened, so just pass it on
          (when (mh? (reified-entity-mh mh)) ;; Paranoid?  Us? :-)
            (add-evidence (reified-entity-mh mh) score 
                          max-local-score minimal-ascension-multiplier)
            (setf (score mh) 0.0) ;; Don't double-count.
            )))))
       
(defmethod apply-role-relation-evidence ((mh match-hypothesis) (params t) (mhs t)) nil)

(defmethod apply-role-relation-evidence ((mh role-relation-match-hypothesis)
                                         (params parameters) (mhs list))
  ;; Now we pass activation to the filler, based on trickling down one more time
  ;; from the reified entity "through" the role relation to the filler.
  (with-slots (base-item target-item score) mh
    (with-slots (trickle-down max-local-score) params
      (when (and (expression? base-item) (expression? target-item)
                 (mh? (reified-entity-mh mh)) (mh? (role-filler-mh mh)))
        ;; Okay, a little paranoid.  But just a little.
        (add-evidence (role-filler-mh mh)
                      (* trickle-down (score (reified-entity-mh mh)))
                      max-local-score)))))

;;;; Scores for a mapping
;; Previously we simply summed the scores for all the constitent match
;; hypotheses.  This isn't quite right, because their trickle-down includes 
;; contributions from match hypotheses that cannot be together in any consistent
;; mapping. This has lead to experiments in blocking such contributions.  Specifically,
;; when block-most-out-of-mapping-contributions? is t, it will only count trickle-down from
;; mhs within a mapping, for global mappings.

(defun mapping-structural-evaluation (mapping parameters &optional (global?))
  (declare (type mapping mapping))
  (setf (score mapping) 0.0)
  (cond ((not (or (and global? (block-most-out-of-mapping-contributions? parameters))
                  (not (allow-out-of-mapping-score-contributions? parameters))))
         (setf (score mapping)
           (reduce #'(lambda (mh sum) (+ mh sum))
                   (mhs mapping)
                   :key 'sme::score :initial-value 0.0)))
        (t (let* ((mhs (mhs-include-commutative-children mapping))
                  (mh-queue (sort mhs #'order>))
                  (map-params (mapping-parameters (sme mapping))))
             ;; Cache global values in the ci score.
             ;; N.B. A bit wasteful, but it is useful to see what those values are.
             (dolist (mh mh-queue) (setf (ci-score mh) (score mh))
               (setf (score mh) 0.0))
             (dolist (mh mh-queue) (mh-local-structural-evaluation mh map-params))
             ;; Propagate local evidence via trickle-down
             (dolist (mh mh-queue) (apply-trickle-down mh map-params mh-queue global?))
             (dolist (mh mh-queue) (apply-role-relation-evidence mh map-params mh-queue))
             ;; Compute score for the mapping, and restore cached values
             (dolist (mh mh-queue) (incf (score mapping) (score mh))
               (setf (score mh) (max (score mh) (ci-score mh))))
             (score mapping)))))
  
;;;; Limiting maximum scores
;; max-local-score is a floating-point number which no node will
;; be allowed to rise above. If this number is too low, then many nodes max out
;; and the score is essentially a count of the number of match hypotheses, which
;; wipes out the effect of systematicity and leads to worse results.  

(defun add-evidence (node amount max-value
                          &optional (minimal-ascension-multiplier 1.0))
  (cond
   ((and (listp (rationale node))
         (eq (car (rationale node)) :minimal-ascension))
    (let* ((depth (third (rationale node)))
           (amount-updated (* amount (expt minimal-ascension-multiplier depth))))
      ;; Attenuate contributions based on depth, if multiplier is less than one.
      (setf (score node)
        (min-without-nils max-value (+ (score node) amount-updated)))))
   (t (setf (score node)
        (min-without-nils max-value (+ (score node) amount))))))

;;Returns a list of mhs for a mapping.  If there are commutative mhs, makes a best guess at
;;which of their possible children to include.
(defun mhs-include-commutative-children (mapping)
  (let ((new-mhs)
        (old-mhs (mhs mapping))
        (possible-mhs))
    (dolist (mh old-mhs (append possible-mhs new-mhs))
      (push mh new-mhs)
      (when (commutative-mh? mh)
        (dolist (possible-mh (possible-descendants mh))
          (if (and (not (find possible-mh old-mhs))
                   (not (find possible-mh possible-mhs))
                   (mutually-consistent? possible-mh mapping)
                   (every #'(lambda (mh)
                              (mutually-consistent? possible-mh mh))
                          possible-mhs))
            (push possible-mh possible-mhs)))))))


;;;; Debugging procedures
;; ses = structural evaluation score

(defun show-trickle-down (mh)
  (let ((td 0)
        (trickle-down (trickle-down (mapping-parameters (sme mh)))))
     (format t "~%For ~A: " mh) 
     (dolist (parent (parents mh))
        (let ((parent-score (* (score parent) trickle-down)))
           (format t "~%  From ~A: ~D." parent parent-score)
           (incf td parent-score)))
     (format t "~% Total TD = ~D; Score = ~D."
       td (score mh)) mh))

(defun show-role-contribution (mh)
  (let ((role 0)
        (trickle-down (trickle-down (mapping-parameters (sme mh)))))
    (format t "~%For ~A: " mh)
    (dolist (parent (parents mh))
      (when (and (typep parent 'role-relation-match-hypothesis)
                 (eq (role-filler-mh parent) mh))
        (let ((entity-score (* (score (reified-entity-mh parent)) trickle-down)))
          (format t "~%  From ~A: ~D." parent entity-score)
          (incf role entity-score))))
    (format t "~% Total role = ~D; Score = ~D." role (score mh))
    mh))

(defun show-mh-ses (sme &key (by-order nil)) ;; otherwise, by score.
  (let ((mh-list (sort (copy-list (mhs sme))
		       (if by-order
			   #'order<  #'score>)))
	(min 100000.0)
	(max -100000.0)
	(order 0))
    (dolist (mh mh-list)
     (unless (or (incomplete? mh)
		 (inconsistent? mh))
      (if (> (score mh) max) (setq max (score mh)))
      (if (< (score mh) min) (setq min (score mh)))))
    (format t "~% ~D match hypotheses,~% Min SES = ~D~% Max SES = ~D"
	    (length mh-list) min max)
    (dolist (mh mh-list)
     (unless (or (incomplete? mh)
		 (inconsistent? mh))
      (unless (= order (order mh))
       (format t "~% Order = ~A" (order mh))
       (setq order (order mh)))
      (format t "~%~A = ~D" mh (score mh))))))

(defun show-maps-ses (sme &key (with-mhs nil))  ;; otherwise, skip mhs.
  (let ((map-list (sort (copy-list (mappings sme)) #'score>))
        (min 100000.0)
        (max -100000.0))
    (dolist (mapping map-list)
      (if (> (score mapping) max) (setq max (score mapping)))
      (if (< (score mapping) min) (setq min (score mapping))))
    (format t "~%~D mappings, ~% Min SES = ~D~% Max SES = ~D"
	    (length map-list) min max)
    (dolist (mapping map-list)
      (format t "~%~A = ~D" mapping (score mapping))
       (if with-mhs
          (dolist (mh (mhs mapping))
             (format t "~%~T~A = ~D" mh (score mh)))
          (mhs mapping)))))
