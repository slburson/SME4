;;;; -*- Mode: Lisp; -*- 
;;;; ------------------------------------------------------------ 
;;;; File name: mhs.lsp
;;;;    System: SME 
;;;;   Version: v4
;;;;    Author: Ron Ferguson & Ken Forbus 
;;;;  $LastChangedDate: 2015-10-19 21:28:25 -0500 (Mon, 19 Oct 2015) $
;;;;  $LastChangedBy: forbus $
;;;; ------------------------------------------------------------ 
;;;; Match hypothesis code for I-SME  
 
(in-package :sme) 
 
;; Match hypotheses are created in two ways: 
;; 
;; 1. Loop through each base item with each target item, using 
;;    tiered identicality to propose a potential match.
;; 2. Align two items because they play corresponding roles in the 
;;    arguments of some other expression. 
;; 

;; Procedures: 
;; (create-local-match <b> <t> <sme>) actually does the work involved in 
;;   building a match hypothesis.   
;; (find-indexed-mh base-item target-item sme)  finds a MH in the SME 
;;   between the two items, if available. 
 
 
;;;; Match hypothesis definition  
;;;;---------------------------------------------------------------------- 
(defclass match-hypothesis () 
  ((id 
    :documentation "The ID of the MH" 
    :type integer  :initform -1  :accessor id) 
   ;; Timestamp and ID's are distinct -- an ID is unique within an SME. 
   ;; Any number of MH's can have the same timestamp, since that indicates 
   ;; when in its SME's processing it was created. 
   (timestamp 
    :documentation "When in the SME's processing it was created" 
    :accessor timestamp  :type integer  :initform 0  :initarg :timestamp) 
   (sme 
    :documentation "SME that this MH belongs to." 
    :type sme  :accessor sme  :initarg :sme :initform nil) 
   (base-item 
    :documentation "The base expression or entity" 
    :reader base-item  :initarg :base-item) 
   (target-item 
    :documentation "The target expression or entity" 
    :reader target-item  :initarg :target-item) 
   (rationale ;; can be set via procedures that create mh's  
    :documentation "Reason why this hypothesis might be true" 
    :accessor rationale  :initarg :rationale) 
   ;;; Fields connecting the match hypothesis to others 
   (parents 
    :documentation "All MHs in which this one is an arg" 
    :type list  :accessor parents  :initarg :parents  :initform nil)
   (num-twins
    :documentation "Because of symmetric matches, which produce two mhs between a pair of expressions,
                  we can end up with two or even more mhs (twins) for a given target and base.  This value is 
                  total number of twins of this mh (including this one itself)."
    :accessor num-twins :initarg :num-twins :initform 0)
   (descendants 
    :documentation "Set of itself + MH's for its arguments, recursively." 
    :accessor descendants  :type 
    list
     )
   (nogoods 
    :documentation "All MHs inconsistent with this one" 
    :accessor nogoods  :type 
    list
     ) 
   (arguments 
    :documentation "All MHs that are arguments to this one" 
    :accessor arguments  :initarg :arguments  :initform nil) 
   (functor-mh 
    :documentation "Match hypothesis corresponding to the functor, if any" 
    :accessor functor-mh  :initarg :functor-mh  :initform nil) 
   (order 
    :documentation "Max of orders for base and target items" 
    :type integer   :accessor order  :initarg :order 
    :initform -99);; error marker. 
   (score 
    :documentation "The structural evaluation score of this match hypothesis via trickle-down." 
    :type float  :accessor score  :initform 0.0) 
   (trickle-up-score 
    :documentation "The structural evaluation score of this match hypothesis via trickle-up." 
    :type float  :accessor trickle-up-score  :initform 0.0) 
   (ci-score
    :documentation "Score field used in candidate inference evaluation"
    :type float :accessor ci-score :initform 0.0)
   (incomplete? 
    :documentation "Non-nil if all the args (children) matched are matched. 
                    When non-nil, contains alist of unmatched arguments, 
                    where the CAR is the unmatched item or arg MH, and the 
                    CDR is the reason for the nonmatch.  Note that this  
                    characteristic of the slot isn't used directly by 
                    the program, and is chiefly for diagnostics." 
    :type list 
    :accessor incomplete?  :initarg :incomplete?  :initform nil) 
   (inconsistent? 
    :documentation "If non-nil, the MH is structurally inconsistent. 
                    Initially NIL." 
    :accessor inconsistent?  :initarg :inconsistent?  :initform nil)
   )
  (:documentation 
   "A Match Hypothesis is a proposed alignment between two items in 
    the base and target representations.  Match hypotheses can be proposed 
    for various reasons (such as relational identicality or the argument 
    alignment of parent MHs).  A match hypothesis also caches some useful 
    information, such as whether it is currently incomplete or 
    inconsistent, what match hypotheses are arguments of itself, and which 
    other match hypotheses are locally inconsistent (or 'nogood') with 
    itself (i.e., already propose a different mapping between either its 
    base or target item." 
  )) 


(defmethod children ((mh match-hypothesis))
   (if (functor-mh mh) (cons (functor-mh mh) (arguments mh))
      (arguments mh))) ;; used in ciscore

(defmethod arity ((mh match-hypothesis))
  (length (arguments mh)))

(defmethod roots ((mh match-hypothesis))
  (if (null (parents mh)) (list mh) ;; self-rooted
    (let ((roots nil))
      (dolist (parent (parents mh))
        (dolist (new-root (roots parent)) ;; counting on low depth
          (pushnew new-root roots :test #'eq)))
      roots)))

(defmethod root? ((thing match-hypothesis)) (null (parents thing)))

(defmethod initialize-instance ((instance match-hypothesis) &rest initargs) 
  "Initialize the match hypothesis by setting its ID and timestamp, 
and by setting up the reference sets for the descendants and nogoods. 
Assumes that the SME slot is always set by the constructor." 
  (declare (ignore initargs)) 
  (call-next-method)
  (when (sme instance) 
    (set-mh-id instance)
    (setf (descendants instance) nil)
    (setf (nogoods instance) nil)
    (setf (timestamp instance) (incf (timeclock (sme instance)))))
  instance)

(defun set-mh-id (mh)
  (let ((sme (sme mh)))
    (setf (id mh) (incf (mh-counter sme)))
    mh))
 
(defmethod print-object ((mh match-hypothesis) stream) 
  (format stream "<MH ~A ~A ~A>" (base-item mh) (target-item mh) (id mh))) 
 
;;; MATCH HYPOTHESIS SUBCLASSES 
;;;---------------------------------------------------------------------- 
 
(defclass simple-match-hypothesis (match-hypothesis) 
  () 
  (:documentation  
   "This match hypothesis is the default used for expression, 
    entity, and attribute matches.  It is currently identical to  
    the Match Hypothesis class, but this may change in the future.")) 
 
(defclass commutative-match-hypothesis (match-hypothesis) 
  ((possible-children 
    ;; Note: The set of possible children can be globally inconsistent. 
    ;; The point is to cache the set of MH's between arguments so that 
    ;; trickle-down can work correctly, and that the merge algorithm 
    ;; can draw upon this pool when building mappings. 
    :documentation "The set of MH's between arguments of this MH." 
    :accessor possible-children  :initform nil  :initarg :possible-children)) 
  (:documentation 
   "This is a match hypothesis between commutative expressions, which 
    must be handled differently than general expressions since many 
    different argument matches may be proposed between the commutative 
    arguments.")) 

(defmethod possible-descendants ((cmh commutative-match-hypothesis))
  (when (possible-children cmh)
    (reduce #'union (mapcar #'possible-descendants (possible-children cmh)))))

(defmethod possible-descendants ((mh match-hypothesis))
  (descendants mh))

(defclass symmetric-match-hypothesis (commutative-match-hypothesis) 
  () 
  (:documentation  
   "This match hypothesis is identical to a commutative match 
    hypothesis, but is constrained so that it only occurs between 
    expessions over a pair of items or expressions.")) 
 
(defclass group-match-hypothesis (commutative-match-hypothesis) 
  () 
  (:documentation  
   "This match hypothesis is identical to a commutative match 
    hypothesis, but is constrained so that it can only take place between 
    two group relations, which related undifferentiated groups."))  

;;; Role Relation Match Hypothesis Subclass
;;; These help make structural evaluation more accurate when using
;;; schemas and Davisonian representations.  

(defclass role-relation-match-hypothesis (match-hypothesis)
  ((reified-entity-mh
    :type match-hypothesis :initarg :reified-entity-mh
    :accessor reified-entity-mh
    :documentation "The argument mh representing the reified entity.")
   (role-filler-mh
    :type match-hypothesis :initarg :roll-filler-mh
    :accessor role-filler-mh
    :documentation "The argument mh representing the role filler."))
  (:documentation
   "Role relation match hypotheses pass activation down to their reified
    entity, but then send it "down" to the role filler, as if it were
    an argument in an expression that the reified-entity represents."))

(defmethod role-relation-mh? ((mh t)) nil)
(defmethod role-relation-mh? ((mh role-relation-match-hypothesis)) t)
 
;;; Tests
 
(eval-when (compile load eval)
  (proclaim '(inline mh? commutative-mh?)) 
  (proclaim '(inline mh-losing? mh-okay? okay-parents?)))
 
(defun mh? (possible-mh) 
  "True if the argument is a Match Hypothesis." 
  (typep possible-mh 'match-hypothesis)) 
 
(defmethod commutative-mh? ((mh commutative-match-hypothesis)) 
  "True if the argument is a commutative match hypothesis." 
  t)
(defmethod commutative-mh? ((mh t)) 
  "True if the argument is a commutative match hypothesis." 
  nil)

(defmethod entity? ((mh match-hypothesis)) 
  "True if the match hypothesis is between entities." 
  (entity? (base-item mh)))

(defmethod entity-mh? ((mh t)) nil)
(defmethod entity-mh? ((mh match-hypothesis))
  (entity? (base-item mh)))

(defmethod expression-mh? ((mh t))
  "True if the match hypothesis is between expressions."
  nil)
(defmethod expression-mh? ((mh match-hypothesis))
  "True if the match hypothesis is between expressions."
  (sme:expression? (sme:base-item mh)))

(defmethod attribute-mh? ((mh t)) nil)

(defmethod attribute-mh? ((mh match-hypothesis))
  (and (attribute? (base-item mh))
       (attribute? (target-item mh))))

(defmethod functor-mh? ((mh t)) nil)
(defmethod functor-mh? ((mh match-hypothesis))
  (and (predicate? (base-item mh))
       (predicate? (target-item mh))))
                   
(defmethod root-mh? ((mh t)) nil)
(defmethod root-mh? ((mh match-hypothesis))
  (null (parents mh)))

(defmethod script? ((mh match-hypothesis)) 
  "True if the match hypothesis is between script predicates." 
  (script? (predicate (base-item mh)))) 
 
(defmethod set? ((mh match-hypothesis)) 
  "True if the match hypothesis is between sets." 
  (set? (predicate (base-item mh)))) 
 
(defmethod n-ary? ((mh match-hypothesis)) 
  "True if the match hypothesis is between n-ary predicates." 
  (n-ary? (predicate (base-item mh)))) 
 
(defun mh-losing? (mh)  
  "True if match hypothesis is unacceptable--i.e., is inconsistent 
   or incomplete." 
  (or (inconsistent? mh) (incomplete? mh))) 
 
(defun mh-okay? (mh)  
  "True if match hypothesis is complete and consistent." 
  (not (mh-losing? mh))) 
 
(defun okay-parents? (mh) 
  "True if MH has parents, and at least one of its parents 
   is complete and consistent." 
  (and (parents mh) 
       (some #'mh-okay? (parents mh)))) 
 
(defmethod name ((mh match-hypothesis)) 
  "Return a 'name' for the match hypothesis, based on the items." 
  (list :mh (base-item mh) (target-item mh))) 
 
;;; Retrieving MHs from an SME
 
(defun get-mh (index &optional (sme *sme*)) 
  "Given the ID of an MH, return the MH." 
  (find index (mhs sme) :key #'id :test #'=)) 
 
;; This is very handy for debugging. 
(defun fetch-mh (base-thing target-thing &optional (sme *sme*)) 
  "Given base and target entities and expressions, given as their user 
   forms, retrieve the corresponding MH, if any.  Intended for user 
   use, rather than as part of other functions." 
  (let ((base-item (fetch-item base-thing (base sme))) 
        (target-item (fetch-item target-thing (target sme)))) 
    (find-indexed-mh base-item target-item sme))) 

;;;; Creating Match Hypotheses 
;;;;------------------------------------------------------------ 
;; 
;; Procedures: 
;; (create-local-match <b> <t> <sme>) actually does the work involved in 
;;   building a match hypothesis.   
;; (find-argument-alignments <mh>) proposes possible matches between 
;;   the arguments of the items matched by <mh>. 

(defgeneric create-local-match (bth tth rationale sme)
  (:documentation 
  "Create a local match hypothesis between the given base and target  
   items, using the given rationale and mapping engine.  If a 
   previously constructed MH using the same base and target items is 
   available, return that.  Otherwise construct new MH, with the type of 
   MH based on the type of the base item (the target item is assumed to 
   be of the same general type--i.e. no entity to expression 
   matches)." ))

;;; Matching symetric relations involves creating two match hypotheses, one
;;; for each order of arguments.  Most code doesn't care, so this returns
;;; two values, a single MH which sufficies for most code, but the second
;;; argument is a list.

(defmethod create-local-match (bth tth rationale (sme sme)) 
  (let ((mhs (find-indexed-mhs bth tth sme)))
    (cond 
     (mhs                            ;; already constructed? 
      (values (car mhs) mhs))
     ((entity? bth)                  ;; entity mh. 
      (make-entity-mh bth tth rationale sme))
     ((and (not (expression? bth)) (function? bth))
      (make-functor-mh bth tth sme rationale))
     ((not (expression? bth))
      (make-entity-mh bth tth rationale sme))
     ((or (symmetric? (predicate bth))   ;; symmetric expression
          (rationale-supports-symmetry? rationale))
      (let ((mhs (make-symmetric-mh bth tth rationale sme)))
        (values (car mhs) mhs)))
     ((commutative? (predicate bth)) ;; commutative expression 
      (make-commutative-mh bth tth rationale sme)) 
     ((predicate-is-role-relation? (predicate bth)) ;; Role relation
      (make-role-relation-mh bth tth rationale sme
                             (role-relation-pos (predicate bth))))
     (t                              ;; regular expression 
      (let ((mhs (make-expression-mh bth tth rationale sme)))
        (values (car mhs) mhs)))
      )))
 
(defun make-entity-mh (base-entity target-entity rationale sme) 
  "Construct a match hypothesis between two entities, using the  
   given rationale and SME.  Returns the MH." 
  (let ((mh (make-instance 'simple-match-hypothesis 
              :base-item base-entity :target-item target-entity 
              :sme sme :rationale rationale  
             ;; :type ':ENTITY
              :order 0))) 
    (index-mh-structure mh sme) 
    mh)) 
 
(defun make-expression-mh (base-expr target-expr rationale sme) 
  "Construct a match hypothesis between these two expressions, 
   and between the arguments of these two expressions (recursively). 
   Returns the list of mhs (there may be more than one if any of
   the descendents are symmetric relations)."
  (let ((mh (make-instance 'simple-match-hypothesis 
              :base-item base-expr :target-item target-expr 
              :sme sme :rationale rationale 
              :order (max (order base-expr) (order target-expr))))) 
    (let ((mhs (create-argument-expansion mh sme)))
      (dolist (new-mh mhs)
        (setf (num-twins new-mh) (length mhs))
        (index-mh-structure new-mh sme)
	(maybe-create-functor-alignment new-mh sme rationale))
      mhs)))

(defun make-symmetric-mh (base-expr target-expr rationale sme) 
  "Construct a match hypothesis between two symmetric expressions,  
   and between the arguments of those two expressions (recursively). 
   Returns the mhs.  There are always two mhs between symmetric relations,
   one with the arguments reversed.  However, even more mhs may be returned
   if some of descendents are also symmetric."
  (let* ((mhL (make-instance 'simple-match-hypothesis
                :base-item base-expr :target-item target-expr
                :sme sme :rationale rationale 
                :order (max (order base-expr) (order target-expr))))
         (mhR (make-instance 'simple-match-hypothesis 
                :base-item base-expr :target-item target-expr
                :sme sme :rationale rationale 
                :order (max (order base-expr) (order target-expr))))
         (mhsL (create-argument-expansion mhL sme nil))
         (mhsR (create-argument-expansion mhR sme t))
         (mhs (append mhsL mhsR)))
    
    (dolist (new-mh mhs mhs)
      (setf (num-twins new-mh) (length mhs))
      
      (index-mh-structure new-mh sme)
      (maybe-create-functor-alignment new-mh sme rationale))))
    
(defun make-role-relation-mh (base-expr target-expr rationale sme pos)
  ;; Pos = position of the reified list in the argument list
  (unless (or (= pos 1) (= pos 2))
    (error "Unknown role relation position: ~A, in ~A, ~A, ~A, ~A"
      pos base-expr target-expr rationale sme))
  (let* ((mh (make-instance 'role-relation-match-hypothesis
               :base-item base-expr
               :target-item target-expr
               :rationale rationale :sme sme
               :order (max (order base-expr) (order target-expr)))))
    (index-mh-structure mh sme)
    (maybe-create-functor-alignment mh sme rationale)
    (create-argument-expansion mh sme)
    ;; Now get the role relation direction appropriate
    (cond ((= pos 1)
           (setf (reified-entity-mh mh) (nth 0 (arguments mh)))
           (setf (role-filler-mh mh) (nth 1 (arguments mh))))
          (t (setf (reified-entity-mh mh) (nth 1 (arguments mh)))
             (setf (role-filler-mh mh) (nth 0 (arguments mh)))))
    mh))
  

(defun make-commutative-mh (base-expr target-expr rationale sme) 
  "Construct a match hypothesis between two commutative expressions,  
   and between the arguments of those two expressions (recursively). 
   Returns the MH." 
  (let ((mh (make-instance 'commutative-match-hypothesis 
              :base-item base-expr :target-item target-expr 
              :rationale rationale :sme sme 
;;              :type ':EXPRESSION  
              :order (max (order base-expr) (order target-expr))))) 
    (index-mh-structure mh sme) 
    (maybe-create-functor-alignment mh sme rationale)
    (create-argument-expansion mh sme) 
    mh)) 
 
;;;; Helpers for MH creation 
(defun index-mh-structure (mh sme) 
  (let ((base-item (base-item mh)) 
        (target-item (target-item mh)) 
        (base-table (base-mh-table sme)) 
        (target-table (target-mh-table sme)))
    (setf (gethash base-item base-table)
      (set-add mh (gethash base-item base-table)))
    (setf (gethash target-item target-table)
      (set-add mh (gethash target-item target-table))))
  (push mh (new-mhs sme)) 
  (push mh (mhs sme))) 

;;;; Creating Match Hypotheses Between Functors (predicates/functions)
;; The parameter enforce-1to1-minimal-ascension?, when true, will create
;; functor mhs when doing minimal ascension.  This ensures that the same 
;; non-identical predicate substitutions are used througout the mapping.

(defun maybe-create-functor-alignment (new-mh sme rationale)
  (unless (and (listp rationale)
	       (eq (car rationale) :minimal-ascension)
               (not (enforce-1to1-minimal-ascension?
                     (mapping-parameters sme))))
    (create-functor-alignment new-mh sme)))

(defun create-functor-alignment (new-mh sme
                                        &optional (mh-type 'simple-match-hypothesis)) 
  "Given a newly-created expression match hypothesis, find or create 
   a match hypothesis for matched functors (i.e. predicates) of 
   those two expressions.  Returns the functor MH." 
  (let* ((bfunctor (predicate (base-item new-mh))) 
	 (tfunctor (predicate (target-item new-mh)))
	 (functor-mh (find-or-make-functor-mh bfunctor tfunctor sme 
					      :mh-type mh-type)))
    (push new-mh (parents functor-mh))
    (setf (functor-mh new-mh) functor-mh)
    functor-mh)) 

(defun find-or-make-functor-mh (bfunctor tfunctor sme
                                         &key (rationale ':functor-match) 
                                         (mh-type 'simple-match-hypothesis))
  (or (find-indexed-mh bfunctor tfunctor sme)
      (make-functor-mh bfunctor tfunctor sme rationale mh-type)))

(defun make-functor-mh (bfunctor tfunctor sme rationale
                                 &optional (mh-type 'simple-match-hypothesis))
  (let ((mh (make-instance mh-type 
			   :base-item bfunctor :target-item tfunctor 
			   :order 0 :sme sme  
			   :rationale rationale)))
    (index-mh-structure mh sme)
    mh))

;;;; Argument Expansion 
 
(defgeneric create-argument-expansion (mh sme &optional (reverse?)) 
  (:documentation  
   "For the given match hypothesis, check the arguments of the MH to 
    see if they align as well.  Argument alignments are held to a lower 
    standard in SME than local (identicality-based) alignments.
    If reverse? is t, we reverse the order of the target's arguments.")) 

(defmethod create-argument-expansion ((mh match-hypothesis)
                                      (sme sme) &optional (reverse?)) 
  "Given the newly-created MH, check the arguments of the MH to  
   see if they might also be alignable.  If so, propose new MHs 
   for the arguments.  If not, note that the current MH is 
   incomplete. If reverse? is t, then reverse the arguments of the target item.
   Returns a list of MHs, since multiple MHs may 
   need to be created if this MH has symmetric descendents." 
  (flet ((expand-args (base-arg target-arg) 
                      (let ((bitem (expr-arg-item base-arg)) 
                            (titem (expr-arg-item target-arg))) 
                        (multiple-value-bind (alignable? rationale)
                            (arguments-alignable? bitem titem sme) 
                          (cond (alignable?
                                 (multiple-value-bind (first total)
                                     (create-local-match bitem titem rationale sme)
                                   (or total first)))
                                (t 
                                 (push `(:incompatible ,base-arg ,target-arg) 
                                       (incomplete? mh))
                                 nil))))))
    (let* ((bargs (arguments (base-item mh)))
           (targs (arguments (target-item mh)))
           (targs (if reverse? (reverse targs) targs)))
      (cond ((not (= (length bargs) (length targs)))
             (push `(:arglist-incompatible ,bargs ,targs)
                   (incomplete? mh))
             (list mh))
            (t (let* ((arg-mhs (mapcar #'expand-args bargs targs))
                      (arg-mh-lists (make-arg-mh-lists arg-mhs))
                      (mhs))
                 (cond ((incomplete? mh) (list mh))
                       ((and (null bargs) (null targs)) (list mh))
                       ((or (null bargs) (null targs))
                        (push `(:arglist-incompatible ,bargs ,targs)
                              (incomplete? mh))
                        (list mh))
                       (t (dolist (arg-mh-list arg-mh-lists mhs)
                            (let ((new-mh (copy-mh mh)))
                              (dolist (arg-mh arg-mh-list)
                                (index-argument-mh arg-mh new-mh))            
                              ;;Need to preserve order, for role relations
                              (setf (arguments mh) (nreverse (arguments mh)))
                              (push new-mh mhs)))))))))))


(defmethod create-argument-expansion ((mh commutative-match-hypothesis)
                                      (sme sme) &optional (reverse?)) 
  "Create the set of expanded arguments for a commutative match hypothesis." 
  (declare (ignore reverse?))
  (create-commutative-argument-expansion mh sme))

(defun create-commutative-argument-expansion (mh sme) 
  ;; split out so other commutative MHs can share the same code
  (let ((bargs (arguments (base-item mh))) 
        (targs (arguments (target-item mh))) 
        (bitem nil)(titem nil)(arg-mh nil) 
        (aligned-bargs nil) (aligned-targs nil)) 
    ;; Since commutative predicates don't distinguish 
    ;; between their arguments, we'll rely on the arguments-alignable? 
    ;; procedure to do the hard stuff. 
    ;; N^2 test of pairs of arguments. 
    (dolist (barg bargs) 
      (dolist (targ targs) 
        (setq bitem (cdr barg))            
        (setq titem (cdr targ)) 
        (when (arguments-alignable? bitem titem sme) 
          (setq arg-mh (create-local-match bitem titem ':aligned-argument sme)) 
          (index-argument-mh arg-mh mh) 
          (push barg aligned-bargs) ; bookkeeping for reality check. 
          (push targ aligned-targs)))) 
    (commutative-mh-reality-check mh bargs targs aligned-bargs aligned-targs) 
    mh)) 
 
;; Now do quick reality check.  If n-ary, insist that every one  
;; of either base or target has a match.  The intuition is that one item  
;; has to be a subset of the other.  If not n-ary, then every element of 
;; both base and target must have a match. 
(defun commutative-mh-reality-check (mh bargs targs aligned-bargs 
                                     aligned-targs &aux diffs) 
  "Quick reality check on commutative match hypothesis.  If MH is 
   not n-ary, and some argument are missing, marks MH as incomplete." 
  (declare (inline base-nary? target-nary? 
                   nonmatching-base-args nonmatching-target-args)) 
  ;; First, some helper functions. 
  (flet ((base-nary? (mh) (n-ary? (predicate (base-item mh)))) 
         (target-nary? (mh) (n-ary? (predicate (target-item mh)))) 
         (nonmatching-base-args () 
           (let ((diff (set-difference bargs aligned-bargs :test #'eq))) 
             (when diff (cons ':base diff)))) 
         (nonmatching-target-args () 
           (let ((diff (set-difference targs aligned-targs))) 
             (when diff (cons ':target diff))))) 
    (cond ((base-nary? mh) 
           (cond ((target-nary? mh) 
                  (when (setq diffs (append (nonmatching-base-args) 
                                            (nonmatching-target-args))) 
                    (mark-incomplete mh diffs ':missing-arguments))) 
                 (t 
                  (when (setq diffs (nonmatching-target-args)) 
                    (mark-incomplete mh diffs ':missing-target-arguments))))) 
          ((target-nary? mh) 
           (when (setq diffs (nonmatching-base-args)) 
             (mark-incomplete mh diffs ':missing-base-arguments))) 
          (t  
           (let ((base-diffs (nonmatching-base-args)) 
                 (target-diffs (nonmatching-target-args))) 
             (when (and base-diffs target-diffs) 
               (mark-incomplete mh (append base-diffs target-diffs) 
                                ':missing-arguments))))))) 

;;; Helper Functions for Argument Expansion

(defun make-arg-mh-lists (mhs)
  ;; Takes a list of lists
  ;; returns a new list of lists consisting of all combinations produced 
  ;; by picking one item
  ;; from each of the lists in the input list
  ;; For example:  ((a1 b1) (c2 d2)) => ((a1 c2) (a1 d2) (b1 c2) (b1 d2))
  (when mhs
    (let ((new-mhs)
          (init-mhs (if (listp (first mhs))
                     (first mhs)
                     (list (first mhs))))
          (rem-mhs (make-arg-mh-lists (cdr mhs))))
      (dolist (mh init-mhs (reverse new-mhs))
        (if rem-mhs
          (dolist (rem-mh rem-mhs)
            (push (cons mh (copy-list rem-mh)) new-mhs))
          (push (list mh) new-mhs))))))


(defun copy-mh (mh)
  ;; Copies the most important info from an MH into a new MH
  ;; used in argument expansion
  (make-instance 'simple-match-hypothesis
    :base-item (base-item mh) :target-item (target-item mh)
    :sme (sme mh) :rationale (rationale mh)
    :order (order mh)))

;;; Indexing Argument Match Hypotheses 
;;;---------------------------------------------------------------------- 
 
(defgeneric index-argument-mh (arg-mh mh) 
  (:documentation 
   "Index the newly-created argument match hypothesis as a child of 
    the given match hypothesis.")) 
 
(defmethod index-argument-mh ((arg-mh match-hypothesis) 
                              (new-mh match-hypothesis)) 
  (pushnew arg-mh (arguments new-mh) :test #'eq) 
  (pushnew new-mh (parents arg-mh) :test #'eq))

(defmethod index-argument-mh ((arg-mhs cons) 
                              (new-mh match-hypothesis)) 
  (dolist (arg-mh arg-mhs)
    (pushnew arg-mh (arguments new-mh) :test #'eq)
    (pushnew new-mh (parents arg-mh) :test #'eq)))
 
 
(defmethod index-argument-mh ((arg-mh match-hypothesis) 
                              (new-mh commutative-match-hypothesis)) 
  (push new-mh (parents arg-mh)) 
  (push arg-mh (possible-children new-mh))) 
 
 
;;;; Detecting Match Hypothesis Roots 
 
(defgeneric calculate-mh-roots (sme)
  (:documentation "Calculate the set of root match hypotheses."))

(defmethod calculate-mh-roots ((sme sme)) 
  ;; Incremental => old roots may now have parents 
  ;; Notice that even with incremental addition of information, 
  ;; an MH that was inconsistent earlier cannot become consistent. 
  ;; It is possible that an incomplete MH could become complete, 
  ;; However, by someone loosening up the criterion for matching 
  ;; two of its arguments.  
  (setf (root-mhs sme)  
        (delete-if #'okay-parents? (root-mhs sme))) 
  ;;Now see if any of the new ones are roots 
  (dolist (new-mh (new-mhs sme)) 
      (find-consistent-root new-mh sme))) 
 
(defmethod find-consistent-root (mh (sme sme)) 
  (cond ((mh-okay? mh) 
         (unless (or (okay-parents? mh) 
                     (functor? (base-item mh)) 
                     (and (entity? (base-item mh))
                          (find (base-item mh)
                                (match-filters sme)
                                :key 'base-item))) 
          (pushnew mh (root-mhs sme) :test #'eq))) 
        (t ;; look at arguments 
         (dolist (arg-mh (arguments mh)) 
           (find-consistent-root arg-mh sme))))) 
 
 
;;; Retrieving Match Hypotheses via the Base and Target Items. 

(defun find-indexed-mh (base-item target-item sme) 
   "Returns a match hypothesis containing the given target 
    and base items from the set of MHs already formed.  If 
    no MH is found, returns NIL." 
   (let ((bcomps (mhs-involving-base base-item sme)) 
         (tcomps (mhs-involving-target target-item sme)))
     (set-first-common-member bcomps tcomps)
     ))

(defun find-indexed-mhs (base-item target-item sme &optional 
                                   (bcomps (mhs-involving-base base-item sme))
                                   (tcomps (mhs-involving-target target-item sme))
                                   (num_found 0))
   "Returns a match hypothesis containing the given target 
    and base items from the set of MHs already formed.  If 
    no MH is found, returns NIL." 
   (let ((mh))
     (setf mh (set-first-common-member bcomps tcomps))
     (when mh
       (if (< (1+ num_found) (num-twins mh))
         (cons mh 
               (find-indexed-mhs base-item target-item sme
                                 (remove mh bcomps) (remove mh tcomps)
                                 (1+ num_found)))
         (list mh)))))

(defun mhs-involving-base (base-item sme) 
  "Return mhs that contain this base item." 
   (gethash base-item (base-mh-table sme))) 
 
(defun mhs-involving-target (target-item sme) 
  "Return mhs containing this target item." 
   (gethash target-item (target-mh-table sme))) 
 

;;; Utility Functions 

(defun mark-incomplete (mh part reason) 
  "Mark the current as incomplete, based on the given 
   part, which is described by the reason." 
  (push (cons part reason) (incomplete? mh))) 
 
(defun mark-inconsistent (mh part reason)
  "Mark the MH as inconsistent, based on the given part,
   which is described by the reason."
  (push (cons part reason) (inconsistent? mh)))

;;; Debugging routines

(defun check-inherited-incompleteness (sme &aux bad-pairs)
  "Are all parents of incomplete MHs incomplete?"
  (dolist (mh (mhs sme))
    (when (incomplete? mh)
      (dolist (parent (parents mh))
	(when (not (incomplete? parent))
	  (warn "~A is incomplete, but its parent ~A is not."
		mh parent)
	  (push (cons mh parent) bad-pairs)))))
  bad-pairs)


(defun check-inherited-inconsistentness (sme &aux bad-pairs)
  "Are all parents of inconsistent MHs inconsistent?"
  (dolist (mh (mhs sme))
    (when (inconsistent? mh)
      (dolist (parent (parents mh))
	(when (not (inconsistent? parent))
	  (warn "~A is inconsistent, but its parent ~A is not."
		mh parent)
	  (push (cons mh parent) bad-pairs)))))
  bad-pairs)

(defun estimate-mh-forest-size (sme)
  (let ((base-exprs (expressions (base sme)))
        (target-exprs (expressions (target sme)))
        (total 0))
    (dolist (entry base-exprs (incf total (* (length (entities (base sme)))
                                             (length (entities (target sme))))))
      (let ((other (assoc (car entry) target-exprs)))
        (when other
          (incf total (* (length (cdr entry))
                         (length (cdr other)))))))))



