;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: simdiff.lsp
;;;;    System: SME v4
;;;;   Version: 1.0
;;;;    Author: Ken Forbus
;;;;   Created: March 17, 2001 17:21:23
;;;;   Purpose: Summarize similarities and differences
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2012-12-01 14:46:32 -0600 (Sat, 01 Dec 2012) $
;;;;  $LastChangedBy: forbus $
;;;; ---------------------------------------------------------------------------

(in-package :sme)

;;;; One purpose of comparison is to summarize the similarities and differences
;;; between the things compared.  The mappings produced by SME provide this
;;; information, and the procedures in this file produce summaries based on
;;; mappings.
;;;
;;; Since the fundamental processes of computing similarities and differences
;;; are governed by structure-mapping theory, and the uses to which they are
;;; put are also governed by task/user constraints, we only produce here the
;;; unfiltered, theoretically-motivated summaries of information from a
;;; mapping.  It is assumed that
;;; (a) Task concerns were used in formulating the base and target to begin
;;;     with
;;; (b) This information may be filtered by subsequent processes.  Since this
;;;     filtering process will very likely involve knowledge outside of SME's
;;;     perview (i.e., task constraints, reasoning based on a large KB), we
;;;     only provide SME-level services here.
;;; An exception to this is the ability to pass in a system-specific method
;;; for computing the Lisp expression of an SME entity or expression.  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; API
;;;
;;;
;;; Test procedures
;;; These are reference procedures for testing, and to see how different outputs
;;; might look without going through the hassle of HTML.  
;;;
;;; In these procedures, the keyword ->form should be bound to a procedure that
;;; translates from SME internals to whatever form you need for your external system
;;; The default is lisp-form.  It isn't clear how useful passing this procedure around
;;; is -- it may be that all of the form translation needs to occur at a higher level
;;; than this.  In which case, this keyword argument will go away and we'll use lisp-form
;;; internally.
;;;
;;; All of the procedures below take a mapping as their input.
;;;
;;; (summarize-similarities (mapping &key (->form 'lisp-form)))
;;; computes a summary of the similarities suggested by mapping.
;;; It returns three values:
;;;   A similarity score.  This is normalized between zero and one,
;;;     using the base compared to itself to compute the range.
;;;   A list of entity correspondences.  Each element of this list
;;;     has the form (<base entity> <target entity> <score>)
;;;     The intent of including the score is to support explanation
;;;     strategies that use cutoffs.  The list is sorted from high to low.
;;;   A list of root correspondences.  Each element of this list
;;;     has the form (<expression> <score>), but in <expression>,
;;;     entity correspondences are marked with the special expression
;;;     (:entity-pairing <base entity> <target entity>).  The purpose
;;;     of these expressions is to support displays with combined
;;;     information (see below for an example)
;;;
;;; (summarize-differences (mapping &key (->form 'lisp-form)))
;;; computes a summary of the differences as suggested by <mapping>.
;;; It returns four values:
;;;  A similarity score.  This is the same as for similarities above.
;;;  A list of property differences.  Each element of this list has the
;;;    form (<statement> <score> <entity deltas>) where each <entity delta>
;;;    has the form ((:entity-pairing <base entity> <target entity>)
;;;                      <base types> <target types>)
;;;    where the types are attributes in the base or target that are not
;;;    part of the mapping.  In experiments, these have been a particularly
;;;    salient type of alignable difference.
;;;  A list of alignable absences.  This consists in turn of two lists,
;;;    one for the base and one for the target.  Each element of these
;;;    lists has the form (<expression> <subexpressions>) where
;;;    <expression> is not part of the mapping but each <subexpression> is.
;;;    [The set <subexpressions> is not guarenteed to be maximal, some filtering
;;;     may be required.  If this turns out to be a problem we can fix this, but
;;;     this information is mainly there for generating callbacks.]
;;;  A list of nonalignable differences.  This consists in turn of two lists,
;;;    one for the base and one for the target.  Each element of these lists
;;;    consists of an expression from the base (or target) which does not 
;;;    intersect with the mapping at all.
;;;
;;; (show-similarities (mapping &key (->form 'lisp-form)
;;;                                  (stream *standard-output*))
;;; displays the similarities suggested by <mapping> to <stream>
;;;
;;; (show-differences (mapping &key (stream *standard-output*)
;;;                                 (->form 'lisp-form))
;;; displays the differences suggested by <mapping> to <stream>.
;;;
;;; Examples of output from the show procedures is included at the end of the file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Summarizing similarities
;;; There are three aspects of similarity that seem appropriate for summaries:
;;; 1. A numerical score, rating the overall similarity.  For this we simply
;;; use the structural evaluation score of the mapping, normalized.
;;; 2. The entity correspondences.  These are the objects involved in the two
;;; descriptions that have been placed into alignment.
;;; 3. The key relational correspondences. These correspond to the kernels of
;;; the mapping, i.e., those mapped expressions that subsume all other
;;; expressions in the mapping and are not themselves subsumed.

(defun summarize-similarities (mapping &key (->form 'lisp-form))
  (unless (mapping? mapping)
    (error "Not a mapping: ~A.  (summarize-similarities)" mapping))
  (values (compute-normalized-ses mapping)
          (summarize-entity-correspondences mapping :->form ->form)
          (summarize-root-correspondences mapping :->form ->form)
          mapping))

(defun compute-normalized-ses (mapping)
  ;;; Removed normalization because it was way too slow.
  ;;; We'll have to do this via a more clever method at some point
  ;;; Aside from raw performance, this was also using whatever the
  ;;; default SME type was, which isn't really appropriate.
  ;;;   (let* ((sme (sme mapping))
  ;;;          (base (base sme))
  ;;;          (scratch-sme (match (define-sme base base)))
  ;;;          (self-mapping (car (mappings scratch-sme))))
  ;;;     (/ (score mapping) (score self-mapping)))
  (score mapping))

(defun attribute-exp? (exp)
  (eq ':attribute (sme::pred-type (predicate exp))))

(defun get-item-class (entity)
  "Returns the type of this entity by examining the parent expressions and 
   finding the attribute expression that determins the type"
  (let ((attr-exp (find nil (parents entity)
                        :test #'(lambda (x exp)
                                  (declare (ignore x))
                                  (attribute-exp? exp)))))
    (if attr-exp
      (name (predicate attr-exp))
      nil)))
  
(defun summarize-entity-correspondences (mapping &key (->form 'lisp-form))
  "Output is list of entries: ((<base> <target>) (<baseclass> <targetclass>) <score>)"
  (sort (delete nil
                (mapcar #'(lambda (mh)
                            (when (entity? (base-item mh))
                              (let ((base (base-item mh))
                                    (target (target-item mh)))
                                (list (list (funcall ->form base)
                                            (funcall ->form target))
                                      (list (funcall ->form (get-item-class base))
                                            (funcall ->form (get-item-class target)))
                                      (score mh)))))
                  (mhs mapping)))
        '>
        :key 'third))

(defun summarize-entity-correspondences-no-classes (mapping &key (->form 'lisp-form))
  "Output is list of entries: ((<base> <target>) (<baseclass> <targetclass>) <score>)"

  (sort (delete nil
                (mapcar #'(lambda (mh)
                            (when (entity? (base-item mh))
                              (let ((base (base-item mh))
                                    (target (target-item mh)))
                                (list (funcall ->form base)
                                      (funcall ->form target)
                                      (score mh)))))
                  (mhs mapping)))
        '>
        :key 'third))

(defun summarize-root-correspondences (mapping &key (->form 'lisp-form))
  "Output is list of entries: (<common structure> <score>)"
  ;;; Common structure has base/target substituted in
  (let ((entity-correspondences (summarize-entity-correspondences-no-classes mapping
                                                                  :->form ->form)))
    (sort (mapcar #'(lambda (mh)
                      (list (substitute-entity-pairings (funcall ->form (base-item mh))
                                                        entity-correspondences)
                            (score mh)
                            mh))
            (remove-if-not #'expression-mh? (root-mhs mapping)))
          '>
          :key 'second)))

(defun substitute-entity-pairings (form entity-correspondences)
  (cond ((null form) nil)
        (t (let ((entry (assoc form entity-correspondences
                                :test 'equal)))
             (cond (entry ;; Need to do the substitution
                    (list :entity-pairing (car entry) (second entry)))
                   ((consp form)
                    (cons (substitute-entity-pairings (car form)
                                                      entity-correspondences)
                          (substitute-entity-pairings (cdr form)
                                                      entity-correspondences)))
                   (t form))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display support for similarities

(defun package-entity-pairings (form)
  (cond ((null form) nil)
        ((listp form)
         (cond ((eq (car form) :entity-pairing)
                (format nil "~A/~A" (cadr form) (third form)))
               (t (cons (package-entity-pairings (car form))
                        (package-entity-pairings (cdr form))))))
        (t form)))

(defun show-similarities (mapping &key (->form 'lisp-form)
                                  (stream *standard-output*))
  ;;; Illustration of how to use these facilities.
  (multiple-value-bind (nsim entities expressions)
      (summarize-similarities mapping :->form ->form)
    (format stream "~%How ~A and ~A are similar: [~D]" 
      (name (base (sme mapping))) (name (target (sme mapping)))
      nsim)
    (cond (entities
           (format stream "~%What goes with what:")
           (dolist (type-alignment entities)
             (dolist (entity-alignment (rest type-alignment))
               (format stream "~%~A corresponds to ~A [~D]"
                 (car entity-alignment) (second entity-alignment)
                 (third entity-alignment)))))
          (t (format stream "~%No known entity alignments.")))
    (cond (expressions
            (format stream "~%Commonalities:")
            (dolist (expression-alignment expressions)
              (format stream "~%~A [~D]"
                (pretty-print-to-string
                 (package-entity-pairings (car expression-alignment)))
                (second expression-alignment)))))))
                
(defun pretty-print-to-string (form &key (width 40.0))
   "Pretty-prints a LISP expression to a string.  The output is constrained
    to a width that is the number of characters specified by width."
   (let ((*print-right-margin* width))
      (remove-leading-newline
        (with-output-to-string (s)
          (pprint form s)))))

(defun remove-leading-newline (string)
   (if (eq (elt string 0) #\newline)
      (subseq string 1)
     string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Summarizing Differences
;;;
;;; In structure-mapping, there are two fundamental kinds of differences.
;;; Alignable differences are differences that are connected to the
;;; similarities between the things compared.  Non-alignable differences
;;; are differences that are not connected to the commonalities.
;;; Psychologically, alignable differences are much more salient.
;;; We extract three kinds of differences here.  
;;; 1. Property differences.  When corresponding items in a relational
;;;    correspondence differ in type, this is worth noting (i.e.,
;;;    the agent of buying32 was a person, whereas for buying45 it was a robot.)
;;;    N.B. Since SME does not internalize an ontology, we only use explicit
;;;    type information stored in the descriptions, and cannot filter based
;;;    on subsumption.  Again, this seems like a good division of labor because
;;;    this filtering is likely to be task and KB-specific.
;;; 2. Alignable absences.  Candidate inferences are in an sense a form of
;;;    alignable difference.  That is, they involve a base expression that has
;;;    at least one entity that is mapped.  But since candidate inferences
;;;    are only from base to target, and the support/extrapolation scores
;;;    defined for candidate inferences don't make sense for these, we compute
;;;    them separately.
;;;  7/17/01 - Shawn - Alignable differences can be thought of as "Differences
;;;     related to what is in common".  If you have N expressions in the base/target
;;;     which have element A in common which are mapped to some entity/exp (X) in the 
;;;     target/base then you can say that because A corresponds to X, then your N
;;;     expressions are alignable absenses.  In addition you can replace A in those
;;;     expressions with X to show the "candidate inference" that would align the
;;;     expressions.  With this in mind the alignable differences will be returned
;;;     with the structure of 
;;;          (entity/exp-mapping (exp-with-substitution other-mappings)
;;;                              (exp-with-substitution other-mappings)
;;;                              (exp-with-substitution other-mappings)
;;;     Example - if expression Foo in the base has children A, B, C and A is 
;;;      mapped to X in the target and C is mapped to Z in the target and there are
;;;      expressions Baz and Mumble in the base that also contain A as a child then
;;;      we can say:  Because A is mapped to X  then Foo (with X subsitituted in for A)
;;;      is an alignable absense (and also C can map to Z), Baz is an alignable absense
;;;      (with X subst in for A) and the same for Mumble
;;;       
;;; 3. Non-alignable differences.  These are base and target items that simply
;;;    do not participate in the mapping at all.  Notice that there might be other
;;;    mappings that they participate in.  

(defun summarize-differences (mapping &key (->form 'lisp-form))
  (multiple-value-bind (aas nads)
      (compute-alignable-absences-and-nads mapping :->form ->form)
    (values (compute-normalized-ses mapping)
            (summarize-property-differences mapping :->form ->form)
            aas nads mapping)))

(defun summarize-property-differences (mapping &key (->form 'lisp-form))
  ;; Start with the root correspondences, see which of them involve
  ;; entity pairings with different properties
  (let ((entity-deltas nil) ;; Accumulate these.
        (root-correspondences (summarize-root-correspondences mapping
                                                              :->form ->form))
        (property-differences nil))
    (dolist (root-correspondence root-correspondences property-differences)
      ;; each root-correspondences is (<form> <score>)
      (let ((entity-pairings (extract-entity-pairings root-correspondence))
            (entity-differences nil))
        (dolist (ep entity-pairings)
          (let ((deltas (assoc ep entity-deltas :test 'equal)))
            (unless deltas
              (push (setq deltas (cons ep
                                         (compute-entity-pair-deltas
                                          (second ep) (third ep) mapping)))
                    entity-deltas))
            (unless (eq (cadr deltas) :none-known)
              (pushnew deltas entity-differences :test 'equal))))
        (when entity-differences
          (push (list (car root-correspondence) (second root-correspondence)
                      entity-differences (third root-correspondence))
                property-differences))))))

(defun extract-entity-pairings (form &optional (pairings nil))
  (cond ((null form) nil)
        ((listp form)
         (if (eq (car form) :entity-pairing)
             (if (member form pairings :test 'equal) pairings
               (cons form pairings))
           (extract-entity-pairings (car form)
                                    (extract-entity-pairings (cdr form)
                                                             pairings))))
        (t pairings)))
           
(defun compute-entity-pair-deltas (base-eform target-eform mapping)
  ;;; Big assumption here: Whatever someone passed in as form printer is
  ;;; invertable.  Could be trouble, in which case a farther-out layer should
  ;;; be used to do the real translation, and just use the default of sme::lisp-form
  ;;; within this subsystem.
  (let ((base-entity (lookup-entity base-eform (base (sme mapping))))
        (target-entity (lookup-entity target-eform (target (sme mapping)))))
    (unless (and (entity? base-entity) (entity? target-entity))
      ;; Fail-soft.  Could potentially mask bugs, but this shouldn't be
      ;; called externally so should be fine.
      (return-from compute-entity-pair-deltas (list :none-known)))
    (let ((base-deltas (attributes-not-in-mapping base-entity mapping :base))
          (target-deltas (attributes-not-in-mapping target-entity mapping :target)))
      (if (and base-deltas target-deltas) (list base-deltas target-deltas)
        (list :none-known)))))

;; this is already defined in dgroup.lsp ->  fetch-entity...  change 
(defun lookup-entity (name dgroup)
  (find name (entities dgroup) :test 'equal :key 'name))
    
(defun attributes-not-in-mapping (entity mapping which)
  (delete nil
          (mapcar #'(lambda (parent)
                      (when (attribute? (functor parent)) ;; Only look at attributes
                        (unless (find parent (mhs mapping)
                                      :key (if (eq which :base) 'base-item
                                              'target-item))
                          ;; If it's mapped, then it can't be different
                          (name (functor parent)))))
            (parents entity))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alignable absences and nonalignable differences
;;;
;;; This is very similar to the candidate inference calculation.  
;;; The twists are: (a) It is done to both base and target, and (b) those roots
;;; which intersect the mapping are AA's and those which do not are NAD's.

(defun compute-alignable-absences-and-nads (mapping &key (->form 'lisp-form))
  "Returns two values, the set of alignable absences and the set of nonalignable differences.
The alignable absences consist of two lists, one for the base and one for the target.
Each element of these lists consists of (<expression> <list of pieces of mapping>).
The nonalignable differences consists of two lists, one for the base and one for the target.
Each list consists of the set of expressions that do not intersect the mapping at all."
  (multiple-value-bind (base-aas base-nads)
      (compute-aas-nads (base (sme mapping)) mapping :base :->form ->form)
    (multiple-value-bind (target-aas target-nads)
        (compute-aas-nads (target (sme mapping)) mapping :target :->form ->form)
      (values (list base-aas target-aas) (list base-nads target-nads)))))


(defun compute-aas-nads (description mapping which &key (->form 'lisp-form))
  "For each root in description, walk downwards until some element
intersects the mapping.  If this intersection is non-nil, then it is
an alignable absence.  Otherwise it is a nonalignable difference."
  (let ((aas nil)
        (nads nil))
    (dolist (root (roots description) (values aas nads))
      (let ((intersection (maximal-intersection-with-mapping root mapping which)))
        (unless (member root intersection :key #'car) ;; Whole thing is in mapping, so ignore
          (if (null intersection) (push (funcall ->form root) nads)
            (setq aas (update-aas root intersection aas mapping which :->form ->form))))))))


(defun substitute-nested (new old sequence &key (test 'eq) (replace-all? t))
  ;; if replace-all? is nil will just replace the first it finds
  (cond ((null sequence) nil)
        ((funcall test sequence old)
         new)
        ((not (listp sequence))
         old)
        ((funcall test (first sequence) old)
         (cons new
               (if replace-all? 
                 (substitute-nested new old (rest sequence) 
                                    :test test :replace-all? replace-all?)
                 (rest sequence))))
        ((listp (first sequence))
         (cons (substitute-nested new old (first sequence) 
                                  :test test :replace-all? replace-all?)
               (substitute-nested new old (rest sequence) 
                                  :test test :replace-all? replace-all?)))
        (t
         (cons (first sequence)
               (substitute-nested new old (rest sequence) 
                                  :test test :replace-all? replace-all?)))))


(defun mapping-substitution (elem expression-form element-mappings-forms 
                                  &key (->form 'lisp-form))
  (let* ((orig-elem-form (funcall ->form elem))
         (elem-form orig-elem-form))
    (cond ((entity? elem)
           (let ((corresponding-entity (second (assoc elem-form element-mappings-forms))))
             (substitute-nested (or corresponding-entity
                             (list :skolem elem-form))
                         elem-form
                         expression-form :test #'equal)))
          ((expression? elem)
           (dolist (child (children elem))
             (setf elem-form (mapping-substitution child elem-form 
                                                   element-mappings-forms
                                                   :->form ->form)))
           (if (equal orig-elem-form expression-form)
             elem-form
             (substitute-nested elem-form orig-elem-form expression-form :test #'equal)))                     
          (t elem-form))))


(defun entity-intersection-with-expression (item mapping which)
  (let ((MH (find item (mhs mapping) 
                  :key (if (eq which :base) 'base-item 'target-item))))
    (cond ((entity? item)
           (if MH
             (list (list item (if (eq which :base)
                                (target-item MH)
                                (base-item MH))))
             nil))
          ((expression? item)
           (mapcan #'(lambda (child) (entity-intersection-with-expression child mapping which))
             (children item)))
          (t nil))))             


(defun update-aas (root intersection current-aas mapping which &key (->form 'lisp-form))
  (let* ((root-form (funcall ->form root))
         (intersection-form (funcall ->form intersection))
         (intersection-form-w-ents  (funcall ->form (remove-duplicates (append intersection
                                                                               (entity-intersection-with-expression root mapping which))
                                                                       :test 'equal)))
         (mapping-substituted-root-form
          (mapping-substitution root root-form intersection-form-w-ents :->form ->form)))
    (dolist (ent-exp-mapping-form intersection-form)
      (let ((intersection-without-ent-exp-mapping-form 
             (remove ent-exp-mapping-form intersection-form-w-ents :test 'equal))
            (aas-entry (assoc ent-exp-mapping-form current-aas :test #'equal)))
        (if aas-entry
          (push (list root-form
                      mapping-substituted-root-form
                      intersection-without-ent-exp-mapping-form)
                (cdr aas-entry))
          (push (list ent-exp-mapping-form
                      (list root-form
                            mapping-substituted-root-form
                            intersection-without-ent-exp-mapping-form))
                current-aas))))
    current-aas))


;; Maximal-intersection-with-mapping returns a list of all entities/expressions
;;  that correspond in the target/base (I have also modified it so that it returns
;;  a list of pairs (item corresponding-item) 
;; This returns <item corresponding-item> reguardless of whether you are looking
;;   at base to target or target to base
(defun maximal-intersection-with-mapping (item mapping which)
  (let ((MH (find item (mhs mapping) 
                  :key (if (eq which :base) 'base-item 'target-item))))
    (cond ((entity? item)
           (if MH
             (list (list item (if (eq which :base) (target-item MH) (base-item MH))))
             nil))
          ((expression? item)
           (if MH 
             (list (list item (if (eq which :base) (target-item MH) (base-item MH))))
             (mapcan #'(lambda (child) (maximal-intersection-with-mapping child mapping which))
                                  (children item))))
          (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Displaying differences
;;;
;;; This is a simple test procedure, to experiment with explanations that might
;;; make sense before getting into HTML

(defun show-differences (mapping &key (stream *standard-output*)
                                 (->form 'lisp-form))
  (let* ((sme (sme mapping))
         (base (name (base sme)))
         (target (name (target sme))))
    (multiple-value-bind (ses property-differences alignable-absences nads)
        (summarize-differences mapping :->form ->form)
      (format stream "~%How ~A and ~A are different: [~A]" 
        base target ses)
      (cond (property-differences 
             (dolist (property-difference property-differences)
               ;; Format is (<exp> <score> <entity deltas>)
               ;; where each <entity delta> has the form
               ;; (<entity pairing> <base types> <target types>)
               (format stream "~%In ~A, [~D]" 
                 (pretty-print-to-string (package-entity-pairings
                                          (car property-difference)))
                 (cadr property-difference))
               (dolist (ep (third property-difference))
                 (format stream "~%  ~A is ~A, whereas ~A is ~A."
                   (cadr (car ep)) (generate-conjunctive-type-string (cadr ep))
                   (third (car ep)) (generate-conjunctive-type-string (third ep))))))
            (t (format stream "~%No property differences.")))
      (cond ((or (car alignable-absences)
                 (cadr alignable-absences))
             (show-alignable-absences-differences base target alignable-absences :stream stream))
            (t (format stream "~%No alignable absences.")))
      (cond ((or (car nads) (cadr nads))
             (format stream "~%Some other differences:")
             (when (car nads)
               (format stream "~%In ~A but not in ~A:" base target)
               (dolist (nad (car nads))
                 (format stream "~% ~A" (pretty-print-to-string nad))))
             (when (cadr nads)
               (format stream "~%In ~A but not in ~A:" target base)
               (dolist (nad (cadr nads))
                 (format stream "~% ~A" (pretty-print-to-string nad)))))
            (t (format stream "~%No nonalignable differences."))))))

(defun generate-conjunctive-type-string (type-list)
  (cond ((null type-list) "")
        (t (concatenate 'string 
             (format nil "a ~S" (car type-list))
             (if (and (cdr type-list) (null (cddr type-list))) " and "
               (if (null (cdr type-list)) "" ", "))
             (generate-conjunctive-type-string (cdr type-list))))))
 
(defun show-alignable-absences-differences (base target alignable-absences
                                                 &key (stream *standard-output*))
  (let ((base-aas (car alignable-absences))
        (target-aas (cadr alignable-absences)))   
    (when base-aas
      (format stream "~%The following are in ~S but absent from ~S~%" base target)
      (dolist (aa base-aas)
        (let ((base-elem (first (first aa)))
              (corresponding-elem (second (first aa)))
              (expressions-list (rest aa)))
          (print-alignable-absence-info base-elem corresponding-elem expressions-list 
                                        :stream stream))))
    (when target-aas
      (format stream "~%The following are in ~S but absent from ~S~%" target base)     
      (dolist (aa target-aas)
        (let ((target-elem (first (first aa)))
              (corresponding-elem (second (first aa)))
              (expressions-list (rest aa)))
          (print-alignable-absence-info target-elem corresponding-elem expressions-list 
                                        :stream stream))))))


(defun print-alignable-absence-info (elem corresponding-elem expressions
                                          &key (stream *standard-output*))  
  (format stream "~% By ~S <-> ~S~%" elem corresponding-elem)
  (dolist (expr expressions)
    (let ((root-expr (first expr))
          (mapping-substituted-expr (second expr))
          (additional-mappings (third expr)))
      (format stream "~S~%" root-expr)
      (format stream "   Conjecture: ~S~%" mapping-substituted-expr)
      
      (when additional-mappings
        (format stream "      Additional correspondences:")
        (dolist (add-mapp additional-mappings)
          (format stream " ~S <-> ~S" (first add-mapp) (second add-mapp)))
        (format stream "~%")))))

;;;; ---------------------------------------------------------------------------
;;; END OF CODE
