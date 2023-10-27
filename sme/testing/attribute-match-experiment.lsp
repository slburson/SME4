;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: attribute-match-experiment.lsp
;;;;    System: SME
;;;;    Author: Ken Forbus
;;;;   Created: September 21, 2014 16:11:21
;;;;   Purpose: Model Keane attribute matching task
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2014-09-21 20:04:49 -0500 (Sun, 21 Sep 2014) $
;;;;  $LastChangedBy: forbus $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Holyoak and Thagard attribute matching task
;;; We think this is an extremely unnatural task, and not really a test of analogy
;;; at all.  But we note that number of remaps is compatible with what was found
;;; for people, if one uses representations that allow it to be treated as a comparison
;;; process.

;;;(smart steve)	(hungry fido)
;;;(tall bill)	(friendly rover)
;;;(smart bill)	(hungry rover)
;;;(tall tom)	(friendly blackie)
;;;(timid tom)	(frisky blackie)
;;;
;;;Singleton first condition (easy): Subjects mean time to solve = 178.0 seconds
;;; (Keane, in press)
;;; No SME remaps needed.
;;;
;;;(timid tom)	(hungry fido)
;;;(tall tom)	(frisky blackie)
;;;(tall bill)	(friendly blackie)
;;;(smart bill)	(hungry rover)
;;;(smart steve)	(friendly rover)
;;;Singleton last condition(hard): Subjects’ mean time to solve = 363.1 seconds
;;; (Keane, in press)
;;; 3 SME remaps needed.



(sme::defpredicate has-attribute (entity entity) relation)

(defvar *singleton-first-base* 
    '(:sfb (has-attribute steve smart)
      (has-attribute bill tall)
      (has-attribute bill smart)
      (has-attribute tom tall)
      (has-attribute tom timid)))

(defvar *singleton-first-target*
    '(:sft (has-attribute fido hungry)
      (has-attribute rover friendly)
      (has-attribute rover hungry)
      (has-attribute blackie friendly)
      (has-attribute blackie frisky)))

(defvar *singleton-last-base*
    '(:slb (has-attribute tom timid)
      (has-attribute tom tall)
      (has-attribute bill tall)
      (has-attribute bill smart)
      (has-attribute steve smart)))

(defvar *singleton-last-target*
    '(:slt (has-attribute fido hungry)
      (has-attribute blakie frisky)
      (has-attribute blackie friendly)
      (has-attribute rover hungry)
      (has-attribute over friendly)))

(defvar *attribute-match-pairs*
    (list (list *singleton-first-base*
                *singleton-first-target*)
          (list *singleton-last-base*
                *singleton-last-target*)))

(defun run-attribute-match-problem (base-spec target-spec)
  ;; Specs = (<case name> . <facts to be added in order>)
  (let* ((the-base (sme::define-description (car base-spec)
                       nil nil))
         (the-target (sme::define-description (car target-spec)
                         nil nil))
         (the-sme (sme::define-sme the-base the-target)))
    (do ((base-facts (cdr base-spec)
                     (cdr base-facts))
         (target-facts (cdr target-spec)
                       (cdr target-facts))
         (first? t)
         (remaps 0))
        ((or (null base-facts) (null target-facts)) ;; assume same length
         (values remaps the-sme))
      (sme::define-expression (car base-facts) the-base)
      (sme::define-expression (car target-facts) the-target)
      (cond (first? (setq first? nil)
                    (sme::match the-sme))
            (t (sme::incremental-match the-sme)
               (when (attribute-match-remap-needed? the-sme)
                 (sme::remap the-sme)
                 (incf remaps)))))))

(defun attribute-match-remap-needed? (the-sme)
  ;; Every entity needs to be matched to something in the mapping,
  ;; to satisfy the rules of this game.  Otherwise, must remap.
  (let ((mapping (car (sme::mappings the-sme)))) ;; assume first is best
    (cond ((null mapping) t) ;; Yikes, really should be a mapping
          (t (let ((mhs (sme::mhs mapping)))
               (not (and (every (lambda (b-ent)
                                  (some (lambda (mh)
                                          (eq (sme::base-item mh) b-ent)) mhs))
                                (sme::entities (sme::base the-sme)))
                         (every (lambda (b-ent)
                                  (some (lambda (mh)
                                          (eq (sme::base-item mh) b-ent)) mhs))
                                (sme::entities (sme::base the-sme))))))))))      
    
      

  





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code