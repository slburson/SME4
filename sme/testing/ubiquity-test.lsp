;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: ubiquity-test.lsp
;;;;    System: SME
;;;;   Version: 4.0
;;;;    Author: Kenneth D. Forbus
;;;;   Created: November 29, 2006 09:10:15
;;;;   Purpose: Test stand for gathering SME-level statistics
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2012-01-16 17:38:54 -0600 (Mon, 16 Jan 2012) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------
(in-package :common-lisp-user)

;;;; Looks at effects of ubiquitous predicates.
;;;; Tricky, because not having the right ubiquitous predicates
;;;;  means one can blow heap easily.
;;;; This relies on utilities in test-stand.lsp.

(defun run-sme-ubiquitous-experiment
    (&key (results-path 
           #+mswindows "c:\\sme4-open\\data\\"
           #+linux "/data/qrg/temp/sme-data/"
           )
          (unique-prefix (generate-date-prefix)))
  (ensure-directories-exist results-path)
  ;;; Only run those tasks where ubiquitous predicates are used
  (format t "~% AP Physics")
  (gather-sme-ubiquitous-experiment-data
   (concatenate 'string *sme-data-path*
     #+mswindows "ap-physics\\"
     #+linux "ap-physics/"
     ))
  (sme-ubiquitous-data->cdf
   results-path
   (concatenate 'string unique-prefix "ap-physics"))
  (format t "~% Thermo")
  (gather-sme-ubiquitous-experiment-data
   (concatenate 'string *sme-data-path*
     #+mswindows "thermo\\"
     #+linux "thermo/"
     ))
  (sme-ubiquitous-data->cdf
   results-path
   (concatenate 'string unique-prefix "thermo"))
  (format t "~% MoralDM")
    (gather-sme-ubiquitous-experiment-data
   (concatenate 'string *sme-data-path*
     #+mswindows "moraldm\\"
     #+linux "moraldm/"
     ))
  (sme-ubiquitous-data->cdf
   results-path
   (concatenate 'string unique-prefix "moraldm"))
  )

(defun gather-sme-ubiquitous-experiment-data
    (path 
     &key (data-gatherer
           'recording-sme-ubiquitous-data-gatherer))
  (declare (special *data*))
  (setq *data* nil)
  (setf (sys:gsgc-switch :print) t)
  (setf (sys:gsgc-switch :stats) t)
  (setf (sys::gsgc-parameter :generation-spread) 5)
  (setf (sys:gsgc-parameter :free-percent-new) 35)
  (setf (sys:gsgc-parameter :expansion-free-percent-new) 35)
  (setf (sys::gsgc-parameter :expansion-free-percent-old) 35)
  (dolist (dehydrated-sme 
           (gather-dehydrated-smes-in-path path) *data*)
    (gather-ubiquitous-data-on-dehydrated-sme dehydrated-sme
      :data-gatherer data-gatherer)))

;;;; Processing a single file
(defun gather-ubiquitous-data-on-dehydrated-sme
    (file &key (data-gatherer 
                'recording-sme-ubiquitous-data-gatherer)
          (verbose? nil))
  (declare (special the-sme the-base))
  (let ((result (setup-dehydrated-sme file)))
    (cond ((eq result :error) ;; Something barfed during file loading
           ;; Mark as error and go on.
           (rename-file file (replace-type-in-file-name file "error")))
          (t (let ((time (time-dehydrated-sme)))
               (let ((before-sme the-sme)
                     (result (setup-dehydrated-sme file)))
                 (unless (eq result :error)
                   (setf (sme::ubiquitous-predicates 
                          (sme::vocabulary the-base)) nil)
                   (let ((no-ubiquitous-time (time-dehydrated-partial-sme)))
                     (funcall data-gatherer file before-sme time
                              the-sme no-ubiquitous-time)
                     (when verbose? 
                       (default-sme-data-gatherer file
                           before-sme time)
                       (default-sme-data-gatherer file
                           the-sme no-ubiquitous-time))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Timing 
;;; This relies on the microsoecond timer in Franz, 
;;; via procedures defined in test-stand.lsp

 
(defvar *ubi-timeout* 30.0) 

(defun time-dehydrated-partial-sme ()
  (declare (special the-sme))
  (sys::with-timeout (*ubi-timeout* :timeout)
    (with-microsecond-timing 
        (generate-mhs-for-timing the-sme))))

(defun generate-mhs-for-timing (sme)
  (setf (sme::unused-score sme) 0.0)
  (let* ((btime (sme::base-timestamp sme)) ;; last time base was examined.
         (ttime (sme::target-timestamp sme)) ;; last point target was examined.
         (bitems (sme::items-since (sme::base sme) btime))
         (titems (sme::items-since (sme::target sme) ttime)))
    (setf (sme::new-mappings sme) nil)
    ;; New base items against ALL target itmes
    (sme::match-all-expressions bitems (sme::expressions (sme::target sme)) sme)
    ;; New target items against ALL base itmes
    (sme::match-all-expressions (sme::expressions (sme::base sme)) titems sme)
    (length (sme::mhs sme)))) 


;;; Saving Results

(defun sme-ubiquitous-data->cdf (path &optional (tag ""))
  (declare (special *data*))
  (with-open-file (fout (format nil "~A~Acdf.txt"
                          path tag)
                        :direction :output
                        :if-exists :supersede)
    (let ((*print-length* 1000)
          (*print-level* 100)
          (*print-right-margin* 100000))
      (format fout "SME,Base,Target,#Ent(B),#Ent(T),#Exp(B),#Exp(T),Time,Okay?,#MHs,Ubi-Mhs,UbiT,#Ks,#Ms,#CIs-b,MeanCIs,AP,EndP,ExpP")
      (dolist (datum *data*)
        (let ((alist (cdr datum)))
          (let ((time (cdr (assoc :ubi-time alist))))
            (when (and (numberp time)
                       (> time 0.0))
              ;;; If it was so fast that it is under 1 millisecond, then ignore it.
              (format fout "~%~A" (car datum))
              (dolist (key '(:base-name :target-name :base-entities :target-entities
                                        :base-expressions :target-expressions
                                        :run-time :okay :mhs
                                        :ubi-mhs :ubi-time
                                        :kernels :mappings
                                        :ncis-best-mapping :mean-cis-all-mappings
                                        :mean-perplexity :mean-entity-perplexity
                                        :mean-expression-perplexity))
                (format fout ",~A" (cdr (assoc key alist)))))))))))

        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Data collection
;; When one doesn't know in advance what data is to be gathered, using
;; a flexible media like alists is best.  We'll make these headed alists
;; for simplicity


(defun recording-sme-ubiquitous-data-gatherer (file sme time partial-sme ubi-time)
  (declare (special *data*))
  (multiple-value-bind (total-p ent-p exp-p)
      (average-perplexity sme)
  (let ((datum (list (pathname-name file) ;; Use headed alist, with file name
                     (cons :base-name (sme::name (sme::base sme)))
                     (cons :target-name (sme::name (sme::target sme)))
                     (cons :run-time time)
                     (cons :okay t)
                     (cons :base-expressions (sme::expr-counter (sme::base sme)))
                     (cons :target-expressions (sme::expr-counter (sme::target sme)))
                     (cons :base-entities (sme::entity-counter (sme::base sme)))
                     (cons :target-entities (sme::entity-counter (sme::target sme)))
                     (cons :mhs (length (sme::mhs sme)))
                     (cons :ubi-mhs (length (sme::mhs partial-sme)))
                     (cons :ubi-time ubi-time)
                     (cons :kernels (length (sme::kernel-mappings sme)))
                     (cons :mappings (length (sme::mappings sme)))
                     (cons :mean-perplexity total-p)
                     (cons :mean-entity-perplexity ent-p)
                     (cons :mean-expression-perplexity exp-p)
                     (cons :ncis-best-mapping
                           (if (sme::mappings sme)
                             (length (sme::inferences (car (sme::mappings sme))))
                             0))
                     (cons :mean-cis-all-mappings
                           (if (sme::mappings sme)
                             (/ (apply '+ (mapcar (lambda (m)
                                                    (length (sme::inferences m)))
                                            (sme::mappings sme)))
                                
                                (float (length (sme::mappings sme))))
                             0)))))
    (push datum *data*)
    datum))) 

;;;; ---------------------------------------------------------------------------
;;;; End of Code
