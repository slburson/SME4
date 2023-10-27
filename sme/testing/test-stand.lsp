;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: test-stand.lsp
;;;;    System: SME
;;;;   Version: 4.0
;;;;    Author: Kenneth D. Forbus
;;;;   Created: November 29, 2006 09:10:15
;;;;   Purpose: Test stand for gathering SME-level statistics
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2016-01-24 $
;;;;  $LastChangedBy: ferguson $
;;;; ---------------------------------------------------------------------------
(in-package :common-lisp-user)

;;;; Doing empirical complexity analyses of SME's operations requires 
;;;; an efficient means of data collection.  These routines provide that
;;;; service.  

;;;; The big trick, of course, is to support a variety of different analyses with
;;;; minimal new infrastructure.  To do this, we provide some utilities and conventions
;;;; that let one do quick command-line programming.

;;;; Organizing a batch experiment
;; 
;; For simplicity, we will use one directory per experiment.
;; All .sme files in that directory will be run.
;; Result files are stored in a different directory, to simplify file operations.

(defparameter *sme-data-path* 
    #+:mswindows "c:\\sme4-open\\corpus\\"
    #+:unix "/data/qrg/temp/smes2014/"
  ) 

(defun run-sme-corpus-experiment (&key (results-path
                                        #+:mswindows "c:\\sme4-open\\data\\"
                                        #+:unix "/data/qrg/temp/sme-data/"
                                        )
                                       (unique-prefix
                                        (generate-date-prefix)))
  (ensure-directories-exist results-path)
  (format t "~%Moral DM")
  (gather-sme-batch-experiment-data
   (concatenate 'string *sme-data-path*
     #+:mswindows "moraldm\\"
     #+:unix "moraldm/"))
  (sme-batch-data->delimited-file
   results-path
   (concatenate 'string unique-prefix "moraldm"))
  (format t "~%Thermo")
  (gather-sme-batch-experiment-data
   (concatenate 'string *sme-data-path*
     #+:mswindows "thermo\\"
     #+:unix "thermo/"))
  (sme-batch-data->delimited-file
   results-path
   (concatenate 'string unique-prefix "thermo"))
  (format t "~%Geometry")
  (gather-sme-batch-experiment-data
   (concatenate 'string *sme-data-path*
     #+:mswindows "geometry\\"
     #+:unix "geometry/"))
  (sme-batch-data->delimited-file
   results-path
   (concatenate 'string unique-prefix "geometry")) 
  (format t "~%Oddity")
  (gather-sme-batch-experiment-data
   (concatenate 'string *sme-data-path*
     #+:mswindows "oddity\\"
     #+:unix "oddity/"))
  (sme-batch-data->delimited-file
   results-path
   (concatenate 'string unique-prefix "oddity"))
  (format t "~%AP Physics")
  (gather-sme-batch-experiment-data
   (concatenate 'string *sme-data-path*
     #+:mswindows "ap-physics\\"
     #+:unix "ap-physics/"))
  (sme-batch-data->delimited-file
   results-path
  (concatenate 'string unique-prefix "ap-physics"))
  )

(defun generate-date-prefix ()
  (multiple-value-bind (seconds minutes hour date month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore seconds minutes hour))
    (format nil "~A~2,'0D~2,'0D" year month date)))


(defun test-sme-batch-experiment (path)
  (gather-sme-batch-experiment-data
   path :data-gatherer 'default-sme-data-gatherer
   :verification-failure-handler
   'default-dehydration-verification-handler)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gathering the data
;;;;
;;;; data-gatherer is the procedure that gathers the data on an SME.
;;;; verification-failure-handler takes care of SMEs that don't pass 
;;;;   the internal verification tests provided by dehydration.

(defun gather-sme-batch-experiment-data
    (path 
     &key (data-gatherer
           'recording-sme-data-gatherer)
     (verification-failure-handler
      'recording-dehydration-verification-handler))
  (declare (special *data*))
  (setq *data* nil)
#+allegro  (set-gc-switches)
  (let ((n-smes 0))
    (dolist (dehydrated-sme 
             (gather-dehydrated-smes-in-path path) n-smes)
      (gather-data-on-dehydrated-sme dehydrated-sme
                                     :data-gatherer data-gatherer
                                     :verification-failure-handler
                                     verification-failure-handler)))
  *data*)

#+allegro
(defun set-gc-switches ()
  (setf (sys:gsgc-switch :print) t)
  (setf (sys:gsgc-switch :stats) t)
  (setf (sys::gsgc-parameter :generation-spread) 5)
  (setf (sys:gsgc-parameter :free-percent-new) 35)
  (setf (sys:gsgc-parameter :expansion-free-percent-new) 35)
  (setf (sys::gsgc-parameter :expansion-free-percent-old) 35))
 
(defun gather-dehydrated-smes-in-path (path)
  (directory (concatenate 'string path "*.sme")))

;;;; Processing a single file   

(defun gather-data-on-dehydrated-sme (file &key (data-gatherer 
                                                 'default-sme-data-gatherer)
                                           (verbose? nil)
                                           (verification-failure-handler 
                                            'default-dehydration-verification-handler))
  (declare (special the-sme))
  (let ((result (setup-dehydrated-sme file)))
    (cond ((eq result :error) ;; Something barfed during file loading
           ;; Mark as error and go on.
           (rename-file file (replace-type-in-file-name file "error")))
          (t (let ((time (time-dehydrated-sme)))
               (multiple-value-bind (o-mhs n-mhs o-mappings n-mappings)
                   (verify-dehydration-result)
                 (cond ((and (null o-mhs) (null n-mhs)
                             (null o-mappings)
                          (null n-mappings))
                        (cond ((= time 0.0)
                               (rename-file file (replace-type-in-file-name file "too-small"))
                               (format t "~% Too small to time: ~A." file))
                          (t (funcall data-gatherer file the-sme time)
                            
;;;                            (rename-file file (replace-type-in-file-name file "done"))
                            (when verbose? (default-sme-data-gatherer file the-sme time)))))
                   (t (funcall verification-failure-handler
                               file the-sme time o-mhs n-mhs o-mappings n-mappings)))))))))

(defun restore-to-sme-file-type (dir)
  (dolist (fptr (directory (concatenate 'string dir "*.too-small")))
    (rename-file fptr (replace-type-in-file-name fptr "sme"))))

(defun default-sme-data-gatherer (file sme time)
  (format t "~%~A: " file)
  (format t "~% Base: ~A" (sme::name (sme::base sme)))
  (format t "~% Target: ~A" (sme::name (sme::target sme)))
  (format t "~% Time: ~D" time)
  (format t "~% |B| = ~D; |T| = ~D; |MHs| = ~D; |K| = ~D; |M| = ~D."
    (sme::expr-counter (sme::base sme))
    (sme::expr-counter (sme::target sme))
    (length (sme::mhs sme))
    (length (sme::kernel-mappings sme))
    (length (sme::mappings sme))))

(defun default-dehydration-verification-handler (file sme time 
                                                o-mhs n-mhs
                                                o-mappings n-mappings)
  (format t "~%Dehydration failure!")
  (when o-mhs
    (format t "~D MHs unique in dehydrated" (length o-mhs))
    (pprint o-mhs))
  (when n-mhs
    (format t "~D MHs unique in reconstituted" (length n-mhs))
    (pprint n-mhs))
  (when o-mappings
    (format t "~D MHs unique in dehydrated" (length o-mappings))
    (pprint o-mappings))
  (when n-mappings
    (format t "~D MHs unique in reconstituted" (length n-mappings))
    (pprint n-mappings))
  (default-sme-data-gatherer file sme time))

(defun setup-dehydrated-sme (file)
  (makunbound '*vocab*)
  (makunbound 'the-base)
  (makunbound 'the-target)
  (makunbound 'the-match-constraints)
  (makunbound 'the-sme)
  (makunbound 'the-results)
  (gc)
  (qrg::trap-error (error :error) (load file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Timing & verification
;; Now using run-time, instead of real-time, to factor out 
;; as much OS-level stuff as ACL allows.
;;
;; One annoying problem: SME is so fast on modern machines
;; that it's below the resolution of timing routines on
;; most CPUs for small examples.  Earlier versions tried running
;; small examples multiple times and averaging, but that's just too 
;; kludgy, and besides, there are plenty of big examples around.
;; Allegro handles this via some extensions.

;; RWF, 2015/01/24: with-fine-timing is ANSI compliant, which is 
;; has millisecond accuracy on LispWorks and SBCL, while Clozure
;; does microsecond timing.

#+allegro
(defmacro with-microsecond-timing (form)
  `(multiple-value-bind (start-seconds start-microseconds)
       (excl::acl-internal-real-time)
     ,form
     (multiple-value-bind (end-seconds end-microseconds)
         (excl::acl-internal-real-time)
       (+ (float (- end-seconds start-seconds))
          (* 1.0e-6 (- end-microseconds start-microseconds))))))               

(defmacro with-fine-timing (form)
  "Time the interal form, returning the time as a floating point number.
   Precision is either milliseconds or microseconds depending on Lisp
   implementation."
  `(let ((start-time-units (get-internal-real-time)))
      ,form
      (let ((end-time-units (get-internal-real-time)))
         (float (/ (- end-time-units start-time-units) 
                   internal-time-units-per-second)))))

(defun time-dehydrated-sme ()
  (declare (special the-sme the-match-constraints))
#+allegro
  (with-microsecond-timing (sme::match-with-appropriate-filters                           
                            the-sme the-match-constraints))
#-allegro
  (with-fine-timing (sme::match-with-appropriate-filters                           
                     the-sme the-match-constraints))
  )


(defun verify-dehydration-result ()
  (declare (special the-sme the-results))
  (let ((temporary-file 
         (make-qrg-file-name (make-data-path) "temporary-sme.sme")))
    (sme:dehydrate-sme the-sme temporary-file)
    (let ((reconstituted-result 
           (find-the-result-in-dehydrated-file temporary-file)))
      (multiple-value-bind (old-not-new-mhs new-not-old-mhs)
          (sme::compare-sme-mh-results the-results reconstituted-result)
        (multiple-value-bind (old-not-new-mappings new-not-old-mappings)
            (sme::compare-sme-mapping-results the-results reconstituted-result)
          (delete-file temporary-file)
          (values old-not-new-mhs new-not-old-mhs 
                  old-not-new-mappings new-not-old-mappings))))))

(defun find-the-result-in-dehydrated-file (file)
  (let ((eof nil))
    (with-open-file (fin file :direction :input)
      (do ((form (read fin nil eof)
                 (read fin nil eof)))
          ((eq form eof) nil)
        (when (and (listp form)
                   (eq (car form) 'setq)
                   (eq (cadr form) 'the-results))
          (return-from find-the-result-in-dehydrated-file
            (values (cadr (third form)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dumping data
;;;; This uses the procedures in alist-template-data.lsp.  We read and write
;;;; data from delimited files for compactness.  Dumping the whole alist as a readable
;;;; expression is so redundant that the files get gigantic.
;;;; N. B. Previous versions sometimes did filtering before dumping.  Can't do
;;;; that here, so the data must be filtered and/or massaged before it gets here.

(defparameter *sme-complexity-analysis-template*
    '((:file "File" :string)
      (:sme "SME" :other)
      (:okay "Okay?" :other)
      (:base-name "Base" :other)
      (:target-name "Target" :other)
      (:base-entities "#Ent(B)" :other)
      (:target-entities "#End(T)" :other)
      (:base-expressions "#Exp(B)" :other)
      (:target-expressions "#Exp(T)" :other)
      (:mhs "#Mhs" :other)
      (:kernels "#Ks" :other)
      (:kernel-sizes "KSizes" :other)
      (:mappings "#Mps" :other)
      (:mean-perplexity "AP" :other)
      (:mean-entity-perplexity "EntP" :other)
      (:mean-expression-perplexity "ExpP" :other)
      (:best-score "NSim(B)" :other)
      (:mapping-scores "NSim(Mps)" :other)
      (:kernel-scores "NSim(Ks)" :other)
      (:run-time "Time" :other))
  "Parameters gathered during SME complexity analyses.")

(defun sme-batch-data->delimited-file (path &optional (tag ""))
  (declare (special *data*))
  (alist-data->keyed-delimited-file *data*
    *sme-complexity-analysis-template*
    (concatenate 'string path tag "_cdf.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Match perplexity
;;;; The number of choices available may be what governs concrete complexity.
;;;; These routines calculate perplexity values for a given SME.  

(defun average-entity-perplexity (sme)
  (let ((the-base (sme::base sme))
        (n 0)
        (sum 0))
    (sme::map-over-entities
     (lambda (e) (incf n)
       (incf sum (length (sme::mhs-involving-base e sme))))
     the-base)
    (values (/ (float sum) (float n))
            n sum)))

(defun average-expression-perplexity (sme)
  (let ((the-base (sme::base sme))
        (n 0)
        (sum 0))
    (sme::map-over-expressions
     (lambda (e) (incf n)
       (incf sum (length (sme::mhs-involving-base e sme))))
     the-base)
    (values (/ (float sum) (float n))
            n sum)))

(defun average-perplexity (sme)
  (multiple-value-bind (ent-p ent-n ent-sum)
      (average-entity-perplexity sme)
    (multiple-value-bind (exp-p exp-n exp-sum)
        (average-expression-perplexity sme)
      (values (/ (+ (float ent-sum) (float exp-sum))
                 (+ (float ent-n) (float exp-n)))
              ent-p exp-p))))
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Data collection
;; When one doesn't know in advance what data is to be gathered, using
;; a flexible media like alists is best.  We'll make these headed alists
;; for simplicity
;;
;;
;; The parameters gathered will be defined by a template, an alist
;; whose entries are (<key> <:sme, :mapping, or :none> <procedure>)
;; :sme, :mapping determine what <procedure> will be called on.
;; The file will always be included, with the key :file,
;; the time will be included, with the key :run-time.

(defparameter *sme-data-to-gather*  
    '((:base-name :sme (lambda (s) (sme::name (sme::base s))))
      (:target-name :sme (lambda (s) (sme::name (sme::target s))))
      (:base-expressions :sme (lambda (s) (sme::expr-counter (sme::base s))))
      (:target-expressions :sme (lambda (s) (sme::expr-counter (sme::target s))))
      (:base-entities :sme (lambda (s) (sme::entity-counter (sme::base s))))
      (:target-entities :sme (lambda (s) (sme::entity-counter (sme::target s))))
      (:mhs :sme (lambda (s) (length (sme::mhs s))))
      (:kernels :sme (lambda (s) (length (sme::kernel-mappings s))))
      (:kernel-sizes :sme (lambda (s)
                            (mapcar (lambda (m)
                                      (length (sme::mhs m)))
                              (sme::kernel-mappings s))))
      (:mappings :sme (lambda (s) (length (sme::mappings s))))
      (:best-score :mapping (lambda (m) (sme::score m)))
      (:mapping-scores :sme (lambda (s) (mapcar 'sme::score (sme::mappings s))))
      (:kernel-scores :sme (lambda (s) (mapcar 'sme::score (sme::kernel-mappings s))))
      (:mean-perplexity :sme (lambda (s) (average-perplexity s)))
      (:mean-entity-perplexity :sme (lambda (s) (average-entity-perplexity s)))
      (:mean-expression-perplexity :sme (lambda (s) (average-expression-perplexity s)))
      ))

(defun recording-sme-data-gatherer (file sme time)
  (declare (special *data*))
  (let ((datum (list (cons :file (pathname-name file))
                 (cons :run-time time)
                 (cons :sme (sme::id sme))))
        (best-mapping (car (sme::mappings sme))))
    (dolist (spec *sme-data-to-gather*)
      (let ((key (car spec))
            (which (cadr spec))
            (procedure (third spec)))
        (let ((value (if (eq which :sme)
                       (funcall procedure sme)
                       (when (and (eq which :mapping)
                               (sme::mapping? best-mapping))
                         (funcall procedure best-mapping)))))
          (push (cons key value) datum))))
    (push datum *data*)
    datum))

(defun recording-dehydration-verification-handler (file sme time 
                                                        o-mhs n-mhs
                                                        o-mappings n-mappings)
  (format t "~%Dehydration failure!")
  (when o-mhs
    (format t "~D MHs unique in dehydrated" (length o-mhs))
    (pprint o-mhs))
  (when n-mhs
    (format t "~D MHs unique in reconstituted" (length n-mhs))
    (pprint n-mhs))
  (when o-mappings
    (format t "~D Mapping(s) unique in dehydrated" (length o-mappings))
    (pprint o-mappings))
  (when n-mappings
    (format t "~D Mapping(s) unique in reconstituted" (length n-mappings))
    (pprint n-mappings))
  (let ((datum (recording-sme-data-gatherer file sme time)))
    (let ((entry (assoc :okay (cdr datum))))
      (when entry
        (setf (cdr entry)
          (list o-mhs n-mhs o-mappings n-mappings))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Data verification
;;
;; We want to ensure that our data does not include redundancies.
;; If someone ran a performance system over and over again, they may have done
;; the same mapping more than once, and we want to detect that.  It's a bit 
;; tricky due to incremental expansion of dgroups; the base and target could
;; be the same, but compared at different times.  Hence we have to look more
;; closely in the cases where they are named the same.

(defun find-overlapping-dehydrated-smes (path)
  (let ((sme-entries (load-smes-in-path path))
        (dupes-table nil)
        (total 0)
        (empty nil) ;; Either base or target is empty
        (redundancies nil)) ;; Redundant
    (dolist (sme-entry sme-entries)
      (incf total)
      (let* ((sme (cdr sme-entry))
             (key (cons (sme::name (sme::base sme))
                        (sme::name (sme::target sme))))
             (entry (assoc key dupes-table :test 'equal)))
        (cond ((or (= (sme::expr-counter (sme::base sme)) 0)
                   (= (sme::expr-counter (sme::target sme)) 0))
               (push sme-entry empty))
              (entry (push sme-entry (cdr entry)))
              (t (push (list key sme-entry) dupes-table)))))
   ;; (setq *dupes-table* dupes-table)
    (dolist (entry dupes-table (values total empty redundancies))
      (if (cddr entry) ;; More than one with same named base and target
        ;; Can be more than two, so divide into equivalence classes
        (dolist (equiv-class
                 (sort-into-equivalence-classes 
                  (cdr entry) #'(lambda (e1 e2)
                                  ;; e1, e2 = (<file> . <sme>)
                                  (and (dgroups-equal? (sme::base (cdr e1))
                                                       (sme::base (cdr e2)))
                                       (dgroups-equal? (sme::target (cdr e1))
                                                       (sme::target (cdr e2)))))))
          (if (cdr equiv-class)
            (push equiv-class redundancies)))))))

(defun delete-duplicate-dehydrated-files (path)
  (let ((losers 0))
    (multiple-value-bind (total empty redundancies)
        (find-overlapping-dehydrated-smes path)
      (format t "~% For ~A: ~D total, ~D with empty base or target."
        path total (length empty))
      (format t "~%  ~D sets of redundancies, ~D extras."
        (length redundancies)
        (- (apply '+ (mapcar 'length redundancies)) (length redundancies)))
      (dolist (entry empty)
        (incf losers)
        (delete-file (car entry)))
      (dolist (dupes redundancies losers)
        (dolist (dup-entry (cdr dupes)) ;; Keep first (random)
          (incf losers)
          (delete-file (car dup-entry)))))))
    
(defun load-smes-in-path (path)
  (declare (special the-sme))
  (let ((smes nil))
    (dolist (file (gather-dehydrated-smes-in-path path)
                  smes)
      (load file)
      (push (cons file the-sme) smes))))

(defun extract-forms-from-dehydrated-sme-file (file)
  ;; We grab everything we can by going through once, since
  ;; file operations are slow.
  (let ((eof (cons nil nil))
        (the-base-form nil)
        (the-target-form nil)
        (vocabulary-forms nil)
        (the-sme-form nil)
        (the-results-form nil))
    (with-open-file (fin file :direction :input)
      (do ((form (read fin nil eof)
                 (read fin nil eof)))
          ((eq form eof)
           (values vocabulary-forms the-base-form the-target-form
                   the-sme-form the-results-form))
        (when (listp form)
          (case (car form)
            ((sme::rehydratePredicate
              sme::defubiquitous-predicate)
             (push form vocabulary-forms))
            (setq
             (case (cadr form)
               (the-base (setq the-base-form form))
               (the-target (setq the-target-form form))
               (the-sme (setq the-sme-form form))
               (the-results (setq the-results-form form))))))))))

(defun dgroups-equal? (dg1 dg2)
  (and (null (set-exclusive-or (mapcar 'sme::entity-name (sme::entities dg1))
                               (mapcar 'sme::entity-name (sme::entities dg2))
                               :test 'equal))
       (null (set-exclusive-or (mapcar 'sme::lisp-form-wo-isas
                                 (sme::expressions dg1))
                               (mapcar 'sme::lisp-form-wo-isas
                                 (sme::expressions dg2))))))

(defun sort-into-equivalence-classes (items test)
  (cond ((null items) nil)
        ((null (cdr items)) (list (list (car items))))
        (t (let ((item (car items))
                 (equivs nil)
                 (others nil))
             (dolist (other (cdr items))
               (if (funcall test item other)
                 (push other equivs)
                 (push other others)))
             (cons (cons item equivs)
                   (sort-into-equivalence-classes others test))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Dealing with missing ubiquitous predicates
;;; An early version of the dehydrator didn't dump ubiquitous predicates.
;;; Ouch.  
;;; Move them to a subdirectory of the current path, to avoid biasing the results

(defun remove-data-without-ubiquitous-predicates (path)
  (let ((total 0)
        (losers 0))
    (dolist (file (gather-dehydrated-smes-in-path path)
                  (values losers total))
      (incf total)
      (unless (file-contains-string? file "defubiq")
        (incf losers)
        (copy-file-to file (concatenate 'string path "broken\\"))
        (delete-file file)))))

(defun file-contains-string? (file string)
  (let ((eof (cons nil nil)))
    (with-open-file (fin file :direction :input)
      (do ((line (read-line fin nil eof)
                 (read-line fin nil eof)))
          ((eq line eof) nil)
        (when (search string line)
          (return-from file-contains-string? (values t)))))))

(defun copy-file-to (file new-path)
  ;; Shame this isn't built-in
  (let ((eof (cons nil nil)))
    (with-open-file (fin file
                         :direction :input)
      (with-open-file (fout 
                       (make-pathname :name (pathname-name file)
                                      :type (pathname-type file)
                                      :defaults new-path)
                            :direction :output
                       :if-exists :supersede)
        (do ((line (read-line fin nil eof)
                   (read-line fin nil eof)))
            ((eq line eof) fout)
          (write-line line fout))))))

(defun add-extension-to-files (path new)
  (dolist (file (directory 
                 (concatenate 'string path "*")))
    (rename-file file
                 (make-pathname :defaults file
                                :type new))))

(defun replace-type-in-file-name (file-namestring new-type)
  (let ((old-pathname (parse-namestring file-namestring)))
    (namestring (make-pathname
                 :name (pathname-name old-pathname)
                 :type new-type
                 :host (pathname-host old-pathname)
                 :device (pathname-device old-pathname)
                 :directory (pathname-directory old-pathname)))))

(defun change-extensions-from-to (path old new)
  (dolist (file (directory
                 (concatenate 'string path "*." old)))
    (rename-file file
      (make-pathname :defaults file
        :type new))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Gathering information about facts in dgroups

;;;; Given a directory of dehydrated SMEs, this procedure tabulates
;;;; the min, max, and mean number of facts, entities, attributes, nats and relations.
;;;; N.B. facts are top-level statements, so there could be more relations than
;;;; facts, hence the separate count.

(defun extract-dgroup-stats (dgroup)
  (let ((n-entities (sme::entity-counter dgroup))
        (n-facts (length (sme::roots dgroup)))
        (n-relns 0)
        (n-nats 0)
        (n-attributes 0))
    (sme::map-over-expressions (lambda (exp)
                                 (if (sme::attribute? exp) (incf n-attributes)
                                   (if (sme::function? exp) (incf n-nats)
                                     (incf n-relns))))
      dgroup)
    (values n-entities n-facts n-attributes n-nats n-relns)))


(defstruct integer-stats-counter
  (id "" :type t)
  (min 100000 :type integer)
  (max -10000 :type integer)
  (sum 0 :type integer)
  (count 0 :type integer))

(defun increment-stats-counter (new-datum counter)
  (setf (integer-stats-counter-sum counter)
    (incf (integer-stats-counter-sum counter) new-datum))
  (if (> new-datum (integer-stats-counter-max counter))
    (setf (integer-stats-counter-max counter) new-datum))
  (if (< new-datum (integer-stats-counter-min counter))
    (setf (integer-stats-counter-min counter) new-datum))
  (incf (integer-stats-counter-count counter)))

(defun tabulate-dgroup-directory-stats (path)
  (declare (special the-base the-target))
  (let ((prior-dgroups nil)
        (stats-counters
         (list (make-integer-stats-counter :id :entities)
           (make-integer-stats-counter :id :facts)
           (make-integer-stats-counter :id :attributes)
           (make-integer-stats-counter :id :nats)
           (make-integer-stats-counter :id :relns))))
    (dolist (dehydrated (directory path) stats-counters)
      (load dehydrated)
      (unless (member the-base prior-dgroups :test 'dgroups-equal?)
        (mapc (lambda (value counter)
                (increment-stats-counter value counter))
          (multiple-value-list (extract-dgroup-stats the-base))
          stats-counters)
        (push the-base prior-dgroups))
      (unless (member the-target prior-dgroups :test 'dgroups-equal?)
        (mapc (lambda (value counter)
                (increment-stats-counter value counter))
          (multiple-value-list (extract-dgroup-stats the-target))
          stats-counters)
        (push the-target prior-dgroups)))))


(defun summarize-integer-stats-counter (c &key (stream *standard-output*))
  (format stream "~%~A: ~D total: Min=~D, Max=~D, Mean=~D, Sum=~D."
    (integer-stats-counter-id c)
    (integer-stats-counter-count c)
    (integer-stats-counter-min c)
    (integer-stats-counter-max c)
    (/ (float (integer-stats-counter-sum c))
      (float (integer-stats-counter-count c)))
    (integer-stats-counter-sum c)))

(defun summarize-integer-stats-counters (counter-list &key (stream *standard-output*))
  (dolist (c counter-list) (summarize-integer-stats-counter c :stream stream)))
           

;;;; ---------------------------------------------------------------------------
;;;; End of Code
