;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: dehydrate-tools.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Kenneth D. Forbus
;;;;   Created: September 21, 2007 10:49:49
;;;;   Purpose: Tools to help with experiments involving dehydrated SMEs
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2014-10-07 16:06:51 -0500 (Tue, 07 Oct 2014) $
;;;;  $LastChangedBy: forbus $
;;;; ---------------------------------------------------------------------------
(in-package :common-lisp-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Statistics on dehydrated SMEs 
;;; The dehydrator's job is to make data collection easy.  This is the other half,
;;; calculating statistics on accumulated data.  These utilities expect a data set
;;; to be delimited via directories, with one data set per directory.  Recursion
;;; into subdirectories is not currently supported, since no existing experiments
;;; do that.
;;;
;;; These procedures store their results in global variables for simplicity.
;;; Typically the results are headed alists, to simplify dumping data.

;;; API
;;;
;;; (dgroup-statistics-for-dehydrated-data-set <directory>)
;;;   gathers statistics about the dgroups in the dehydrated SMEs in <directory>
;;;   Assumes unique names for dgroups, and avoids double-counting.
;;;
;;; (dgroup-statistics-for-dehydrated-data-sets <directory-list>)
;;;   Like the above, but accumulates across multiple data sets
;;;
;;; (dump-dgroup-statistics <file-path>)
;;;   Dumps comma delimited file with the contents of *dgroup-data*
;;;   for easy spreadsheet processing.
;;;
;;; Internal helper procedures for writing your own analyses:
;;;
;;; (clear-dehydration-variables) 
;;;  sets globals used in rehydrating to nil, to free up space.
;;;
;;; (calculate-statistics-for-dehydrated-sme <file-name> <stats-procedure>)
;;;   loads <file-name>, calculates data by calling <stats-procedure>, then
;;;   wipes the globals, so that the garbage collector can reclaim space.
;;;
;;; (gather-statistics-for-dehydrated-data-set <directory> <procedure>)
;;;   runs <procedure> over every dehydrated SME in <directory>
;;;
;;; 

(defun clear-dehydration-variables ()
  (declare (special *vocab* the-base the-target the-match-constraints the-sme the-results))
  (setq *vocab* nil
    the-base nil
    the-target nil
    the-match-constraints nil
    the-sme nil
    the-results nil))

(defun calculate-statistics-for-dehydrated-sme (file-name stats-procedure)
  ;; ***** Should add error handling, ideally logging
  (let ((result (qrg::trap-error (error :error)
                  (load file-name))))
    (cond ((eq result :error)
           (format t "~%Rehydration error: ~A." file-name))
          (t (funcall stats-procedure)
             (clear-dehydration-variables)))))


(defun pathname-file-name-and-type (p)
  (format nil "~A.~A" (pathname-name p) (pathname-type p)))

(defun gather-statistics-for-dehydrated-data-set (stats-procedure directory
                                                                  &key (file-type "sme"))
  (dolist (file-name (directory (format nil
                                    "~A*.~A" directory file-type)))
    (calculate-statistics-for-dehydrated-sme 
     file-name
     (lambda () (funcall stats-procedure
                         (pathname-file-name-and-type file-name)
                         directory)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gathering dgroup statistics
;;;
;;; Each data point is a headed alist, with the head being the dgroup name.
;;; The entries are
;;;  :file <file name>
;;;  :path <file path>
;;;  :n-expressions <number of expressions>
;;;  :n-attributes <number of expressions that are attributes>
;;;  :n-relations <number of expressions that are relations>
;;;  :n-NATs <number of expressions that are non-atomic terms>
;;;  :max-order <maximum order of expressions in dgroup>
;;;  :n-entities <number of entities>
;;;  :n-relation-functors <number of unique relations used>
;;;  :n-attribute-functors <number of unique attributes used>
;;;  :n-function-functors <number of unique functions used>

(defvar *dgroup-data* nil)
(defvar *dgroups-loaded* nil)

(defun gather-dgroup-statistics-for-dehydrated-sme (file-name directory)
  (declare (special the-base the-target))
  (gather-dgroup-statistics-for the-base file-name directory)
  (gather-dgroup-statistics-for the-target file-name directory))

(defun same-dgroup-facts? (dg1 dg2)
  (let ((facts1 (cdr (sme::lisp-form dg1)))
        (facts2 (cdr (sme::lisp-form dg2))))
    (not (or (set-difference facts1 facts2 :test 'equal)
             (set-difference facts2 facts1 :test 'equal)))))

(defun gather-dgroup-statistics-for (dgroup file-name directory)
  (unless (some (lambda (prior) (same-dgroup-facts? dgroup prior))
                *dgroups-loaded*) ;; Avoid recording duplicates
    (push dgroup *dgroups-loaded*)
    (multiple-value-bind (n-relations n-attributes n-NATs
                                      n-relation-functors n-attribute-functors
                                      n-function-functors)
        (find-unique-functor-counts dgroup)
      (push `(,(sme::name dgroup) 
                (:file . ,file-name)
                (:path . ,directory)
                (:n-expressions . ,(sme::expression-count dgroup))
                (:n-entities . ,(sme::entity-count dgroup))
                (:n-attributes . ,n-attributes)
                (:n-relations . ,n-relations)
                (:n-NATs . ,n-NATs)
                (:max-order . ,(sme::max-order dgroup))
                (:n-relation-functors . ,n-relation-functors)
                (:n-attribute-functors . ,n-attribute-functors)
                (:n-function-functors . ,n-function-functors))
            *dgroup-data*))))

(defun find-unique-functor-counts (dgr)
  (let ((relations-count 0)
        (attributes-count 0)
        (nat-count 0)
        (relation-functors nil) ;; including logical
        (attribute-functors nil) 
        (function-functors nil))
    (dolist (exp (sme:get-dgroup-expressions dgr)
                 (values relations-count attributes-count nat-count
                         (length relation-functors)
                         (length attribute-functors)
                         (length function-functors)))
      (let ((functor (sme::functor exp)))
        (case (sme::pred-type functor)
          (:attribute (incf attributes-count)
                      (pushnew functor attribute-functors))
          ((:relation :logical) (incf relations-count)
           (pushnew functor relation-functors))
          (:function (incf nat-count)
                     (pushnew functor function-functors)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;

(defun tabulate-dgroup-statistics (smes-path output-path output-prefix
                                             &optional
                                             (subdirs
                                              '("moraldm" "geometry" "oddity" "ap-physics" "thermo"))
                                             (separator #+mswindows "\\" 
                                                        #+linux "/"
                                                        ))
                                                       
  (dolist (subdir subdirs)
    (dgroup-statistics-for-dehydrated-data-set
     (concatenate 'string smes-path subdir separator))
    (dump-dgroup-statistics 
     (concatenate 'string output-path output-prefix subdir "_cdf.txt"))))
           

(defun dgroup-statistics-for-dehydrated-data-set (directory)
  (setq *dgroup-data* nil)
  (setq *dgroups-loaded* nil)
  (gather-statistics-for-dehydrated-data-set
   'gather-dgroup-statistics-for-dehydrated-sme directory)
  *dgroup-data*)

(defun dgroup-statistics-for-dehydrated-data-sets (directory-list)
  (setq *dgroup-data* nil)
  (dolist (directory directory-list)
    (gather-statistics-for-dehydrated-data-set
     'gather-dgroup-statistics-for-dehydrated-sme directory))
  *dgroup-data*)

(defun dump-dgroup-statistics (file-out)
  ;; uses global *dgroup-data*
  (with-open-file (fout file-out :direction :output
                        :if-exists :supersede)
    (write-dgroup-statistics-column-headers fout)
    (dolist (datum *dgroup-data*)
      (write-dgroup-statistics-cdf datum fout))))

(defun write-dgroup-statistics-column-headers (&optional (stream *standard-output*))
  (format stream "Date: ~A" (generate-date-prefix))
  (format stream "~%,Name,#Ent,#Exp,#Att,#Reln,#NATs,MaxOrd,RelCount,AttCount,FnCount,file,path"))

(defun write-dgroup-statistics-cdf (datum &optional (stream *standard-output*))
  (let ((name (car datum))
        (alist (cdr datum))
        (*print-right-margin* 100000))
    (let ((file (cdr (assoc :file alist)))
          (path (cdr (assoc :path alist)))
          (n-expressions (cdr (assoc :n-expressions alist)))
          (n-entities (cdr (assoc :n-entities alist)))
          (n-attributes (cdr (assoc :n-attributes alist)))
          (n-relations (cdr (assoc :n-relations alist)))
          (n-NATs (cdr (assoc :n-NATs alist)))
          (max-order (cdr (assoc :max-order alist)))
          (n-relation-functors (cdr (assoc :n-relation-functors alist)))
          (n-attribute-functors (cdr (assoc :n-attribute-functors alist)))
          (n-function-functors (cdr (assoc :n-function-functors alist))))
      (cond ((and (numberp n-expressions)(numberp n-entities)(numberp n-relations)
                  (numberp n-NATs) (numberp max-order)(numberp n-relation-functors)
                  (numberp n-attribute-functors) (not (numberp file)) (not (numberp path)))
             (format stream "~%,~S,~D,~D,~D,~D,~D,~D,~D,~D,~D,~S,~S"
               name n-entities n-expressions n-attributes n-relations n-NATs max-order
               n-relation-functors n-attribute-functors n-function-functors
               file path))
            (t (error "Bad Dgroup datum: ~A" datum))))))

(defun dgroup-stats-cdf->case-names (cdf-file)
  (let ((marker (cons cdf-file nil))
        (case-names nil))
    (with-open-file (fin cdf-file :direction :input)
      (read-line fin) ;; Get rid of header line
      (do ((line (read-line fin nil marker)
                 (read-line fin nil marker)))
          ((eq line marker) case-names)
        (multiple-value-bind (field-value next-start)
            (string-to-next-comma line 1)
          (declare (ignore next-start))
          (push (read-from-string field-value) case-names))))))

(defun string-to-next-comma (string start-pos)
  (let ((end-pos (position #\, string :start start-pos)))
    (values (subseq string start-pos end-pos) (1+ end-pos))))

(defun sme-stats-cdf->case-names (cdf-file)
  (let ((marker (cons cdf-file nil))
        (case-names nil))
    (with-open-file (fin cdf-file :direction :input)
      (read-line fin) ;; Get rid of header lines
      (read-line fin)
      (do ((line (read-line fin nil marker)
                 (read-line fin nil marker)))
          ((eq line marker) case-names)
        ;; Format is file,sme,okay?,base,target,...
        (let ((start-pos 1))
          (dotimes (i 3)
            (multiple-value-bind (str next-start)
                (string-to-next-comma line start-pos)
              (declare (ignore str))
              (setq start-pos next-start)))
          (multiple-value-bind (field-value next-start)
              (string-to-next-comma line start-pos)
            (declare (ignore next-start))
            (push (read-from-string field-value) case-names)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding ubiquitous predicates
  
(defun ubiquitous-predicates-in-dehydration-experiment (path &optional (type "sme"))
  ;; The dehydrator produces minimal vocabularies, given the base and target of
  ;; an SME.  To reconstruct what the full set of ubiquitous predicates were for
  ;; an experiment requires either (a) digging through the source code, or (b)
  ;; assuming the union of the c(omparisons is representative, and taking the
  ;; union of all the ubiquitous predicates in the comparisons.  This procedure
  ;; implements (b).  
  (let ((preds nil))
    (dolist (file (directory 
                   (concatenate 'string path "*." type))
                  preds)
      (do-on-file-forms 
          (lambda (form)
            (when (and (listp form) 
                       (eq (car form) 'sme::defubiquitous-predicate))
              (pushnew (cadr form) preds :test 'equal)))
        file))))

(defun do-on-file-forms (procedure file)
  (let ((done? (cons file file)))
    (with-open-file (fin file :direction :input)
      (do ((line (read fin nil done?)
                 (read fin nil done?)))
          ((eq line done?))
        (funcall procedure line)))))

(defun files-mentioning-term (term path &optional (type "sme"))
    (let ((files nil))
    (dolist (file (directory 
                   (concatenate 'string path "*." type))
                  files)
      (do-on-file-forms 
          (lambda (form)
            (when (and (listp form) 
                       (contains-term? term form))
              (pushnew file files :test 'equal)))
        file))))

(defun contains-term? (term form)
  (cond ((null form) nil)
        ((equal term form) t)
        ((atom form) nil)
        (t (or (contains-term? term (car form))
               (contains-term? term (cdr form)))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Verification of dgroup consistency within an experiment

(defvar *dgroup-alist* nil)

(defun gather-dgroups-from-dehydrated-experiment (path &optional (extension ".sme"))
  (declare (special the-sme))
  (setq *dgroup-alist* nil)
  (dolist (file (directory (concatenate 'string path "*" extension)) *dgroup-alist*)
    (load file)
    (let ((the-base (sme::base the-sme))
          (the-target (sme::target the-sme)))
      (let ((entry (assoc (sme::name the-base) *dgroup-alist* :test 'equal)))
        (unless entry
          (push (setq entry (cons (sme::name the-base) nil)) *dgroup-alist*))
        (push the-base (cdr entry))
        (setq entry (assoc (sme::name the-target) *dgroup-alist* :test 'equal))
        (unless entry
          (push (setq entry (cons (sme::name the-target) nil)) *dgroup-alist*))
        (push the-target (cdr entry))))))

(defun find-dgroup-deltas (dgroup1 dgroup2)
  ;; Quick check
  (let ((facts1 (cdr (sme::lisp-form dgroup1)))
        (facts2 (cdr (sme::lisp-form dgroup2))))
    (list (set-difference facts1 facts2 :test 'equal)
          (set-difference facts2 facts1 :test 'equal))))

(defun find-dgroup-deltas-in-dgroup-list (dgroup-list)
  ;; Quick check
  (mapcar (lambda (other) (find-dgroup-deltas (car dgroup-list) other))
    (cdr dgroup-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tabulating data from dehydration results
;;;; The procedures in test-stand.lsp are aimed at gathering performance information
;;;; based on redoing the match.  This doesn't always make sense, e.g. in systems where
;;;; incremental matches are created.  The key information is already stored in the dehydration
;;;; results, so this procedure gathers up that information.

(defun gather-dehydrated-sme-statistics (path)
  (clear-sme-tabulation-data)
  (gather-statistics-for-dehydrated-data-set
   (lambda (file-name path)
     (gather-sme-global-statistics `((:file . ,file-name)
                                     (:path . ,path))))
   path))

(defvar *sme-tabulated-data* nil "Global statistics of an SME instance.")
(defvar *sme-mappings-tabulated* nil "Statistics for each mapping of an SME instance")

(defun clear-sme-tabulation-data ()
  (setq *sme-tabulated-data* nil)
  (setq *sme-mappings-tabulated* nil))

(defun gather-sme-global-statistics (starting-point)
  (declare (special the-results))
  ;; Uses the-results global
  (let* ((sme-specs (cdr (assoc :sme the-results)))
         (version (car sme-specs))
         (base-name (cadr (assoc :base the-results)))
         (target-name (cadr (assoc :target the-results))))
    (let* ((n-mhs (length (cadr (assoc :matches the-results))))
           (mappings (cadr (assoc :mappings the-results)))
           (n-mappings (length mappings)))
      (push `(,@ (copy-list starting-point)
                 (:version . ,version)
                 (:base . ,base-name)
                 (:target . ,target-name)
                 (:n-mhs . ,n-mhs)
                 (:n-mappings . ,n-mappings)) *sme-tabulated-data*)
      (dolist (m mappings)
        (let* ((id (cadr m))
               (id (if (listp id) (car id) id))
              (score (caddr m))
              (n-mhs (length (fourth m)))
              (n-inferences (length (sixth m))))
          (push `(,@ (copy-list starting-point)
                     (:base . ,base-name)
                     (:target . ,target-name)
                     (:id ,id)
                     (:score . ,score)
                     (:n-mhs . ,n-mhs)
                     (:n-inferences . ,n-inferences)) *sme-mappings-tabulated*))))))

(defparameter *sme-dehydrated-stats-template*
    '((:file "File" :string)
      (:path "Path" :string)
      (:base "Base" :other)
      (:target "Target" :other)
      (:n-mhs "#Mhs" :other)
      (:n-mappings "#Mps" :other))
  "Data gathered during dehydrated SME stats analysis.")

(defparameter *mappings-dehydrated-stats-template*
  '((:file "File" :string)
    (:path "Path" :string)
    (:base "Base" :other)
    (:target "Target" :other)
    (:id "ID" :other)
    (:score "NSim" :other)
    (:n-mhs "#Mhs" :other)
    (:n-inferences "#CIs" :other)))

(defun dump-dehydrated-sme-statistics (output-path root-file-name)
  (declare (special *sme-tabulated-data*
                    *sme-mappings-tabulated*))
  (alist-data->keyed-delimited-file *sme-tabulated-data*
                                    *sme-dehydrated-stats-template*
                                    (concatenate 'string output-path 
                                      root-file-name "_sme_cdf.txt"))
  (alist-data->keyed-delimited-file *sme-mappings-tabulated*
                                    *mappings-dehydrated-stats-template*
                                    (concatenate 'string output-path
                                      root-file-name "_Mps_cdf.txt")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; More debugging tools

(defun compare-sme-inputs (sme1 sme2)
    (format t "~%Base: ~A" 
    (set-exclusive-or (mapcar (lambda (entry)
                                (mapcar 'sme::lisp-form entry))
                        (sme::expressions (sme::base sme1)))
                      (mapcar (lambda (entry)
                                (mapcar 'sme::lisp-form entry))
                        (sme::expressions (sme::base sme2)))
                      :test 'equal))
  (format t "~%Target: ~A" 
    (set-exclusive-or (mapcar (lambda (entry)
                                (mapcar 'sme::lisp-form entry))
                        (sme::expressions (sme::target sme1)))
                      (mapcar (lambda (entry)
                                (mapcar 'sme::lisp-form entry))
                        (sme::expressions (sme::target sme2)))
                      :test 'equal))
  (format t"~%1 not 2: ~A"
    (set-difference (mapcar 'sme::lisp-form (sme::mappings sme1))
                    (mapcar 'sme::lisp-form (sme::mappings sme2))
                    :test 'sme::basically-same-mapping?))
    (format t"~%2 not 1: ~A"
    (set-difference (mapcar 'sme::lisp-form (sme::mappings sme2))
                    (mapcar 'sme::lisp-form (sme::mappings sme1))
                    :test 'sme::basically-same-mapping?)))

(defun compare-sme-parameters (sme1 sme2)
  (let ((params-seen nil)(diffs nil)
        (p1 (sme::lisp-form (sme::mapping-parameters sme1)))
        (p2 (sme::lisp-form (sme::mapping-parameters sme2))))
    (dolist (entry p1)
      (let ((param (car entry))
            (value (cadr entry)))
        (let ((other-entry (assoc param p2)))
          (cond ((null other-entry)
                 (push (cons param :1-not-2) diffs))
                (t (let ((other-value (cadr other-entry)))
                     (unless (equal value other-value)
                       (push (list param value other-value) diffs)))))
          (push param params-seen))))
    (dolist (entry p2)
      (let ((param (car entry)))
        (unless (member param params-seen)
          (push (cons param :2-not-1) diffs))))
    diffs))


(defun compare-sme-mhs (sme1 sme2)
  (sme::compare-sme-mh-results (sme::lisp-form sme1) (sme::lisp-form sme2)))

(defun compare-sme-mappings (sme1 sme2)
  (sme::compare-sme-mapping-results (sme::lisp-form sme1) (sme::lisp-form sme2)))
  

;;;; ---------------------------------------------------------------------------
;;;; End of Code