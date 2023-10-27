;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: alist-template-data.lsp
;;;;    System: SME v4
;;;;    Author: Ken Forbus
;;;;   Created: October 26, 2007 10:10:28
;;;;   Purpose: Manage alist data, including read/write from delimited files
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2014-08-27 19:39:55 -0500 (Wed, 27 Aug 2014) $
;;;;  $LastChangedBy: forbus $
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

;;;; In low-level data gathering, alists are often the representation of choice because
;;;; there are often a few to a dozen analyses one wishes to do, with the same analysis
;;;; routines and without recompiling all the time.  One also wants to crunch such data
;;;; via spreadsheets.  These routines provide a general facility for doing this.

;;;; An alist template is itself an alist, with entries consisting of (<key> <label> <type>).
;;;; <key> is the alist entry key, typically in the keyword package (but this is not enforced).
;;;; <label> is a string that will be used for column headers in spreadsheets.
;;;; <type> is :string or anything else, where :string means, don't read this.
;;;; Important: Label must never contain the character used for delimiting.
;;;; The order of entries in the alist template determines their order in the spreadsheet columns.

(defvar *delimit-char* #\,)

(defparameter *test-alist-template* '((:foo "Foo" :*)(:bar "Bar" :*)(:mumble "Mumble" :*)
                                      (:grumble "Grumble" :string)))
(defparameter *test-alist-data* '(((:foo . 1)(:bar . 2)(:mumble . 3)
                                   (:grumble . "This is a test"))
                                  ((:foo . (34 12 1 1 1 1))(:bar . 3.1415)(:mumble . 2.718)
                                   (:grumble . "c:\\autoexec.bat -- remember me?"))))

;;;; API 
;;;; (alist-data->keyed-delimited-file <data> <alist template> <file-name>)
;;;; N.B. This dumps the keys as well as the pretty strings, in the row before the header row.
;;;;      This allows us to reload a data file without having the template at hand.
;;;; (keyed-delimited-file->alist-data <file-name>)
;;;; Returns two values, the list of alist data entries, and the alist template.
     
(defun alist-data->keyed-delimited-file (data alist-template file-name)
  (with-open-file (fout file-name :direction :output
                    :if-exists :supersede)
    (let ((*print-length* 1000)
          (*print-level* 100)
          (*print-right-margin* 100000))
      (print-alist-template-info alist-template fout)
      (dolist (datum data)
        (print-alist-delimited-row datum alist-template fout)))))

(defun print-alist-template-info (alist-template
                                  &optional (stream *standard-output*))
  (dolist (entry alist-template)
    (format stream "~S" (car entry)) (princ *delimit-char* stream))
  (terpri stream)
  (dolist (entry alist-template)
    (format stream "~S" (third entry))(princ *delimit-char* stream))
  (terpri stream)
  (dolist (entry alist-template)
    (format stream "~A" (cadr entry)) (princ *delimit-char* stream)))
      
(defun print-alist-delimited-row (datum alist-template
                                   &optional (stream *standard-output*))
  (terpri stream)
  (dolist (template-entry alist-template)
    (let ((type (third template-entry))
          (key (car template-entry)))
      (format stream (if (eq type :string) "~A"
                       "~S")
        (cdr (assoc key datum)))
      (princ *delimit-char* stream))))
      
(defun keyed-delimited-file->alist-data (file-name)
  (with-open-file (fin file-name :direction :input)
    (let ((template-keys (extract-alist-keys-from-delimited-string 
                          (read-line fin)))
          (template-types (extract-alist-keys-from-delimited-string
                           (read-line fin)))
          (template-headers (extract-headers-from-delimited-string
                             (read-line fin))))
      (cond ((not (= (length template-keys)
                    (length template-headers)))
             (error "Mismatch in keys (~D) and headers (~D): ~A, ~A."
               (length template-keys) (length template-headers)
               template-keys template-headers))
        (t (let ((template (mapcar 'list template-keys template-headers template-types)))
             (do ((line (read-line fin nil template)
                    (read-line fin nil template))
                  (n-items (length template))
                  (data-from-line nil)
                  (data nil))
                 ((equal line template)
                  (values (nreverse data) template))
               (setq data-from-line (extract-data-from-delimited-string line template))
               (when (= (length data-from-line) n-items)
                 (push (mapcar (lambda (template-entry value)
                                 (cons (car template-entry) value))
                         template data-from-line)
                   data)))))))))

(defun extract-alist-keys-from-delimited-string (string)
  (mapcar (lambda (str)
            (read-from-string str nil str))
    (crack-delimited-string string)))

(defun extract-headers-from-delimited-string (string)
  (crack-delimited-string string))

(defun extract-data-from-delimited-string (string template)
  (mapcar (lambda (str template-entry)
            (if (eq (third template-entry) :string) str
              (read-from-string str nil str)))
    (crack-delimited-string string)
    template))

(defun crack-delimited-string (string &optional (delimit-char *delimit-char*))
  ;; Returns a list of substrings, broken out via delimit-char
  (do ((i 0 (1+ i))
       (answers nil)
       (end (length string))
       (char 0)
       (start 0))
      ((= i end)
       (unless (= start end)
         (push (subseq string start end) answers))
       (nreverse answers))
    (setq char (elt string i))
    (cond ((eq char delimit-char) ;; Got a range
           (push (subseq string start i) answers)
           (setq start (1+ i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of Code