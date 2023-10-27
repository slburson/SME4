;;; -*- Mode: Lisp; Package: QRG -*-

;;; Most of the contents of 'qrgsetup.lsp' was the build system, which in this fork has been
;;; replaced with ASDF.  But there are a few utilities in that file also which are used by SME.
;;; These have been moved here.

(in-package :qrg)


;;; DEFINE SOME USEFUL GLOBAL PARAMETERS AND FUNCTIONS
;;; ----------------------------------------------------------------------------
(defun fast-compile ()
   (proclaim '(optimize (speed 3) (safety 1) (space 0))))

(defun safe-compile ()
   (proclaim '(optimize (speed 0) (safety 3) (space 3))))


;;; There was code in 'qrgsetup.lsp' to set this to the directory of the file being loaded.
;;; Unfortunately, that is a less useful thing to do when using ASDF, because the fasls are
;;; elsewhere, under ~/.cache/; not kept in the source directory.  For the moment, we force
;;; it to be set manually.
(defvar *qrg-path* :unbound "Path stub to which all systems add their pathnames")
(defvar *data-path* :unbound)
(defvar *path-sepchar* "/" "Path separation character (system-dependent)")

(defun set-qrg-path (new-path)
   "Changes the value of qrg::*qrg-path*."
   (declare (type string new-path))
   (setq *qrg-path* new-path)
   (setq *data-path* *qrg-path*)) ; ???

(defun make-qrg-path (&rest strings)
  (apply #'append-qrg-path *qrg-path* strings))

(defun make-data-path (&rest strings)
   (apply #'append-qrg-path *data-path* strings))

(defun append-qrg-path (qrg-path &rest strings)
   "Appends path info to a path created via make-qrg-path."
   (reduce #'(lambda (str1 str2)
               (concatenate 'string str1 str2 *path-sepchar*))
     strings :initial-value qrg-path))

(defun make-qrg-file-name (path fname)
  (make-full-file-spec path fname ""))

(defun make-full-file-spec (path file ext)
  "Ensures that a path separator character appears between path and file"
   (let ((last-path-sepchar (position (aref *path-sepchar* 0) path :from-end t)))
      (cond ((and last-path-sepchar
                  (= last-path-sepchar (1- (length path)))) ;; Ends with path separator?
             (concatenate 'string path file ext))
            (t (concatenate 'string path *path-sepchar* file ext)))))


;;; MISCELLANEOUS UTILITY FUNCTIONS
;;; ----------------------------------------------------------------------------

(defmacro runtime-call (fn-name pkg &rest args)
  ;;Enable files to load without generating package-not-found errors
  `(funcall (intern ,fn-name (find-package ,pkg)) ,@args))


;;; ERROR-HANDLING
;;; ----------------------------------------------------------------------------

(defparameter *trap-errors* t "Controls whether or not errors are actually trapped.")

(defmacro trap-error ((error-type &rest trap-forms) &body body)
  "Executes trap-forms whenever an error of error-type occurs in body."
  (let ((tag (gensym "error-handling-block")))
    `(if ,'*trap-errors*
       (block ,tag
         (handler-bind ((,error-type
                         #'(lambda (condition)
                             (declare (ignorable condition))
                             ;; When printing the error, the format call below
                             ;; should use ~A, not ~S since we want this to 
                             ;; be readable by humans.
                             #+(or :runtime-system :print-trapped-errors)
                             (format t "~%qrg:trap-error: ~A" condition)
                             (return-from ,tag 
                               (progn ,@trap-forms)))))
           ,@body))
       (block ,tag
         ,@body))))  ; hope expansion doesn't cause side-effects

