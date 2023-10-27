;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: qrgsetup.lsp
;;;;    System: All QRG systems
;;;;   Version: 1.14
;;;;    Author: John Everett
;;;;   Created: Feb 22, 1995
;;;;   Purpose: To initialize the default Lisp environment for QRG systems
;;;; ---------------------------------------------------------------------------
;;;;  $LastChangedDate: 2015-10-22 14:53:26 -0500 (Thu, 22 Oct 2015) $
;;;;  $LastChangedBy: usher $
;;;; ---------------------------------------------------------------------------


(in-package :cl-user)


;;; ADD TO FEATURES LIST
;;; ----------------------------------------------------------------------------

(pushnew :qrg *features*)


;;; SETUP A QRG PACKAGE THAT CONTAINS UTILITIES SHARED BY ALL QRG SYSTEMS
;;; ----------------------------------------------------------------------------
;;; The QRG package provides a little bundle of utilities that all QRG systems
;;; should have access to, so whenever you create a new package for a QRG
;;; system, be sure that it uses this package.  The Common-LISP USER package
;;; uses this package so that its small suite of exported functions (load-file,
;;; load-files, and make-qrg-path) are transparently available in CL-USER (the
;;; default package on startup).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage :qrg (:use :common-lisp))
  (use-package (find-package :qrg) (find-package :cl-user)))

;;; SETUP THE DATA PACKAGE SO THAT IT'S AVAILABLE TO ALL QRG SYSTEMS
;;; ----------------------------------------------------------------------------
;;; Provide the ability to move data to a separate package in the future.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (and (find-package :data) (find-package :d))
    (rename-package (find-package :cl-user)
                    :common-lisp-user  ; keep name the same
                    (append (package-nicknames (find-package :cl-user))
                            '(:data :d)))))


;;; DEFINE SOME USEFUL GLOBAL PARAMETERS AND FUNCTIONS IN CL-USER
;;; ----------------------------------------------------------------------------
(defun fast-compile ()
   (proclaim '(optimize (speed 3) (safety 1) (space 0))))

(defun safe-compile ()
   (proclaim '(optimize (speed 0) (safety 3) (space 3))))


(in-package :qrg)  ;;<<<SWITCH TO QRG PACKAGE AT THIS POINT


;;; GLOBAL VARIABLES
;;; ----------------------------------------------------------------------------
;;; These global variables are available in the common-lisp-user package.
;;; Rebinding them will alter the behavior the loading functions.  For example,
;;; rebinding the *qrg-path* variable will cause the loading functions to look
;;; elsewhere for QRG files.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *qrg-path*     nil "Path stub to which all systems add their pathnames")
  (defvar *data-path*   nil)
  (defvar *path-sepchar* nil "Path separation character (system-dependent)")
  (defvar *src-ext*      nil "Source extension for files")
  (defvar *src-exts*     nil "List of possible source extensions")
  (defvar *bin-ext*      nil "Binary or fasload extension for files")
  (defvar *bin-exts*     nil "List of possible binary extensions")
  (defvar *current-load-path* nil "Dynamically bound to make path available in 
                                 environment during load of system")
  (defvar *modules-loaded* nil "List of modules that will only get reloaded when 
                                we call the load routine with force-load t")
  (defvar *modules-loading* nil "List of modules currently being loaded 
                                (to prevent infinite recursion)")
  (defvar *defsys-loaded* nil "List of defsys files that have been loaded")
  (defvar *not-interactive?* t 
    "Switch to bypass interactive error handling methods"))


;;; USER-SPECIFIC CONFIGURATION SECTION
;;; ============================================================================
;;; You must ensure that the following numbered items are set correctly for your
;;; particular hardware and software.


;1. PLATFORM-SPECIFIC SETTINGS -------------------------------------------------

;2. SETTING THE LOCAL QRG PATH STUB --------------------------------------------
;;; This path stub must include a terminal path separation character.

;;; This should be done in your startup.lsp, so that different locations work
;;; without changing this file!

      
;;; END OF USER-SPECIFIC CONFIGURATION
;;; ============================================================================
      

;;; HARDWARE-SPECIFIC SETTINGS 
;;; ----------------------------------------------------------------------------
;;; You should not change the following settings, except to add new cases.

(setq *path-sepchar*
      #+mac ":"
      #+unix "/"
      #+microsoft "\\" ;; the first backslash escapes
      )

(setq *src-ext* ".lsp")
(setq *src-exts* (list *src-ext*))
(setq *bin-ext* ".fasl")  ; for unix and windows
(setq *bin-exts* (list *bin-ext*))

;;; THE QRG LOADING FACILITY
;;; ----------------------------------------------------------------------------
;;; See qrg-system-spec.text (located in *qrg-path*) for a detailed discussion
;;; of this loading facility.  The following are notes on specific aspects of
;;; the code:
;;; 1. :COMPILE-IF-NEWER results in an error (LUCID/RS6000) if the file
;;; contains, in this order, (1) a macro, (2) defun forms defining functions
;;; called by the macro, and (3) a top-level call to the macro, apparently
;;; because the compilation process does not incrementally load the forms of the
;;; file, so the first defun is undefined at the time that the compiler hits the
;;; top-level call.  A solution to this would be to have the :COMPILE-IF-NEWER
;;; first load the source, then compile and load the binary, but this would be
;;; slower.  Another solution would be to ensure that toplevel calls occur only
;;; in their own files, separate from their defining functions.  As of 2/25/95,
;;; going with this second "solution," but will implement the first if demand
;;; warrants.

(declaim (special *action* *verbose*))
(defvar *action* :compile-if-newer "The current default for load-file")
(defvar *verbose* t "Default value of the :verbose keyword to load-file")
;;; This should be let-bound by list-system-files:
(defvar *system-files* nil "Source files that would be loaded.")

(defmacro with-compiler-chatter (messages? &rest body)
  "implementation-specific means of controlling compiler messaging"
  `(let ((lisp:*compile-verbose* ,messages?)
         )
     ,@body))


(defun load-files (path file-list &rest keys
                   &key (action *action*) (verbose *verbose*)
                   (outpath path) (src-ext *src-ext*) (bin-ext *bin-ext*)
                   (src-exts *src-exts*) (bin-exts *bin-exts*)
                        &allow-other-keys)
  (declare (ignorable verbose outpath src-ext bin-ext src-exts bin-exts))
  (let ((*action* action))
    (ecase action
      ((:compile :compile-if-newer :source-if-newer :load-source :load-binary :compile-and-load :list)
       (dolist (file file-list)
         (apply #'load-file path file keys)
         #+common-graphics
         (cg:process-pending-events)
         ))
      ((:load-compile :load-and-compile)
       (apply #'load-files path file-list 
              `(:action :load-source ,@keys))
       (apply #'load-files path file-list 
              `(:action :compile ,@keys))))))

(defun load-file (path file &rest keys
                  &key (action *action*) (verbose *verbose*)
                  (outpath path) (src-ext *src-ext*) (bin-ext *bin-ext*)
                  (src-exts *src-exts*) (bin-exts *bin-exts*)
                       &allow-other-keys)
  (declare (ignorable verbose outpath src-ext bin-ext src-exts bin-exts))
  (let ((*action* action)
        (*current-load-path* 
         (directory-namestring 
          (pathname (make-full-file-spec path file "")))))
    (apply 'load-file-aux path file keys)))

(defun load-file-aux (path file
                      &key (action *action*) (verbose *verbose*)
                      (outpath path)
                      (src-ext *src-ext* src-ext?) 
                      (bin-ext *bin-ext* bin-ext?)
                      (src-exts *src-exts*) 
                      (bin-exts *bin-exts*))
  (unless outpath (setq outpath path))
  (let ((src nil)
        (bin nil)
        (src-exists? nil) 
        (bin-exists? nil)
        (*load-verbose* verbose))  ; gag indirectly loaded files
    (with-compiler-chatter verbose
      ;; find source file
      (unless (eq action :load-binary)
        (when src-ext? (setq src-exts (list src-ext)))
        (do ((exts src-exts (cdr exts)))
            ((or (null exts) src-exists?))
          (setq src (make-full-file-spec path file (car exts)))
          (setq src-exists? (probe-file src)))
        (unless src-exists? 
          (setq src (make-full-file-spec path file (car src-exts)))))
      ;; find binary file
      (unless (member action '(:load-source :list :load-and-list))
        (when bin-ext? (setq bin-exts (list bin-ext)))
        (do ((exts bin-exts (cdr exts)))
            ((or (null exts) bin-exists?))
          (setq bin (make-full-file-spec outpath file (car exts)))
          (setq bin-exists? (probe-file bin)))
        (unless bin-exists?
          (setq bin (make-full-file-spec outpath file (car bin-exts)))))
      ;; check for error
      (unless (or src-exists? bin-exists?)
        (cond (qrg::*not-interactive?*
               (warn "Can't find ~S or ~S." src bin file)
               (return-from load-file-aux nil))
              ((y-or-n-p "~%Can't find ~A~%or ~A.~%Skip ~A?~%" src bin file)
               (return-from load-file-aux nil))
              (t
               (error "~%Can't find ~A or ~A~%" src bin))))
      ;; do the action
      (ecase action
        (:compile-if-newer
         (cond ((and src-exists? bin-exists?)
                (cond ((>= (file-write-date bin)
                           (file-write-date src))
                       (load bin :verbose verbose))
                      (t (compile-file src :output-file bin)
                         (load bin :verbose verbose))))
               (bin-exists? (load bin :verbose verbose))
               (src-exists?
                (compile-file src :output-file bin)
                (load bin :verbose verbose))))
        (:source-if-newer
         (cond ((and src-exists? bin-exists?)
                (cond ((>= (file-write-date bin) (file-write-date src))
                       (load bin :verbose verbose))
                      (t (load src :verbose verbose))))
               (bin-exists? (load bin :verbose verbose))
               (src-exists? (load src :verbose verbose))))
        (:compile (compile-file src :output-file bin))
        (:load-compile 
         (load src :verbose verbose)
         (compile-file src :output-file bin))
        (:compile-and-load
         ; (load src :verbose verbose)
         (compile-file src :output-file bin)
         (load bin :verbose verbose))
        (:load-source (load src :verbose verbose))
        (:load-binary (load bin :verbose verbose))
        (:list (register-file src))
        (:load-and-list  ; special case for recursively loaded defsys files
         (register-file src)
         (let ((*action* :list))
           (load src :verbose verbose)))
        ))))

(defun make-full-file-spec (path file ext)
  "Ensures that a path separator character appears between path and file"
   (let ((last-path-sepchar (position (aref *path-sepchar* 0) path :from-end t)))
      (cond ((and last-path-sepchar
                  (= last-path-sepchar (1- (length path)))) ;; Ends with path separator?
             (concatenate 'string path file ext))
            (t (concatenate 'string path *path-sepchar* file ext)))))

(defun make-qrg-source-file-name (path fname)
  (make-full-file-spec path fname *src-ext*))

(defun make-qrg-binary-file-name (path fname)
  (make-full-file-spec path fname *bin-ext*))

(defun make-qrg-file-name (path fname)
  (make-full-file-spec path fname ""))

(defun make-data-path (&rest strings)
   (apply #'append-qrg-path *qrg-path* strings))

(defun make-qrg-path (&rest strings)
  (apply #'append-qrg-path *qrg-path* strings))

(defun append-qrg-path (qrg-path &rest strings)
   "Appends path info to a path created via make-qrg-path."
   (reduce #'(lambda (str1 str2)
               (concatenate 'string str1 str2 *path-sepchar*))
     strings :initial-value qrg-path))

(defun load-qrg-defsys (&rest strings)
  "Load the defsys file from the path indicated by the set of strings.
   If the strings do not already include the QRG base path, it is
   prepended to the path."
  (apply 'load-qrg-defsys-ex "defsys" strings))

(defun load-qrg-defsys-ex (filename &rest strings)
  "Load the defsys file from the path indicated by the set of strings.
   If the strings do not already include the QRG base path, it is
   prepended to the path.  Filename should be the name of the defsys file to be
   loaded (do not include extension).  Normally, the filename is defsys, in which
   case you can use the preferred function load-qrg-defsys.  Load-qrg-defsys-ex
   is for those cases where the defsys file needs to be called something else."
  (let* ((defsys-path (defsys-path strings))
         (defsys-token (concatenate 'string defsys-path filename))
         (*action* (if (eq *action* :list) :load-and-list :load-source)))
    (unless (member defsys-token *defsys-loaded* :test #'string=)
;      (when (eq *action* :list)
;        (register-file (concatenate 'string defsys-token *src-ext*)))
      (multiple-value-bind (load-ok? new-defsys)
          (let ((*defsys-loaded* (cons defsys-token *defsys-loaded*)))
            (values 
             (load-file defsys-path filename :action *action*)
             *defsys-loaded*))
        (when load-ok?
          (setq *defsys-loaded* (union *defsys-loaded* new-defsys)))
        :done))))

(defun defsys-path (strings)
  (if (and (>= (length (car strings)) (length *qrg-path*))
           #+:mswindows
           (string-equal (car strings) *qrg-path* :end1 (length *qrg-path*))
           #-:mswindows
           (string= (car strings) *qrg-path* :end1 (length *qrg-path*)))
    (apply 'append-qrg-path strings)
    (apply 'make-qrg-path strings)))

(defun set-data-path (new-path)
  (declare (type string new-path))
  (setq *data-path* new-path))

(defun get-data-path ()
  (or *data-path* *qrg-path*))

(defun set-qrg-path (new-path)
   "Changes the value of qrg::*qrg-path*."
   (declare (type string new-path))
   (setq *qrg-path* new-path))

(defun get-qrg-path ()
   "Returns the value of qrg::*qrg-path*."
   *qrg-path*)

(defmacro with-fresh-load (&rest body)
  "Locally resets *modules-loaded* and *defsys-loaded* so that a system wide 
   reload/recompile can occur"
  `(multiple-value-bind (new-modules new-defsys)
       (let ((*modules-loaded* nil)
             (*defsys-loaded* nil))
         ,@body
         (values *modules-loaded* *defsys-loaded*))
     (setq *modules-loaded* (union *modules-loaded* new-modules))
     (setq *defsys-loaded* (union *defsys-loaded* new-defsys))))


(defun module-loaded? (module-name)
  (member module-name *modules-loaded*))  

(defun record-module-loaded (module-name)
  (pushnew module-name *modules-loaded*))

;;; Redefined as a function, so it can be applied:
(defun require-system (system-path loader-func package &rest init-keyword-pairs
                                      &key (force-load nil)
                                      &allow-other-keys)
  (unless (find-package package)
    (load-qrg-defsys system-path))
  (when (or force-load (not (module-loaded? package)))
    (apply (intern loader-func package) init-keyword-pairs)))

;;; Streamline the loading process a bit, and disentangle package issues:
(defun require-module (path module 
                            &rest keys
                            &key (force-load nil) (verbose *verbose*)
                            action
                            &allow-other-keys)
  (when (or force-load
            (sys-handles-own-module-loaded-check? module)
            (not (module-loaded? module)))
    (let ((*verbose* verbose))
      (load-qrg-defsys path)
      (when (eq action :list)
        ;; List the export file automatically (only works if named export.lsp
        ;; or exports.lsp):
        (register-export-file path)
        ;; Then list any auxiliary files:
        (dolist (f (auxiliary-files module))
          (pushnew f *system-files* :test #'equal)))
      (apply 'load-sys module keys))))

(defun list-system-files (path module &rest keys)
  (let* ((*system-files* (list "qrgsetup.lsp"))
         (*action* :list)
         (*verbose* nil)
         (*modules-loaded* nil)
         (*defsys-loaded* nil))
    (apply #'require-module path module :action :list :verbose nil keys)
    *system-files*))

(defun load-all-defsys-files (path module-name)
  "Loads all the defsys files used by the specified system and all systems upon
   which it is dependent."
  ;; Note that currently this is just a wrapper for list-system-files since that
  ;; causes the defsys files to be loaded as a side-effect.  This might change in
  ;; the future, so I've abstraced this functionality.  [Usher]
  (list-system-files path module-name))

(defun register-file (src)
  (let* ((len (length *qrg-path*))
         (trimmed (if (and (> len 0)
                           (< len (length src))
                           (string= *qrg-path* src :end2 len))
                    (subseq src len)
                    src)))
    (pushnew trimmed *system-files*)))


(defparameter *export-filenames* '("export" "exports"))

(defun register-export-file (path)
  (let ((found-files nil))
    (dolist (filename *export-filenames*)
      (let ((export-path (make-full-file-spec 
                          (namestring (aux-path->full-path path)
;;;                                    :syntax :unix
                                      )
                          filename ".lsp")))
        (when (probe-file export-path)
          (push export-path found-files)
          (register-file export-path))))
    found-files))
    

;;; This is really annoying: excl defines compile-system and load-system such 
;;; that they conflict with symbols in the qrg package.  Hence the abbreviation.

(defvar *compile-sys-compile-action* :compile-and-load)

(defgeneric compile-sys (arg &rest args))

(defmethod compile-sys ((path pathname) &rest args)
  (let ((module (first args))
        (keys (rest args)))
    (handler-bind ((warning #'(lambda (condition)
                                (if (muffle-compile-sys-load-warning? condition)
                                  (muffle-warning condition)
                                  (continue condition)))))
      (apply 'require-module path module :action :load-source keys))
    (with-fresh-load
        (apply 'load-sys module :action *compile-sys-compile-action* keys))
    module))

(defmethod compile-sys ((path string) &rest args)
  (let ((module (first args))
        (keys (rest args)))
    (handler-bind ((warning #'(lambda (condition)
                                (if (muffle-compile-sys-load-warning? condition)
                                  (muffle-warning condition)
                                  (continue condition)))))
      (apply 'require-module path module :action :load-source keys))
    (with-fresh-load
        (apply 'load-sys module :action *compile-sys-compile-action* keys))
    module))

(defmethod compile-sys ((module symbol) &rest keys)
  (handler-bind ((warning #'(lambda (condition)
                              (if (muffle-compile-sys-load-warning? condition)
                                (muffle-warning condition)
                                (continue condition)))))
    (apply 'load-sys module :action :load-source keys))
  (with-fresh-load
      (apply 'load-sys module :action *compile-sys-compile-action* keys))
  module)

(defun muffle-compile-sys-load-warning? (condition)
  (let ((format-string (simple-condition-format-control condition)))
    (search "fobject not stack allocated in interpreted environment" 
            format-string)))



(defgeneric load-sys (system &rest keys)
  (:documentation "A generic load method.  
Each system-defining defsys should specialize this method on a keyword symbol 
denoting the module name."))

;;; The fallback errors out:
(defmethod load-sys ((system t) &rest keys)
  (cond ((keywordp system)
         (error "System is not defined: ~s" system))
        ((or (stringp system) (pathnamep system))
         (error "Load-sys takes a keyword system name.  Try calling require-module instead."))
        (t
         (error "Unknown argument to load-sys: ~s.  Expected a keyword." system))))

(defmethod load-sys :around ((system symbol) &rest keys &key (force-load nil) 
                             &allow-other-keys)
  "Handles the maintenance of *modules-loaded* transparently"
  (when (or force-load
            (sys-handles-own-module-loaded-check? system)
            (not (module-loaded? system)))
    (unless (member system *modules-loading*)
      (unwind-protect 
          (progn 
            (push system *modules-loading*)
            (call-next-method)
            ;; if system was not defined, error will throw past this:
            (record-module-loaded system))
        (setf *modules-loading* (delete system *modules-loading*)))
      :done)))


(defgeneric sys-handles-own-module-loaded-check? (system)
  (:documentation "A few of our systems have complex enough loading that the
                   simple module-loaded? check is not sufficient.  For those
                   systems, this method returns non-nil."))

(defmethod sys-handles-own-module-loaded-check? (system)
  (declare (ignore system))
  nil)


(defgeneric usage (system)
  (:documentation "Prints system usage information to *standard-output*"))

;;; No penalty for not defining usage:
(defmethod usage ((system t))
  (values))


;;; AUXILIARY FILES
;;; ----------------------------------------------------------------------------

(defgeneric auxiliary-files (system)
  (:documentation "Lists runtime flatfiles associated with system."))

;;; flatfiles are optional:
(defmethod auxiliary-files ((system t)) nil)


(defun aux-file-path (full-path)
  "Given a full path to a file or directory, converts it into the kind of
   path that is suitable for auxiliary-files."
  (string-right-trim 
   "/\\"
   (trim-string-left
    (namestring full-path :syntax :unix)
    (namestring (get-qrg-path) :syntax :unix))))

(defun trim-string-left (str prefix)
  (let ((len (length prefix)))
    (if (and (> len 0)
             (< len (length str))
             (string-equal prefix str :end2 len))
      (subseq str len)
      str)))


(defparameter *default-excluded-types* 
  '("fasl" "fsl" "scc" "ini"))

(defun dir->aux-file-paths (full-dir-path 
                            &key (excluded-types *default-excluded-types*)
                                 (recurse? nil))
  "Given a path to a directory, returns a list of paths of all the files
   in that directory except those excluded via excluded-types.  Does not
   recurse through sub-directories."
  (let ((paths nil))
    (dolist (path (directory full-dir-path :directories-are-files nil))
      (cond ((directory? path)
             (let ((subdir (car (last (pathname-directory path)))))
               (when (and recurse?
                          (stringp subdir)
                          (string/= subdir ".svn"))
                 (setq paths
                   (append (dir->aux-file-paths path
                             :excluded-types excluded-types
                             :recurse? recurse?)
                     paths)))))
            ((member (pathname-type path) excluded-types :test #'equalp)
             nil)
            (t
             (push (aux-file-path path) paths))))
    paths))

(defun directory? (pathname)
  "Returns non-nil iff the specified path is a directory."
  (and (null (pathname-name pathname))
       (null (pathname-type pathname))))


(defun aux-path->full-path (path)
  (merge-pathnames path (get-qrg-path)))



(defun aux-file (dir file ext)
  "A handy wrapper that can improve the readability of your auxiliary-files
   methods"
  (aux-file-path (make-full-file-spec dir file ext)))

(defun aux-dir (dir-path &key (excluded-types *default-excluded-types*)
                              (recurse? nil))
  "A handy wrapper that can improve the readability of your auxiliary-files
   methods"
  (dir->aux-file-paths dir-path 
                       :excluded-types excluded-types
                       :recurse? recurse?))



;;; GET-PATH
;;; ----------------------------------------------------------------------------

#+(or :common-graphics (not :runtime-system))
(defun get-path (&key (root-path qrg::*qrg-path*)
                      (subdirs nil)
                      (filename nil)
                      (filetype nil))
  (let ((base-path #+runtime-system (pathname (cg::path (cg::app cg:*system*)))
                   #-runtime-system (pathname root-path)))
    (make-pathname
     :name filename
     :type filetype
     :host (pathname-host base-path)
     :device (pathname-device base-path)
     :directory (typecase subdirs
                  (null (pathname-directory base-path))
                  (string (append (pathname-directory base-path) 
                                  (list subdirs)))
                  (cons (append (pathname-directory base-path) subdirs))
                  (otherwise (pathname-directory base-path))))))



(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(get-path)))



;;; MISCELLANEOUS UTILITY FUNCTIONS
;;; ----------------------------------------------------------------------------

(defmacro runtime-call (fn-name pkg &rest args)
  ;;Enable files to load without generating package-not-found errors
  `(funcall (intern ,fn-name (find-package ,pkg)) ,@args))

(defmacro runtime-val (var-name pkg)
  ;;Enable using a variable inside runtime-call without generating errors
  `(symbol-value (find-symbol ,var-name ,pkg)))

(defmacro setq-runtime-val (var-name pkg new-val)
  ;;Enable setting a variable inside runtime-call without generating errors
  `(setf (symbol-value (find-symbol ,var-name ,pkg)) ,new-val))


(defparameter *cons-bytes* #+:64bit 16 #-:64bit 8)

#+:allegro
(defun get-heap-info ()
  "Returns two values:
   (1) The number of bytes currently used in the Lisp heap.
   (2) The total size of the heap in bytes."
  ;; See <qrg>/utils/heap-info.lsp for more thorough documentation than could
  ;; nicely fit into qrgsetup.
  (let* ((map (sys::gsgc-map t))
         (newspace-bytes-used (+ (* (aref map 3) *cons-bytes*) (aref map 9)))
         (n-oldspaces (/ (- (length map) 20) 10))
         (oldspace-bytes-used 0))
    (do ((j 0 (1+ j)))
        ((>= j n-oldspaces))
      ;; conses:
      (incf oldspace-bytes-used
            (* (aref map (+ 20 (* j 10) 3)) *cons-bytes*))
      ;; other bytes:
      (incf oldspace-bytes-used
            (aref map (+ 20 (* j 10) 9))))
    (values
     (+ newspace-bytes-used oldspace-bytes-used)
     (- (aref map 6) (aref map 7)))))

#+:allegro
(defun print-heap-info (&key (stream *standard-output*))
  (multiple-value-bind (heap-used total-heap)
      (get-heap-info)
    (format stream "~%;; Heap: ~aMB/~aMB"
      (string-trim " ." (format nil "~4f" (/ heap-used #.(expt 2 20))))
      (string-trim " ." (format nil "~4f" (/ total-heap #.(expt 2 20)))))))

#+:allegro
(defmacro str+ (&rest strings-or-objs)
  "Shorthand for concatenating strings.  It actually just dispatches to Franz
   Allegro CL's util.string:string+ function, so it can handle objects other 
   than strings if those objects have an applicable princ-to-string method."
  `(util.string:string+ ,@strings-or-objs))

#+:allegro
(defmacro sym+ (&rest strings-or-objs)
    "Shorthand for concatenating symbols  It actually just dispatches to Franz
   Allegro CL's util.string:string+ function, so it can handle objects other 
   than strings if those objects have an applicable princ-to-string method.
   interns it in the current package"
  `(intern (util.string:string+ ,@strings-or-objs)))

#+:allegro
;;; A useful datastructure to store one-to-many value (alist containing ((key1 (val1 val2))(key2 (val1))..)
(defun add-to-assoc-bucket (alist key value &key (test 'eql))
  "adds value to the bucket corresponding to the key in the alist"
  (let* ((current-value (cdr (assoc key alist :test test)))
         (current-value (copy-list current-value)))
    (if current-value
      (rplacd (assoc key alist :test test) (list (push value (car current-value))))
      (push (list key (list value)) alist)))
  alist)

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


(defmacro export-symbols (subpackage-name doc-string symbols)
  "Assigns a name and documentation to the group of symbols and exports them
   from the current package."
  `(progn 
     (export ',(extract-syms-for-export symbols))))

(defun extract-syms-for-export (symbols)
  (mapcar #'(lambda (s)
              (if (consp s)
                (first s)
                s))
    (if (eq (first symbols) 'quote)
      (second symbols)
      symbols)))



;;; EXPORT QRG SYMBOLS
;;; ----------------------------------------------------------------------------
;;; These are the public QRG symbols that all QRG systems should have access to.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(*path-sepchar*
            *current-load-path*
            *src-ext*
            *bin-ext*
            *modules-loaded*
            *defsys-loaded*
            module-loaded?
            record-module-loaded
            load-file
            load-files
            make-data-path
            make-qrg-path
            make-qrg-file-name
            append-qrg-path
            make-full-file-spec
            load-qrg-defsys
            load-qrg-defsys-ex
            set-data-path
            get-data-path
            set-qrg-path
            get-qrg-path
            with-fresh-load
            runtime-call
            runtime-val
            require-system
            require-module
            list-system-files
            load-all-defsys-files
            update-flatfiles
            
            auxiliary-files
            aux-file-path
            dir->aux-file-paths
            aux-path->full-path
            aux-file
            aux-dir
            
            load-sys
            compile-sys
            *compile-sys-compile-action*
            sys-handles-own-module-loaded-check?
            usage
            trap-error
            *trap-errors*
            export-symbols
            get-heap-info
            print-heap-info
            str+
            sym+
            foreign-lib-loaded?
            load-foreign-library)))



;;; DEFAULT *QRG-PATH* TO BE THE PATH FROM WHICH THIS FILE WAS LOADED
;;; ----------------------------------------------------------------------------

(let ((loadpath *load-pathname*))
  (when loadpath
    (set-qrg-path (namestring (make-pathname
                               :host (pathname-host loadpath)
                               :device (pathname-device loadpath)
                               :directory (pathname-directory loadpath))))))


;;; ----------------------------------------------------------------------------
;;; END OF CODE
