;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: charsme
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: March 9, 1994
;;;;  Modified: Wednesday, July 28, 1999 at 12:02:38 by ferguson
;;;;   Purpose: character interface for SME
;;;; --------------------------------------------------------------------------
;;;; Character-oriented interface for SME 4.  Uses cmenu system.
;;;; Useful for simple interactive experimentation.

(in-package :common-lisp-user)

;;; Global Variables
;;; ----------------------------------------------------------------------------
(defvar *charsme-all-matches* nil     "All SME matches done so far")
(defvar *charsme-current-target* nil  "Menu's current target description")
(defvar *charsme-current-base* nil    "Menu's current base description")
(defvar *charsme-vocabulary-file* nil "Vocabulary file for menu system")

;; cmenu Special variables
(eval-when (compile load eval)
  (proclaim '(special *cmenu-prompt* *sme-command-menu* *menu-stream*)))

(defun reinit-charsme ()
   "Reinitialize important SME character interface varables."
  (setq *charsme-all-matches* nil)
  (setq *charsme-current-target* nil)
  (setq *charsme-current-base* nil)
  (format *menu-stream* "...done.~%"))

;;; Main Menu
;;; ----------------------------------------------------------------------------
(defvar *sme-command-menu*
  '(("Reinitialize" (reinit-charsme))
    ("Set defaults" (menu-set-defaults)
     :documentation "Set up the defaults.")
    ("Create a match" (menu-create-match-script)
     :documentation "Run through script to create a match in SME")
    ("Examine a match" (menu-examine-last-match)
     :documentation "Examine the current match."))
  "Main command menu for SME character interface")

(defun sme-toplevel ()
   "Top-level routine for SME character interface.  This is the old
    SME character interface.  For the alpha version of the new 
    character interface, see the routine NEW-SME-TOPLEVEL."
   (let ((cl-user::*cmenu-prompt* "SME >>"))
      (run-command-menu *sme-command-menu* "SME: Toplevel Menu")))

(defun get-description-path ()
   "Prompt user for description path. Returns path."
   (cmenu-choose-directory "The description directory"
     sme::*sme-description-path*
     sme::*sme-description-extension*))

(defun get-dgroup (prompt)
   "Prompt user for dgroup.  Returns filename."
   (cmenu-choose-file sme:*sme-description-path* 
     sme::*sme-description-extension* prompt))

;;; create-match menu
;;; ----------------------------------------------------------------------------
(defun menu-create-match-script (&aux base target)
   "Script for creating a match between dgroups.  Returns SME match"
   (loop while (null sme::*sme-description-path*)  ;; no path set yet!
     do (progn (format t "I don't know the description path yet.~%")
          (format t "Enter it now?")
          (if (yes-or-no-p)
             (setq sme::*sme-description-path* (get-description-path))
             (return-from menu-create-match-script))))
   (setq base (get-dgroup "Choose a base description:"))
   (setq target (get-dgroup "Choose a target description:"))
   (when base (setq *charsme-current-base* (load-description base)))
   (when target (setq *charsme-current-target* (load-description target)))
   (when (and base target)
      (sme:define-sme *charsme-current-base* *charsme-current-target*))
   (format *menu-stream* "Created ~A~%" sme:*sme*)
   (sme:match sme:*sme*)
   (push sme:*sme* *charsme-all-matches*))

;;; Examine last match.
;;; ----------------------------------------------------------------------------
(defun menu-examine-last-match ()
   "Menu for examining the current match."
  (run-command-menu '(("Vital statistics" (show-vital-stats sme:*sme*))
		      ("Show mappings" (sme::show-string sme:*sme*))
		      ("Show base" (sme::show-string *charsme-current-base*))
		      ("Show target" (sme::show-string *charsme-current-target*))
		      ("Show match hypotheses" (sme::show-mhs sme:*sme*))
		      ("List of matches available" (show-all-current-matches))
		      ("Select another match" (menu-choose-new-current-match)))
		    (format nil "SME: Examine match menu")))

(defun show-vital-stats (sme)
   "Show vital statistics for the current match."
   (format *menu-stream* "SME: ~A~%" (header-form sme))
   (format *menu-stream* "Number of MHs: ~A~%" (length (sme::mhs sme:*sme*)))
   (format *menu-stream* "Number of Gmaps: ~A~%" (length (sme:mappings 
                                                          sme:*sme*))))

;; == Examine (and compare) all matches
(defun menu-examine-all-matches ()
   "Menu for examining sets of matches."
   (run-command-menu 
     '(("List of matches available" (show-all-current-matches))
       ("Choose a new current match" (menu-choose-new-current-match))
       ("Examine current match" (menu-examine-last-match)))
     "SME: Examine all matches"))

(defun show-all-current-matches ()
   "Display the set of all current matches"
   (when (null *charsme-all-matches*)
      (format *menu-stream* "No matches have been built.")
      (return-from show-all-current-matches))
   (dolist (match *charsme-all-matches*)
      (format *menu-stream* "~A~%" (header-form match))))

(defmethod header-form ((sme sme::sme))
   "Return a short, easily readable description of an SME object"
   (format nil "<Base: ~A  Target: ~A  Id: ~A>"
     (sme::name (sme:base sme))
     (sme::name (sme:target sme))
     (sme::id sme)))

(defun menu-choose-new-current-match (&aux menu)
   "User chooses a new current match"
   (when (null *charsme-all-matches*)
      (format *menu-stream* "No matches have been built yet.~%")
      (return-from menu-choose-new-current-match))
   (setq menu (mapcar #'(lambda (sme)
                          (list (header-form sme) sme))
                *charsme-all-matches*))
   (setq sme:*sme* (or (cmenu-choose menu "Choose a match")
                       sme::*sme*))
   (setq *charsme-current-target* (sme:target sme:*sme*))
   (setq *charsme-current-base* (sme:base sme:*sme*)))

(defun menu-set-defaults ()
   "User resets some or all defaults of SME."
  (cmenu-choose-values
   '(("Greedy merge--maximum # of maps" sme::*default-greedy-max-#* :integer)
     ("Greedy merge--default cutoff percentage" sme::*default-greedy-cutoff* :float)
     ("Description directory" sme::*sme-description-path* :directory "dgr")
     ("Vocabulary directory" sme::*sme-vocabulary-path* :directory "vcb")
     ("Current vocabulary" sme::*vocabulary* :vocabulary)
     ("Description extension" sme::*sme-description-extension* :string)
     ("Vocabulary extension" sme::*sme-vocabulary-extension* :string)
     ("Warn on kernel plateaus?" sme::*warn-on-kernel-plateaus?* :boolean)
     ("Warn on greedy inversion?" sme::*warn-on-greedy-inversion?* :boolean)
     ("Terminal width" *line-length* :integer))
   "Choose defaults for"))

(defun cmenu-choose-file (directory extension
                                    &optional (header "Choose one of") 
                                    (help "Sorry, no help available."))
   "User chooses file from among those with given path and extension"
  ;; First, do some error checking.
  (cond ((null directory)
	 (format t "No directory has been defined yet for these files.~%")
	 (return-from cmenu-choose-file))
	((null extension)
	 (format t "No extension has been defined yet for these files.~%")
	 (return-from cmenu-choose-file)))
  (let* ((full-extension (concatenate 'string "." extension))
         (choose-file-menu (files-fitting directory full-extension)))
     ;; Make menu.
     (setq choose-file-menu 
        (mapcar #'(lambda (x) (list x x)) choose-file-menu))
     (when (null choose-file-menu)
        (format *menu-stream* "No files with extension '~A' in directory '~A'."
          extension directory))
     (do ((file nil))
         (file file)
        (setq file (cmenu-choose choose-file-menu header help))
        (cond ((or (null file) (eq file :punt))
               (return-from cmenu-choose-file t))
              (t file)))))

(defun menu-choose-vocabulary ()
   "Have user select vocabulary from current directory."
   (setq *charsme-vocabulary-file*
     (cmenu-choose-file sme::*sme-vocabulary-path* sme::*sme-vocabulary-extension*
       "Choose a vocabulary file:"))
   (when (not (eq *charsme-vocabulary-file* t))
      (setq sme::*vocabulary*
        (sme:vocabulary-from-file (namestring *charsme-vocabulary-file*)))))

(define-cvalues-type :vocabulary
    (lambda (var-spec)
      (set (cadr var-spec) (menu-choose-vocabulary))))

(define-cvalues-type :directory
    (lambda (var-spec)
      (cmenu-choose-directory
       (car var-spec) (symbol-value
		       (cadr var-spec)) (fourth var-spec))))

;; Have the user choose a directory for some purpose.
;; If the file extension is given, be sure that the
;;  directory already contains files with that extension.
;;
(defun cmenu-choose-directory (directory-name &optional default-dir file-extension
					      &aux new-dir usable-files done)
  (if default-dir
      (format t "The default directory is '~A'.~%" (namestring default-dir))
      (format t "There is not yet a default directory.~%"))
  (loop
   (when done (return default-dir))
   (print-cmenu-prompt
    (format nil " New value for ~A [default: '~A'] " directory-name default-dir))
   (setq new-dir (cmenu-input-string))
   (terpri)
   (cond
     ((null new-dir)
      (setq done t));; if null, return the default.
     ((not (stringp new-dir));; if not string, complain.
      (format t "Please input a string.~%"))
     ((not (probe-file new-dir));; no such directory.
      (format t "No directory with that name exists.~%"))
     (t
      (setq usable-files (directory
			  (concatenate 'string new-dir "*." file-extension)))
      (cond (usable-files
	     (format t "~A files exist with the '.~A' extension.~%"
		     (length usable-files) file-extension))
	    (t
	     (format t "Warning--no files exist in directory  '~A' with the '.~A' extension.~%"
		     new-dir file-extension)))
      (setq done t)
      (setq default-dir (pathname new-dir))))))

(defun load-description (pathname)
  "Load a description."
  (format *menu-stream* "Loading file ~A...~%" (namestring pathname))
  (sme:dgroup-from-file (namestring pathname)))

(defun cmenu-input-string (&aux instring) 
   "Take input string, and trim whitespace.  Return NIL for null string."
   (setq instring
     (string-trim '(#\space #\tab #\newline) (read-line))) 
   (if (= (length instring) 0)
      nil 
      instring))

(defun files-fitting (pathname &rest file-extensions)
  "Create a list of files in the given directory with the given
   extension.  The extension should include the period (.), and 
   the pathname should end with a pathname separator."
  (mapcan #'(lambda (file-extension)
              (mapcar #'file-namestring
                      (directory (format nil "~A*~A"
                                         pathname file-extension))))
     file-extensions))



