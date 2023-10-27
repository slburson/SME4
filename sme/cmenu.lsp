;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: cmenu.lsp
;;;;    System: SME
;;;;   Version: 2.00
;;;;    Author: Qualitative Reasoning Group
;;;;   Created: ???
;;;;  Modified: Monday, May 14, 2007 at 10:16:07 by Ken Forbus
;;;;   Purpose: Character-oriented menu system.
;;;;            Duplicates a reasonable subset of the old Symbolics menu system,
;;;;             without the machine-specific aspects.
;;;;
;;;; ----------------------------------------------------------------------------
(in-package :common-lisp-user)

(defvar *line-length* 80 "The line length of the terminal being used.")
(defvar *help-key* '? "Standard help key.") ;; can be re-bound
(defvar *abort-key* 'q "Standard abort key.")
(defvar *menu-stream* *standard-output* "Standard stream for menu system")
(defvar *cmenu-prompt* ">->" "Prompt used for menu items.")
(eval-when (compile load eval)
  (proclaim '(special *cmenu-prompt*))) ;; can be lambda-bound.

;;;; Basic choice menu
;;; ----------------------------------------------------------------------------
;; When there is a discrete choice to make, this is the menu to use.
;; Items are specified as (<printed form> <value returned>). 

(defun cmenu-choose (item-list &optional (header "Choose one of")
			       (help "~% Sorry, no help available."))
   "Have user choose from set of items in item list"
   (cond 
     ((null item-list) nil)
     (t 
       (do ((len (length item-list))
            (answer nil)
            (result nil)
            (done? nil))
           (done? result)
          (show-basic-menu-choices header item-list)
          (setq answer (read-stuff))
          (cond ((integerp answer)
                 (cond ((or (< answer 1) (> answer len))
                        (format *menu-stream* "~%  Must be between 1 and ~D." len))
                       (t (setq result (cadr (nth (1- answer) item-list))
                            done? t))))
                ((eq answer :punt) (setq done? t))
                ((eq answer :help) (format *menu-stream* "~A" help))
                (t (format *menu-stream* "~%  Must be an integer between 1 and ~D."
                     len)))))))

(defun choice-aborted? (choice) (or (null choice) (eq choice :punt)))

(defun show-basic-menu-choices (header items)
  ;; This version tries to put as many things on a line as will fit
   "Display set of menu choices (saving space)"
  (format *menu-stream* "~%~A:~%" header)
  (do ((i 1 (1+ i))
       (room-left *line-length*)
       (item "") (len 0)
       (counter 0 (1+ counter))
       (choice (car items) (car rest))
       (rest (cdr items) (cdr rest)))
      ((null choice))
    (setq item (format nil "~D: ~A " i (car choice))
	  len (length item))
    (when (or (> len room-left) (< room-left 0))
      ;; 2nd case is something being too long
      (format *menu-stream* "~%")
      (setq room-left *line-length*))
    (format *menu-stream* "~A" item)
    (setq room-left (- room-left len))))

(defun print-cmenu-prompt (&optional (header ""))
  (if (string= header "") (format t "~%~A" *cmenu-prompt*)
      (format t "~%~A" header)))

(defun read-stuff (&optional (header ""))
  (print-cmenu-prompt header)
  (let ((thing (read *standard-input*)))
    (cond ((symbolp thing)
	   ;; get rid of package dependence for special
	   ;; cases, so far just Q and ?.
	   (let ((name (symbol-name thing)))
      (cond ((or (string= name "q")
                 (string= name "Q")) :punt)
		   ((string= name "?") :help)
		   (t thing))))
          (t thing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Choosing variable values
;;
;; Once a variable has been chosen, select a value for it.
;; Item lists here are (<printed version> <variable> <value-type>).
;; It is assumed that all variables are bound.

;; Supported datatypes: 
;;
;; :anything
;; :integer
;; :float
;; :string
;; :list-of-strings (for lists of files)
;;  This facility is data-driven, so new types may be added.

(defun cmenu-choose-values (item-list &optional (header "Choose values for")
			    (help ""))
  (do ((len (length item-list))
       (answer nil)
       (done? nil))
      (done? nil)
    (show-cvalues-menu-choices header item-list)
    (setq answer (read-stuff))
    (cond ((integerp answer)
	   (cond ((or (< answer 1) (> answer len))
		  (format *menu-stream* "~%  Must be between 1 and ~D." len))
		 (t (twiddle-variable-value (nth (1- answer) item-list)))))
	  ((eq answer ':punt) (setq done? t))
	  ((eq answer ':help) (basic-menu-documentation item-list len help))
	  (t (format *menu-stream* "~%  Must be an integer between 1 and ~D."
		     len)))))

(defun show-cvalues-menu-choices (header items)
  (format *menu-stream* "~%~A:" header)
  (do ((i 1 (1+ i))
       (choice (car items) (car rest))
       (rest (cdr items) (cdr rest)))
      ((null choice))
    (format *menu-stream* "~%  ~D: ~A [~A]" i (car choice) 
	    (symbol-value (cadr choice)))))

(defun twiddle-variable-value (var-spec)
  (let ((val (funcall (lookup-cvalues-type-reader (caddr var-spec)) var-spec)))
    (unless (eq val ':no-value-provided)
      (set (cadr var-spec) val))))

(defvar *cvalues-types* nil)

(defun lookup-cvalues-type-reader (type)
  (let ((entry (assoc type *cvalues-types*)))
    (if entry (cdr entry) (function read))))

(defmacro define-cvalues-type (type function)
  `(let ((entry (assoc ',type *cvalues-types*)))
     (unless entry
       (push (setq entry (cons ',type nil)) *cvalues-types*))
     (setf (cdr entry) (function ,function))))

(defun read-list-of-unempty-strings ()
  (do ((answer nil)
       (current nil))
      (nil)
    (setq current (read-line))
    (when (string= current "")
      (return (nreverse answer)))
    (push current answer)))

(defun read-integer ()
  (do ((answer nil))
      (nil)
    (setq answer (read))
    (cond ((integerp answer) (return answer))
	  (t (print-cmenu-prompt " Must be an integer...try again.:")))))

(defun read-float ()
  (do ((answer nil))
      (nil)
    (setq answer (read))
    (cond ((floatp  answer) (return answer))
	  (t (print-cmenu-prompt " Must be a flonum...try again.")))))

(define-cvalues-type :anything
		     (lambda (var-spec)
		       (print-cmenu-prompt
			(format nil " New value for ~A:" (car var-spec)))
		       (read)))
(define-cvalues-type :string
		     (lambda (var-spec)
		       (print-cmenu-prompt
			(format nil " New value for ~A:" (car var-spec)))
		       (read-line)))
(define-cvalues-type :list-of-strings
		     (lambda (var-spec)
		       (print-cmenu-prompt
			(format nil " New value for ~A:" (car var-spec)))
		       (read-list-of-unempty-strings)))
(define-cvalues-type :integer
		     (lambda (var-spec)
		       (print-cmenu-prompt 
			(format nil " New value for ~A:" (car var-spec)))
		       (read-integer)))
(define-cvalues-type :float
		     (lambda (var-spec)
		       (print-cmenu-prompt 
			(format nil " New value for ~A:" (car var-spec)))
		       (read-float)))

(define-cvalues-type :one-of (lambda (var-spec)
			       (let ((choices (cadr (member :one-of var-spec))))
				 (cmenu-choose
				   choices 
				   (format nil "~% Select new value for ~A:"
					   (car var-spec))))))
(define-cvalues-type :boolean (lambda (var-spec)
				(yes-or-no-p (car var-spec))))

;;;; Command menus
;;; ----------------------------------------------------------------------------
;;
;; A minimalist implementation of a command processor.
;; An entry is:
;; (<key> <code> &optional :documentation <doc string>)
;;
;; For all command menus, Q means quit, 0 means re-display options.
;; Some actions will in turn be command menus, of course. 

(defun run-command-menu (cmenu header &aux len hprompt)
  (setq len (length cmenu))
  (setq hprompt (concatenate 'string header ">>"))
  ;; Runs command menu until told otherwise
  (show-basic-menu-choices header cmenu)
  (do ((answer (read-stuff hprompt)))
      (nil)
      (case answer
	    (:punt (return t))
	    (:help (basic-menu-documentation cmenu len ""))
	    (0 (show-basic-menu-choices header cmenu))
	    (t (cond ((integerp answer)
		      (cond ((or (< answer 1) (> answer len))
			     (format t
				     "~%  Must be between 1 and ~D." len))
			    (t (eval (cadr (nth (1- answer) cmenu))))))
		     (t (format t "<-  Must be 0,1...~D, Q, or ?." len)))))
      (setq answer (read-stuff hprompt))))

(defvar *basic-menu-help*
"  0: Redisplay choices.
  Q: Quits
  ?: Help (if available)
  #: That choice, or help concerning it.") 

(defun basic-menu-documentation (cmenu len &optional (global-help ""))
  (unless (string= global-help "")
    (format t "~%~A" global-help))
  (format t "Either ?,Q, or 1-~D:" len)
  (let ((answer (read-stuff)))
    (cond ((eq answer ':help)
	   (format t *basic-menu-help*))
	  ((eq answer ':punt))
	  ((integerp answer)
	   (cond ((or (< answer 0) (> answer len))
		  (basic-menu-documentation cmenu len global-help))
		 (t (let ((answer (cadr (member :documentation (nth (1- answer) cmenu)))))
		      (cond (answer (format t "~%~A" answer))
			    (t (format t "~% Sorry, no help available for item #~D" len)))))))
	  (t (basic-menu-documentation cmenu len)))))


