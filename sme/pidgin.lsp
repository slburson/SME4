;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                         -*-
;;;; --------------------------------------------------------------------------
;;;; File name: pidgin.lsp
;;;;    System: SME
;;;;   Version: 4
;;;;    Author: Ron Ferguson, Qualitative Reasoning Group
;;;;   Created: September 17, 1997
;;;;  Modified: Ron Ferguson on Thurs Aug 6 11:15:10 1998
;;;;   Purpose: Handle pidgin english output
;;;; --------------------------------------------------------------------------
(in-package :sme)

;;;; Backward compatibility, hasn't been used this century.

(defun test-pidgin (&optional (dgroup-name "swater") (vocab "language"))
   (vocabulary-from-file vocab)
   (let* ((base (dgroup-from-file dgroup-name))
          (exprs (reverse (get-dgroup-expressions base))))
      (dolist (expr exprs)
         (format t "Expression: ~A~%" (user-form expr))
         (format t "    Pidgin: ~A~%" (pidgin expr)))))
 
(defmethod pidgin ((object description))
   "Return the pidgin english for an entire dgroup."
   (mapcar #'(lambda (root) (clean-up-sentence (pidgin root)))
     (reverse (roots object))))

(defmethod pidgin ((object expression))
   (expression->pidgin object))

(defmethod pidgin ((object entity))
   "Return the pidgin english for an object."
   (let ((form (user-form object)))
      (unless (stringp form)
         (setq form (format nil "~A" form)))
      form))

(defun expression->pidgin (expression)
   "Translate a single expression into pidgin english."
   (let* ((pred (predicate expression))
          (pidgin-string (pidgin-description-string pred)))
      (cond ((= (length pidgin-string) 0) 
             (construct-default-pidgin expression))
            (t (substitute-pidgin-string 
                 pidgin-string (arguments expression))))))

(defun clean-up-sentence (string)
   "Clean up a sentence string by trimming it, capitalizing
  the first letter, and adding a period."
   (concatenate 'string 
     (string-capitalize (string-trim '(#\space #\tab) string) :end 1)
     "."))

(defun substitute-pidgin-string (pidgin-string arguments)
   "For each of the case relations (car) in the argument set,
    substitute the pidgin english for that argument 
    instead of the variable for the case relation."
   (let ((string-list (parse-with-delimiters pidgin-string)))
      (apply 'concatenate 'string
        (mapcan #'(lambda (string-token)
                    (let ((replacement
                           (when (string-variable? string-token)
                              (cdr (assoc string-token arguments
                                     :test 'string-var-eql?)))))
                       (if replacement
                          (list (string (pidgin replacement)) " ")
                          (list string-token " "))))
          string-list))))

(defun string->list (string character-bag)
   "Returns a list of substrings, with the characters in the character-bag
    string used as delimiters."
   ;;; Example: "This is a  test  " -> ("This" "is" "a" "test").
   (let ((result nil))
      (flet ((in-bag (character)
              (find character character-bag :test #'char-equal)))
        (do* ((start (position-if-not #'in-bag string)
                (position-if-not #'in-bag string :start (1+ end)))
              (end (or (position-if #'in-bag string :start (1+ start))
                       (length string))
               (or (position-if #'in-bag string :start (1+ start))
                   (length string))))
             ((= end (length string))
              (progn
                (push (subseq string start end) result)
                (reverse result)))
           (push (subseq string start end) result))))
   
   )

;; This function taken from Mark Kantrowitz's Lisp utility library.
;;   Modified so that it removes null strings from the results.
;;   Kludgy and inefficient, unfortunately.  May want to write something
;;   faster later.
(defun parse-with-delimiters (line &optional (delimiters '(#\space #\tab 
                                                           #\newline))
                               (punctuation '(#\. #\, #\! #\( #\) #\')))
  (remove-if #'(lambda (string) (= (length string) 0))
    (parse-with-delimiters-1 line delimiters punctuation)))

(defun parse-with-delimiters-1 (line delimiters &optional punctuation)
  "Breaks LINE into a list of strings, using DELIMITERS as a 
   breaking point."
  ;; what about #\return instead of #\newline?
  (let* ((pos (position-if #'(lambda (character) (find character delimiters))			 
                line))
         (pos2 (position-if #'(lambda (character) (find character punctuation))
                line)))
     (cond ((or (and pos2 (not pos))
                (and pos pos2 (> pos pos2)))
            (if (zerop pos2) ;; punctuation at front of line.
               (cons (subseq line 0 1)
                 (parse-with-delimiters-1 (subseq line 1)
                   delimiters punctuation))
               (cons (subseq line 0 pos2)
                 (parse-with-delimiters-1 (subseq line pos2) delimiters 
                   punctuation))))
           ((or (and pos (not pos2))
                (and pos pos2 (< pos pos2)))
            (cons (subseq line 0 pos)
              (parse-with-delimiters-1 (subseq line (1+ pos)) delimiters
                punctuation)))
           ((not (or pos pos2))
            (list line)))))

(defun string-variable? (string)
   "Is the given string a variable?"
   (char= (schar string 0) #\?))

(defun string-var-eql? (string-var symbol)
   "Is the given string var the same as the symbol name?"
   (string-equal (subseq string-var 1) (string symbol)))

(defun construct-default-pidgin (expression)
   "Construct a default pidgin string for an expression whose
    predicate doesn't have a documented pidgin-description-string."
   (let ((pred (predicate expression))
         (args (arguments expression)))
      (cond ((eql (pred-type pred) :logical)
             (format nil "(~A ~{ (~A) ~})" (name pred)
               (mapcar #'(lambda (arg)
                           (pidgin (cdr arg)))
                 args)))
            (t
              (case (length args)
                (1 (if (function? pred)
                      (format nil "(the ~A of ~A)" (name pred)
                        (pidgin (expr-arg-item (car args))))
                      (format nil "(~A is an attribute of ~A)."
                        (name pred)
                        (pidgin (expr-arg-item (car args))))))
                (2 (format nil "(~A relates ~A to ~A)."
                     (name pred)
                     ;;(expr-arg-case-relation (first args))
                     (pidgin (expr-arg-item (first args)))
                     ;;(expr-arg-case-relation (second args))
                     (pidgin (expr-arg-item (second args)))))
                (otherwise (format nil "(~A relates~{ ~@(~A~) ~A,~})."
                             (name pred)(mapcan #'(lambda (x) 
                                                    (list (car x) (pidgin (cdr x))))
                                          args))))))))
