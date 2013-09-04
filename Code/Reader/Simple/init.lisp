(cl:in-package #:sicl-reader)

(defparameter *standard-readtable* (make-instance 'readtable))

(loop for char in '(#\Space #\Tab #\Linefeed #\Return #\Page)
      do (setf (gethash char (syntax-types *standard-readtable*))
	       :whitespace))

(setf (gethash #\\ (syntax-types *standard-readtable*))
      :single-escape)

(setf (gethash #\| (syntax-types *standard-readtable*))
      :multiple-escape)

(set-macro-character #\( #'left-parenthesis nil *standard-readtable*)
(set-macro-character #\) #'right-parenthesis nil *standard-readtable*)
(set-macro-character #\' #'single-quote nil *standard-readtable*)
(set-macro-character #\" #'double-quote nil *standard-readtable*)
(set-macro-character #\; #'semicolon nil *standard-readtable*)
(set-macro-character #\` #'backquote nil *standard-readtable*)
(set-macro-character #\, #'comma nil *standard-readtable*)


(setf *readtable* (copy-readtable *standard-readtable*))
