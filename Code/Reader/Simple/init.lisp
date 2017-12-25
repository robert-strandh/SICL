(cl:in-package #:sicl-reader)

(defparameter *standard-readtable* (make-instance 'readtable))

(loop for char in '(#\Space #\Tab #\Linefeed #\Return #\Page)
      do (setf (gethash char (syntax-types *standard-readtable*))
	       :whitespace))

(setf (gethash #\\ (syntax-types *standard-readtable*))
      :single-escape)

(setf (gethash #\| (syntax-types *standard-readtable*))
      :multiple-escape)

(set-macro-character #\( 'left-parenthesis nil *standard-readtable*)
(set-macro-character #\) 'right-parenthesis nil *standard-readtable*)
(set-macro-character #\' 'single-quote nil *standard-readtable*)
(set-macro-character #\" 'double-quote nil *standard-readtable*)
(set-macro-character #\; 'semicolon nil *standard-readtable*)
(set-macro-character #\` 'backquote nil *standard-readtable*)
(set-macro-character #\, 'comma nil *standard-readtable*)

(make-dispatch-macro-character #\# t *standard-readtable*)

(set-dispatch-macro-character
 #\# #\' 'sharpsign-single-quote *standard-readtable*)

(set-dispatch-macro-character
 #\# #\( 'sharpsign-left-parenthesis *standard-readtable*)

(set-dispatch-macro-character
 #\# #\. 'sharpsign-dot *standard-readtable*)

(set-dispatch-macro-character
 #\# #\\ 'sharpsign-backslash *standard-readtable*)

(set-dispatch-macro-character
 #\# #\b 'sharpsign-b *standard-readtable*)

(set-dispatch-macro-character
 #\# #\x 'sharpsign-x *standard-readtable*)

(set-dispatch-macro-character
 #\# #\o 'sharpsign-o *standard-readtable*)

(set-dispatch-macro-character
 #\# #\r 'sharpsign-r *standard-readtable*)

(set-dispatch-macro-character
 #\# #\* 'sharpsign-asterisk *standard-readtable*)

(set-dispatch-macro-character
 #\# #\| 'sharpsign-vertical-bar *standard-readtable*)

(set-dispatch-macro-character
 #\# #\a 'sharpsign-a *standard-readtable*)

(set-dispatch-macro-character
 #\# #\: 'sharpsign-colon *standard-readtable*)

(set-dispatch-macro-character
 #\# #\c 'sharpsign-c *standard-readtable*)

(set-dispatch-macro-character
 #\# #\p 'sharpsign-p *standard-readtable*)

(set-dispatch-macro-character
 #\# #\+ 'sharpsign-plus *standard-readtable*)

(set-dispatch-macro-character
 #\# #\- 'sharpsign-minus *standard-readtable*)

(set-dispatch-macro-character
 #\# #\= 'sharpsign-equals *standard-readtable*)

(set-dispatch-macro-character
 #\# #\# 'sharpsign-sharpsign *standard-readtable*)

(set-dispatch-macro-character
 #\# #\< 'sharpsign-invalid *standard-readtable*)

(set-dispatch-macro-character
 #\# #\) 'sharpsign-invalid *standard-readtable*)

(setf *readtable* (copy-readtable *standard-readtable*))
