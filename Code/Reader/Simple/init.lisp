(cl:in-package #:sicl-reader)

(defparameter *standard-readtable*
  (make-instance 'sicl-simple-readtable:readtable))

(loop for char in '(#\Space #\Tab #\Linefeed #\Return #\Page)
      do (setf (sicl-readtable:syntax-type *standard-readtable* char)
	       :whitespace))

(setf (sicl-readtable:syntax-type *standard-readtable* #\\)
      :single-escape)

(setf (sicl-readtable:syntax-type *standard-readtable* #\|)
      :multiple-escape)

(sicl-readtable:set-macro-character
 *standard-readtable* #\( 'left-parenthesis)

(sicl-readtable:set-macro-character
 *standard-readtable* #\) 'right-parenthesis nil)

(sicl-readtable:set-macro-character
 *standard-readtable* #\' 'single-quote nil)

(sicl-readtable:set-macro-character
 *standard-readtable* #\" 'double-quote nil)

(sicl-readtable:set-macro-character
 *standard-readtable* #\; 'semicolon nil)

(sicl-readtable:set-macro-character
 *standard-readtable* #\` 'backquote nil)

(sicl-readtable:set-macro-character
 *standard-readtable* #\, 'comma nil)

(sicl-readtable:make-dispatch-macro-character
 *standard-readtable* #\# t)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\' 'sharpsign-single-quote)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\( 'sharpsign-left-parenthesis)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\. 'sharpsign-dot)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\\ 'sharpsign-backslash)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\b 'sharpsign-b)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\x 'sharpsign-x)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\o 'sharpsign-o)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\r 'sharpsign-r)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\* 'sharpsign-asterisk)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\| 'sharpsign-vertical-bar)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\a 'sharpsign-a)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\: 'sharpsign-colon)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\c 'sharpsign-c)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\p 'sharpsign-p)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\+ 'sharpsign-plus)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\- 'sharpsign-minus)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\= 'sharpsign-equals)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\# 'sharpsign-sharpsign)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\< 'sharpsign-invalid)

(sicl-readtable:set-dispatch-macro-character
 *standard-readtable* #\# #\) 'sharpsign-invalid)

(setf *readtable* (sicl-readtable:copy-readtable *standard-readtable*))
