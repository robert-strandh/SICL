;;;; Copyright (c) 2014, 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

;;;; This file is part of the string module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file string.text for a description of the module.

(cl:in-package #:sicl-string)

(defmethod acclimation:report-condition
    ((c bag-is-dotted-list)
     stream
     acclimation:english)
  (format stream
	  "If a character bag is a list, it must be a proper list.~@
           But the following dotted list was found instead:~@
           ~s."
	  (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c bag-is-circular-list)
     stream
     acclimation:english)
  (format stream
	  "If a character bag is a list, it must be a proper list.~@
           But the following circular list was found instead:~@
           ~s."
	  (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c bag-contains-non-character)
     stream
     acclimation:english)
  (format stream
	  "A character bag must be a sequence that contains only characters.~@
           But the following element was found which is not a character:~@
           ~s."
	  (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c invalid-bounding-indices)
     stream
     acclimation:english)
  (format stream
	  "In order for START and END to be valid bounding indices,~@
           START must between 0 and the length of the string, and~@
           END must be between 0 and the length of the string or NIL,~@
           and START must be less than or equal to END.~@
           But the following values of START and END were found:~@
           ~s, ~s~@
           For the string:~@
           ~s~@
           And the length of that string is ~s."
	  (start c) (end c) (target c) (length (target c))))
