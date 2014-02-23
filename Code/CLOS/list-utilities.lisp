(cl:in-package #:sicl-clos)

;;;; This file contains some utilities that are useful in
;;;; discriminating functions, and perhaps elsewhere as well.
;;;; 
;;;; The utilities defined here are special versions of functions that
;;;; exist in standard Common Lisp.  The reason for not using the more
;;;; general existing version is that those versions might be defined
;;;; as GENERIC functions in some implementations, in particular in
;;;; SICL, and we these functions in the general machinery for making
;;;; generic functions work.  So to avoid circular dependencies and
;;;; metastability issues, we define these special versions that only
;;;; work on list. 

;;; Non-destructive list reversal. 
(defun list-reverse (list)
  (loop with result = '()
	for element in list
	do (push element result)
	finally (return result)))
