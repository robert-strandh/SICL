(cl:in-package :sicl-cons-high)

;;;; Copyright (c) 2008 - 2015
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

;;;; This file is part of the cons-high module of the SICL project.
;;;; See the file SICL.text for a description of the project. 
;;;; See the file cons-high.text for a description of the module.

;;; special version of last used when the second argument to
;;; last is 1. 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun last-1 (list)
    (unless (typep list 'list)
      (error 'must-be-list
	     :datum list
	     :name 'last))
    ;; We can use for ... on, because it uses atom to test for
    ;; the end of the list. 
    (loop for rest on list
	  do (setf list rest))
    list))

(define-compiler-macro last (&whole form list &optional (n 1))
  (if (eql n 1)
      `(last-1 ,list)
      form))
