(cl:in-package #:sicl-reader)

;;;; This file contains definitions of standardized variables that
;;;; affect the behavior of the reader.  See section 2.1.2 in the
;;;; HyperSpec for the meaning of these variables.  
;;;;
;;;; When the reader is compiled for the purpose of cross compilation,
;;;; this file can not be used, because it would violate the Common
;;;; Lisp rules with respect to the COMMON-LISP package, so instead,
;;;; these variables are taken from host.

(defparameter *package* (find-package '#:common-lisp-user)

(defparameter *read-eval* t)

(defparameter *read-base* 10.)

(defparameter *read-default-float-format* 'single-float)

(defparameter *read-suppress* nil)
