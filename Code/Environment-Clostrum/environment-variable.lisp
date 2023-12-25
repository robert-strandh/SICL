(cl:in-package #:sicl-environment)

;;; This variable must be bound when standard "environment" functions
;;; such as FDEFINITION are loaded into some environment.  These
;;; functions define a lexical variable that is initialized to the
;;; value of this variable and that is then closed over so that the
;;; function can refer to it at run time.

(defvar *environment*)

(defvar *client*)
