(cl:in-package #:cleavir-value-hoisting-test)

;;; In this file, we define an example client for the value hoisting
;;; machinery.  We use this client for testing, but it is also an
;;; illustration of how to design a custom client.

(defclass client (trucler-native:native-client)
  ())

(defvar *make-string*)

(defvar *cons*)

(defvar *find-package*)

(defvar *intern*)

(defvar *function-cell*)
