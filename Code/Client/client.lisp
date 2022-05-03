(cl:in-package #:sicl-client)

(defclass sicl (trucler-reference:client)
  ())

(defclass x86-64 () ())

(defclass sicl-x86-64 (sicl x86-64) ())

;;; This special variable should be bound whenever some processing is
;;; needed that requires a client object.  Typically the bootstrapping
;;; procedure will bind it, and so will the top-level REPL.
(defvar *client*)
