(cl:in-package #:common-lisp-user)

(defpackage #:sicl-stream
  (:use #:common-lisp)
  (:shadow #:finish-output
	   #:force-output
	   #:clear-output)
  (:export
   #:finish-output
   #:force-output
   #:clear-output))
