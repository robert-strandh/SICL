(cl:defpackage #:sicl-setf
  (:use :common-lisp)
  (:shadow . #1= (#:define-setf-expander
		  #:defsetf
		  #:get-setf-expansion))
  (:export . #1#))
   

