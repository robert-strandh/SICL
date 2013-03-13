(in-package :common-lisp-user)

(asdf:defsystem #:sicl-clos
  :depends-on (#:sicl-additional-types
	       #:sicl-additional-conditions
	       #:sicl-code-utilities)
  :components
  ((:file "packages"
    :depends-on ())
   (:file "slot-definition"
    :depends-on ("packages"))
   (:file "classes"
    :depends-on ("packages"))
   (:file "generic-functions"
    :depends-on ("packages"))))
	  
