(in-package :common-lisp-user)

(asdf:defsystem #:sicl-clos
  :depends-on (#:sicl-additional-types
	       #:sicl-additional-conditions)
  :components
  ((:file "packages"
    :depends-on ())
   (:file "utilities"
    :depends-on ("packages"))
   (:file "class-slots"
    :depends-on ("packages"
		 "utilities"))
   (:file "standard-instance"
    :depends-on ("packages"
		 "utilities"))
   (:file "clos"
    :depends-on ("packages"
		 "utilities"
		 "standard-instance"))
   (:file "built-in-classes"
    :depends-on ("packages"
		 "utilities"
		 "clos"))
   (:file "generic-functions"
    :depends-on ("packages"
		 "utilities"
		 "standard-instance"
		 "clos"))))
	  
