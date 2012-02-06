(in-package :common-lisp-user)

(asdf:defsystem #:sicl-clos
  :components
  ((:file "packages"
    :depends-on ())
   (:file "utilities"
    :depends-on ("packages"))
   (:file "conditions"
    :depends-on ("packages" "utilities"))
;;   (:file "condition-reporters-en"
;;    :depends-on ("packages" "utilities" "conditions"))
   (:file "class-slots"
    :depends-on ("packages" "utilities" "conditions"))
   (:file "clos"
    :depends-on ("packages" "utilities" "conditions"))))
	  
