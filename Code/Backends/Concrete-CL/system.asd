(cl:in-package #:asdf-user)

(defsystem #:sicl-exp
  :components
  ((:file "packages"
    :depends-on ())
   (:file "utilities"
    :depends-on ("packages"))
   (:file "system"
    :depends-on ("packages"
		 "utilities"))
   (:file "heap"
    :depends-on ("packages"
		 "utilities"
		 "system"))
   (:file "low"
    :depends-on ("packages"
		 "utilities"
		 "system"))
   (:file "assembler"
    :depends-on ("packages"))
   (:file "machine"
    :depends-on ("packages"
   		 "utilities"
   		 "heap"
   		 "low"
   		 "assembler"))
   (:file "compiler"
    :depends-on ("packages"
   		 "utilities"
   		 "heap"
   		 "low"
   		 "machine"))
   ))

