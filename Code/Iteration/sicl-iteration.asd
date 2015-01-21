(in-package #:asdf-user)

(defsystem :sicl-iteration
  :components
  ((:file "packages" :depends-on ())
   (:file "conditions" :depends-on ("packages"))
   (:file "iteration" :depends-on ("packages" "common"))
   (:file "condition-reporters-en" :depends-on ("packages"
                                                "conditions"))
   (:file "docstrings-en" :depends-on ("packages"))))
