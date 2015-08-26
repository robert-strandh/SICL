(cl:in-package #:asdf-user)

(defsystem :sicl
  :name "sicl"
  :components ((:file "packages")
	       (:file "tags" :depends-on ("packages"))
	       (:file "cons" :depends-on ("tags"))
	       (:file "backends-cl" :depends-on ("packages" "tags"))))
