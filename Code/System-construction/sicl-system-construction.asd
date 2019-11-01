(cl:in-package #:asdf-user)

(defsystem #:sicl-system-constructon
  :depdends-on ()
  :serial t
  :components
  ((:file "packages")
   (:file "features-defparameter")
   (:file "modules-defparameter")
   (:file "compile-file-pathname-defparameter")
   (:file "compile-file-truename-defparameter")))
