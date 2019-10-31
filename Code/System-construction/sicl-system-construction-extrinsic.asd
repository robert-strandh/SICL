(cl:in-package #:asdf-user)

(defsystem #:sicl-system-construction-extrinsic
  :depends-on ()
  :serial t
  :components
  ((:file "packages-extrinsic")
   (:file "features-defparameter")
   (:file "modules-defparameter")
   (:file "compile-file-pathname-defparameter")))
