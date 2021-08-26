(cl:in-package #:sicl-boot-phase-6)

(defmethod print-object ((object sicl-boot::host-package) stream)
  (princ "#<HOST-PACKAGE ")
  (princ (package-name object))
  (princ ">"))

(defmethod print-object ((object sicl-boot::host-symbol) stream)
  (princ "#<HOST-SYMBOL ")
  (princ (package-name (symbol-package object)))
  (princ ":")
  (princ (symbol-name object))
  (princ ">"))
