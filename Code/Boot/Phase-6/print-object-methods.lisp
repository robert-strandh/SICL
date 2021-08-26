(cl:in-package #:sicl-boot-phase-6)

(defmethod print-object ((object sicl-boot::host-package) stream)
  (princ "#<HOST-PACKAGE " stream)
  (princ (package-name object) stream)
  (princ ">") stream)

(defmethod print-object ((object sicl-boot::host-symbol) stream)
  (princ "#<HOST-SYMBOL " stream)
  (princ (package-name (symbol-package object)) stream)
  (princ ":" stream)
  (princ (symbol-name object) stream)
  (princ ">") stream)
