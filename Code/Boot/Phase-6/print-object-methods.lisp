(cl:in-package #:sicl-boot-phase-6)

(defmethod print-object ((object sicl-boot::host-package) stream)
  (princ "#<HOST-PACKAGE " stream)
  (princ (package-name object) stream)
  (princ ">") stream)

(defun print-symbol (symbol stream)
  (princ (package-name (symbol-package symbol)) stream)
  (princ ":" stream)
  (princ (symbol-name symbol) stream))

(defmethod print-object ((object sicl-boot::host-symbol) stream)
  (princ "#<HOST-SYMBOL " stream)
  (print-symbol object stream)
  (princ ">") stream)
