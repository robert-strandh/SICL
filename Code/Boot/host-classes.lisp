(cl:in-package #:sicl-boot)

(defclass host-symbol (symbol) ())

(defmethod symbol-name ((symbol host-symbol))
  (host-symbol-name symbol))

(defmethod symbol-package ((symbol host-symbol))
  (host-symbol-package symbol))

(defclass host-null (null) ())

(defmethod symbol-name ((symbol host-null))
  (host-symbol-name symbol))

(defclass host-string (string) ())

(defmethod array-dimensions ((string host-string))
  (host-array-dimensions string))

(defclass host-package (package) ())

(defgeneric package-name (package))

(defmethod package-name ((package host-package))
  (host-package-name package))
