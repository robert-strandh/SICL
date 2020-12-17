(cl:in-package #:sicl-boot)

(defclass host-symbol (symbol) ())

(defmethod symbol-name ((symbol host-symbol))
  (host-symbol-name symbol))

(defclass host-null (null) ())

(defmethod symbol-name ((symbol host-null))
  (host-symbol-name symbol))
