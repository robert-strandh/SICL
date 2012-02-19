(in-package #:sicl-internationalization)

(defclass locale ()
  ((%language :initarg :language :initform 'en-us :reader language)))

(defparameter *locale*
  (make-instance 'locale :language 'en-us))
