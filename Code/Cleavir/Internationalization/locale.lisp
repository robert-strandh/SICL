(cl:in-package #:cleavir-internationalization)

(defclass locale ()
  ((%language :initarg :language :accessor language)))

(defvar *locale*)
