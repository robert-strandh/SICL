(cl:in-package #:sicl-clos)

(defun make-class-specializer (class-name)
  (find-class class-name))

(defun make-eql-specializer (object)
  (make-instance 'eql-specializer :object object))
