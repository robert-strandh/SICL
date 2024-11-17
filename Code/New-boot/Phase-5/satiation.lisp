(cl:in-package #:sicl-new-boot-phase-5)

(defun class-is-subclass-of-metaobject-p
    (class metaobject-class class-precedence-list-function)
  (member metaobject-class (funcall class-precedence-list-function class)
          :test #'eq))

(defun generic-function-is-a-slot-reader-p
    (generic-function
     generic-function-methods-function
     class-of-function
     standard-slot-reader-class)
  (let ((methods
          (funcall generic-function-methods-function generic-function)))
    (and (= (length methods) 1)
         (eq (funcall class-of-function (first methods))
             standard-slot-reader-class))))
