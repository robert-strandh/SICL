(cl:in-package #:sicl-new-boot-phase-5)

(defun class-is-subclass-of-metaobject-p
    (class metaobject-class class-precedence-list-function)
  (member metaobject-class (funcall class-precedence-list-function class)
          :test #'eq))
