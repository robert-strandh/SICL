(cl:in-package #:sicl-clos)

;;; This function is used by ENSURE-METHOD to turn a specializer
;;; designator into a specializer metaobject.  A specializer
;;; designator is either a specializer, denoting itself, or a symbol
;;; denoting a class with that name as indicated by FIND-CLASS.

(defun make-specializer (specializer)
  (let ((environment (sicl-genv:global-environment)))
    (cond ((symbolp specializer)
           (find-metaclass specializer environment))
          ((sicl-genv:typep specializer 'specializer environment)
           specializer)
          (t
           (error "Specializer must be symbol or specializer metaobject: ~s"
                  specializer)))))
