(cl:in-package #:sicl-clos)

;;; This function is used by ENSURE-METHOD to turn a specializer
;;; designator into a specializer metaobject.  A specializer
;;; designator is either a specializer, denoting itself, or a symbol
;;; denoting a class with that name as indicated by FIND-CLASS.

;;; ENVIRONMENT is an environment that is used both to look up the
;;; class in case a symbol is given, and to look up the type
;;; SPECIALIZER in case something other than a symbol is given.

;;; ENVIRONMENT may be a either a lexical environment or a global
;;; environment, so the functions FIND-CLASS and TYPEP must find the
;;; global environment to look up these entities.

(defun make-specializer (specializer environment)
  (cond ((symbolp specializer)
         (sicl-genv:find-class specializer environment))
        ((sicl-genv:typep specializer 'specializer environment)
         specializer)
        (t
         (error "Specializer must be symbol or specializer metaobject: ~s"
                specializer))))
