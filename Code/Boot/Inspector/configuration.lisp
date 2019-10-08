(cl:in-package #:sicl-boot-inspector)

(defmethod clouseau:inspect-object-using-state
    ((object sicl-simple-environment::function-entry)
     (state clouseau:inspected-object)
     (style (eql :collapsed))
     (stream t))
  (cond ((not (null (sicl-simple-environment::macro-function object)))
         (format stream "MACRO"))
        ((not (null (sicl-simple-environment::special-operator object)))
         (format stream "SPECIAL OPERATOR"))
        ((eq (car (sicl-simple-environment::function-cell object))
          (sicl-simple-environment::unbound object))
         (format stream "UNBOUND"))
        (t (format stream "FUNCTION"))))

(defun object-type (object)
  (let ((class-of-object (slot-value object 'sicl-boot-phase-3::%class)))
    (if (typep class-of-object 'sicl-boot-phase-3::header)
        'pure-ersatz 'impure-ersatz)))

(defun object-type-name (object)
  (ecase (object-type object)
    (pure-ersatz "Ersatz (pure)")
    (impure-ersatz "Ersatz (impure)")))

(defun class-name-of-object (object)
  (let ((class-of-object (slot-value object 'sicl-boot-phase-3::%class)))
    (if (typep class-of-object 'sicl-boot-phase-3::header)
        (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e4 *boot*)) class-of-object)
        (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e3 *boot*)) class-of-object))))

(defmethod clouseau:inspect-object-using-state
    ((object sicl-boot-phase-3::header)
     (state clouseau:inspected-object)
     (style (eql :collapsed))
     (stream t))
  (format stream "~a [~a]" (class-name-of-object object) (object-type-name object)))
