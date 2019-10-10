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

(defun class-of-object (object)
  (slot-value object 'sicl-boot-phase-3::%class))

(defun rack-of-object (object)
  (slot-value object 'sicl-boot-phase-3::%rack))

(defun class-name-of-object (object)
  (let ((class-of-object (class-of-object object)))
    (if (typep class-of-object 'sicl-boot-phase-3::header)
        (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e4 *boot*)) class-of-object)
        (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e3 *boot*)) class-of-object))))

(defmethod clouseau:inspect-object-using-state
    ((object sicl-boot-phase-3::header)
     (state clouseau:inspected-object)
     (style (eql :collapsed))
     (stream t))
  (format stream "~a [~a]" (class-name-of-object object) (object-type-name object)))

(defmethod clouseau:inspect-object-using-state
    ((object sicl-boot-phase-3::header)
     (state clouseau:inspected-object)
     (style (eql :expanded-header))
     (stream t))
  (format stream "~a [~a]" (class-name-of-object object) (object-type-name object)))

(defclass ersatz-instance-slot-place
    (clouseau:key-value-place clouseau:read-only-place)
  ())

(defun rack-element-value (object slot)
  (let ((index (aref (rack-of-object slot) 8)))
    (aref (rack-of-object object) index)))

(defmethod clouseau:value ((place ersatz-instance-slot-place))
  (let ((instance (clouseau:container place))
        (slot (clouseau:cell place)))
    (rack-element-value instance slot)))

(defmethod clouseau:valuep ((place ersatz-instance-slot-place))
  (let ((instance (clouseau:container place))
        (slot (clouseau:cell place)))
    (not (eql (rack-element-value instance slot) 10000000))))

(defmethod clouseau:inspect-object-using-state
    ((object sicl-boot-phase-3::header)
     (state clouseau:inspected-object)
     (style (eql :expanded-body))
     (stream t))
  (let ((slots (aref (rack-of-object object) 1)))
    (clim:formatting-table (stream)
      (loop for slot in slots
            for slot-name = (aref (rack-of-object slot) 2)
            for slot-value = (rack-element-value object slot)
            do (clouseau:formatting-place
                   (object
                    'ersatz-instance-slot-place
                    slot
                    present-place
                    present-object)
                 (clim:formatting-row (stream)
                   (clim:formatting-cell (stream :align-y :center)
                     (clouseau:with-style (stream :slot-like)
                       (format stream "~s" slot-name)))
                   (clim:formatting-cell (stream)
                     (present-place stream))
                   (clim:formatting-cell (stream)
                     (present-object stream))))))))
