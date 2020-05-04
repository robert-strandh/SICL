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

(defun object-purity (object)
  (if (typep object 'sicl-boot::header)
      (let ((class-of-object (slot-value object 'sicl-boot::%class)))
        (if (typep class-of-object 'sicl-boot::header)
            (let ((class-of-class-of-object
                    (slot-value class-of-object 'sicl-boot::%class)))
              (if (typep class-of-class-of-object 'sicl-boot::header)
                  4
                  3))
            2))
      (if (eq (class-of (class-of object))
              (find-class 'sicl-boot-phase-1::funcallable-standard-class))
          1
          0)))

(defun object-purity-name (object)
  (ecase (object-purity object)
    (4 "Ersatz (very pure)")
    (3 "Ersatz (pure)")
    (2 "Ersatz (impure)")
    (1 "Bridge")
    (0 "Host")))

(defmacro with-purity-ink ((object stream) &body body)
  (let ((ink-var (gensym)))
    `(let ((,ink-var (ecase (object-purity ,object)
                       (0 clim:+black+)
                       (1 clim:+green+)
                       (2 clim:+red+)
                       (3 clim:+blue+)
                       (4 clim:+yellow+))))
       (clim:with-drawing-options (,stream :ink ,ink-var)
         ,@body))))

(defun class-of-object (object)
  (slot-value object 'sicl-boot::%class))

(defun rack-of-object (object)
  (slot-value object 'sicl-boot::%rack))

(defun class-name-of-object (object)
  (let ((class-of-object (class-of-object object)))
    (if (typep class-of-object 'sicl-boot::header)
        (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e4 *boot*))
                 class-of-object)
        (funcall (sicl-genv:fdefinition 'class-name (sicl-boot::e3 *boot*))
                 class-of-object))))

(defmethod clouseau:inspect-object-using-state
    ((object sicl-boot::header)
     (state clouseau:inspected-object)
     (style (eql :collapsed))
     (stream t))
  (format stream "~a [~a]" (class-name-of-object object) (object-purity-name object))
  (when (member (class-name-of-object object)
                '(standard-class built-in-class sicl-clos:funcallable-standard-class))
    (with-purity-ink (object stream)
      (format stream " ~a" (aref (rack-of-object object) 5)))))

(defmethod clouseau:inspect-object-using-state
    ((object sicl-boot::header)
     (state clouseau:inspected-object)
     (style (eql :expanded-header))
     (stream t))
  (format stream "~a [~a]" (class-name-of-object object) (object-purity-name object))
  (when (member (class-name-of-object object)
                '(standard-class built-in-class sicl-clos:funcallable-standard-class))
    (format stream " ~a" (aref (rack-of-object object) 5))))

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

(defun inspect-very-pure-object (object stream)
  (let ((slots (aref (rack-of-object object) 1)))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :center :align-y :center)
          (with-purity-ink (object stream)
            (format stream "Very pure object"))))
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :center :align-y :center)
          (format stream "Class:"))
        (clim:formatting-cell (stream :align-x :center :align-y :center)
          (format stream "~s" (class-of-object object))))
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

(defun inspect-pure-object (object stream)
  (let ((slots (aref (rack-of-object object) 1)))
    (clim:formatting-table (stream)
      (clim:formatting-row (stream)
        (clim:formatting-cell (stream :align-x :center :align-y :center)
          (with-purity-ink (object stream)
            (format stream "Pure object"))))
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

(defun inspect-impure-object (object stream)
  (with-purity-ink (object stream)
    (format stream "Impure object")))

(defmethod clouseau:inspect-object-using-state
    ((object sicl-boot::header)
     (state clouseau:inspected-object)
     (style (eql :expanded-body))
     (stream t))
  (ecase (object-purity object)
    (3 (inspect-very-pure-object object stream))
    (2 (inspect-pure-object object stream))
    (1 (inspect-impure-object object stream))))
