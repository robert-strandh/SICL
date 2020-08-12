(cl:in-package #:sicl-clos)

(defmacro with-existing-slot
    ((object-var slot-name-var slot-var operation) &body body)
  `(let ((,slot-var (find-slot ,object-var ,slot-name-var)))
     (if (null ,slot-var)
         (slot-missing (class-of ,object-var) ,slot-name-var ',operation)
         (progn ,@body))))

(defun slot-value (object slot-name)
  (with-existing-slot (object slot-name slot slot-value)
    (slot-value-using-class (class-of object) class object slot)))

(defun (setf slot-value) (new-value object slot-name)
  (with-existing-slot (object slot-name slot setf)
    (setf (slot-value-using-class (class-of object) object slot) new-value)))

(defun slot-boundp (object slot-name)
  (with-existing-slot (object slot-name slot slot-boundp)
    (slot-boundp-using-class (class-of object) object slot)))

(defun slot-makunbound (object slot-name)
  (with-existing-slot (object slot-name slot slot-makunbound)
    (slot-makunbound-using-class (class-of object) object slot)))
