(cl:in-package #:sicl-clos)

(defclass class (specializer)
  ((%unique-number 
    ;; FIXME: the unique numbers should be assigned during class
    ;; finalization, and not here.
    :initform (prog1 *class-unique-number* (incf *class-unique-number*))
    :reader unique-number)
   (%name 
    :initform nil
    :initarg :name 
    ;; There is a specified function named (SETF CLASS-NAME), but it
    ;; is not an accessor.  Instead it works by calling
    ;; REINITIALIZE-INSTANCE with the new name.
    :reader class-name)
   (%direct-subclasses 
    :initform '() 
    :initarg :direct-subclasses
    :accessor class-direct-subclasses)))
