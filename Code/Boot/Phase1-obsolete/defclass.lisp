(cl:in-package #:sicl-boot-phase1)

(defmacro defclass (name superclasses slots &optional class-options)
  (declare (ignore class-options))
  (let ((supers (remove t superclasses)))
    `(cl:defclass ,name ,supers ,slots)))

