(cl:in-package #:sicl-reader)

(defgeneric fixup (object seen-objects mapping))

(defmethod fixup :around (object seen-objects mapping)
  (unless (gethash object seen-objects)
    (setf (gethash object seen-objects) t)
    (call-next-method)))

(defmethod fixup (object seen-objects mapping)
  (declare (ignore seen-objects mapping))
  nil)
