(cl:in-package #:sicl-reader)

(defgeneric fixup (object seen-objects mapping))

(defmethod fixup :around (object seen-objects mapping)
  (unless (gethash object seen-objects)
    (setf (gethash object seen-objects) t)
    (call-next-method)))

(defmethod fixup (object seen-objects mapping)
  (declare (ignore seen-objects mapping))
  nil)

(defmethod fixup ((object cons) seen-objects mapping)
  (multiple-value-bind (value found-p)
      (gethash (car object) mapping)
    (if found-p
        (setf (car object) value)
        (fixup (car object))))
  (multiple-value-bind (value found-p)
      (gethash (cdr object) mapping)
    (if found-p
        (setf (cdr object) value)
        (fixup (cdr object)))))
