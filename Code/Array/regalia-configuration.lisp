(cl:in-package #:sicl-array)

(defun t-aref (array index)
  (sicl-primop:primop :t-aref array index))

(defun (setf t-aref) (value array index)
  (sicl-primop:primop :setf-t-aref value array index))
