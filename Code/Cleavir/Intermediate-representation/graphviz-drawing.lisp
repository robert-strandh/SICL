(in-package #:cleavir-ir-graphviz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Drawing a datum on a stream.

(defgeneric draw-datum (datum stream))

;;; During the drawing process, the value of this variable is a hash
;;; table that contains data that have already been drawn. 
(defparameter *datum-table* nil)

(defun datum-id (datum)
  (gethash datum *datum-table*))

(defmethod draw-datum :around (datum stream)
  (when (null (datum-id datum))
    (setf (gethash datum *datum-table*) (gensym))
    (call-next-method)))

