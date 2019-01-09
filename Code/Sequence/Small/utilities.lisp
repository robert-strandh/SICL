(cl:in-package #:sicl-sequence)

(defmacro with-from-end (from-end-var &body body)
  `(if ,from-end-var
       (progn ,@body)
       (progn ,@body)))

(defmacro with-simple (array &body body)
  `(if (typep ,array 'simple-array)
       (progn ,@body)
       (progn ,@body)))

(defmacro with-vector-type (vector-var &body body)
  `(macrolet ((vref (vector index) `(aref ,vector ,index)))
     ,@body))

(defmacro with-element-type (array &body body)
  `(progn ,@body))

;;; This macro is used when the sequence is a list in order to check
;;; that the START and END parameters are valid.
(defmacro with-bounding-indices-list ((start-var end-var) &body body)
  `(progn (verify-bounding-indices-list ,start-var ,end-var)
          ,@body))

;;; This macro is used when the sequence is a vector in order to check
;;; that the START and END parameters are valid.
(defmacro with-bounding-indices-vector ((vector-var start-var end-var) &body body)
  `(progn (verify-bounding-indexes-vector ,vector-var ,start-var ,end-var)
          ,@body))
