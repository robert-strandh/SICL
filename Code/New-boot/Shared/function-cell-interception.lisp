(cl:in-package #:sicl-new-boot)

;;; This variable contains an association list where the key is the
;;; name of function and the value is a function cell.
(defparameter *intercepted-cells* '())

(defmethod clostrum:ensure-operator-cell :around
    ((client client) environment operator-name)
  (let ((entry (assoc operator-name *intercepted-cells* :test #'equal)))
    (if (null entry)
        (call-next-method)
        (cdr entry))))

(defmethod trucler:describe-function :around
    ((client client) environment operator-name)
  (let ((entry (assoc operator-name *intercepted-cells* :test #'equal)))
    (if (null entry)
        (call-next-method)
        (make-instance 'trucler:global-function-description
          :name operator-name))))

(defmacro with-intercepted-function-cells ((&body clauses) &body body)
  `(let ((*intercepted-cells*
           (append (list ,@(loop for (name cell) in clauses
                                 collect `(cons ',name ,cell)))
                   *intercepted-cells*)))
     ,@body))

;;; This variable contains an association list where the key is the
;;; name of a function, and the value is the alternative name of that
;;; function to be used in place of the name in the key.
(defparameter *intercepted-names* '())

(defmacro with-intercepted-function-names (pairs &body body)
  `(let ((*intercepted-names*
           (append ,pairs *intercepted-names*)))
     ,@body))
