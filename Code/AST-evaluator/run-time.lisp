(cl:in-package #:sicl-ast-evaluator)

(defparameter *dynamic-environment* '())

(defclass special-variable-entry ()
  ((%name :initarg :name :reader name)
   (%value :initarg :value :accessor value)))

(defun symbol-value (name global-environment)
  (loop for entry in *dynamic-environment*
        when (and (typep entry 'special-variable-entry)
                  (eq name (name entry)))
          return (value entry)
        ;; FIXME: make sure it is bound in the global environment
        finally (return (env:special-variable name global-environment))))

(defun (setf symbol-value) (value name global-environment)
  (loop for entry in *dynamic-environment*
        when (and (typep entry 'special-variable-entry)
                  (eq name (name entry)))
          do (setf (value entry) value)
          and return value
        finally (setf (env:special-variable name global-environment t)
                      value)
                (return value)))

(defmacro with-variable-bound ((name-form value-form) &body body)
  `(let ((*dynamic-environment*
           (cons (make-instance 'special-variable-entry
                   :name ,name-form
                   :value ,value-form)
                 *dynamic-environment*)))
     ,@body))

(defclass exit-point-entry ()
  ((%name :initarg :name :reader name)
   (%validp :initform t :accessor validp)))

(defmacro with-exit-point ((name-form) &body body)
  `(let ((*dynamic-environment*
           (cons (make-instance 'exit-point-entry
                   :name ,name-form)
                 *dynamic-environment*)))
     ,@body))
