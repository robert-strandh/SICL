(in-package #:sicl-simple-environment)

(defun typestring (type)
  (ecase type
    ((:function) "a function")
    ((:macro) "a macro")
    ((:special-operator) "a special operator")
    ((:constant) "a constant")
    ((:special-variable) "a special variable")
    ((:symbol-macro) "a symbol macro")))

(defmethod acclimation:report-condition
    ((condition redefinition) stream (language acclimation:english))
  (format stream
          "~s is ~s, and cannot be redefined as ~s"
          (name condition) (typestring (oldtype condition)) (typestring (newtype condition))))

(defmethod acclimation:report-condition
    ((condition constant-redefinition) stream (language acclimation:english))
  (format stream
          "~s is a constant defined as ~s, and cannot be modified to be ~s, an unEQL value"
          (name condition) (old condition) (new condition)))

(defmethod acclimation:report-condition
    ((condition attempt-to-proclaim-type-of-special-operator) stream (language acclimation:english))
  (format stream
          "~s is a special operator, and cannot have its type proclaimed"
          (name condition)))

(defmethod acclimation:report-condition
    ((condition attempt-to-proclaim-type-of-macro) stream (language acclimation:english))
  (format stream
          "~s is a macro, and cannot have its type proclaimed"
          (name condition)))

(defmethod acclimation:report-condition
    ((condition attempt-to-proclaim-type-of-constant) stream (language acclimation:english))
  (format stream
          "~s is a constant, and cannot have its type proclaimed"
          (name condition)))

(defmethod acclimation:report-condition
    ((condition undefined-function) stream (language acclimation:english))
  (format stream
          "The function ~s is undefined in the environment ~s"
          (cell-error-name condition)
          (environment condition)))
