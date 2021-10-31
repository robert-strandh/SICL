(cl:in-package #:sicl-describe)

(defmethod describe-object :around (object stream)
  (call-next-method)
  (format stream "~%")
  (values))

(defmethod describe-object (object stream)
  (format stream "Object of unknown type."))

(defmethod describe-object ((object standard-object) stream)
  (format stream "A standard object with the following slots:~%")
  (loop for slot in (sicl-clos:class-slots (class-of object))
        for name = (sicl-clos:slot-definition-name slot)
        do (format stream "~s~%" name)))

(defmethod describe-object ((object standard-class) stream)
  (format stream "A standard class~%")
  (format stream "Name: ~s~%" (class-name object)))

(defmethod describe-object ((object standard-generic-function) stream)
  (format stream "A standard generic function~%")
  (format stream "Name: ~s~%" (sicl-clos:generic-function-name object))
  (format stream
          "Lambda list: ~s~%"
          (sicl-clos:generic-function-lambda-list object))
  (format stream
          "Argument precedence order: ~s~%"
          (sicl-clos:generic-function-argument-precedence-order object)))

(defmethod describe-object ((object fixnum) stream)
  (format stream "A fixnum."))

(defmethod describe-object ((object character) stream)
  (format stream "A character."))

;;; FIXME: say "proper list", "dotted list" and "circular list", and
;;; state the number of CONS cells.
(defmethod describe-object ((object cons) stream)
  (format stream "A CONS cell."))

(defmethod describe-object ((object string) stream)
  (format stream "A string of length ~s~%"
          (length object)))

(defmethod describe-object ((object vector) stream)
  (format stream "A vector of length ~s~%"
          (length object)))

(defmethod describe-object ((object array) stream)
  (format stream "An array with dimensions: ~s~%"
          (array-dimensions object)))
