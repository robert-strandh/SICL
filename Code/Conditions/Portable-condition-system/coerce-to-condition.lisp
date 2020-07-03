;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; COERCE-TO-CONDITION

(defgeneric coerce-to-condition (datum arguments default-type name)
  (:documentation "Attempts to coerce the provided arguments into a condition
object. The DEFAULT-TYPE argument describes the default condition type that
should be created if no condition type can be inferred from DATUM; the NAME
argument is the name of the coercing operator and is used during invalid
coercions to properly report the error."))

(defmethod coerce-to-condition ((datum condition) arguments default-type name)
  "Returns the condition object that was passed to the function. If arguments
are non-NIL, signals a continuable error."
  (when arguments
    (cerror "Ignore the additional arguments."
            'simple-type-error
            :datum arguments
            :expected-type 'null
            :format-control
            "You may not supply additional arguments when giving ~S to ~S."
            :format-arguments (list datum name)))
  datum)

(defmethod coerce-to-condition ((datum symbol) arguments default-type name)
  "Calls MAKE-CONDITION on the provided symbol and arguments."
  (apply #'make-condition datum arguments))

(defmethod coerce-to-condition ((datum string) arguments default-type name)
  "Makes a SIMPLE-CONDITION of the provided DEFAULT-TYPE, using the DATUM string
as its format control and ARGUMENTS as its format arguments."
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition ((datum function) arguments default-type name)
  "Makes a SIMPLE-CONDITION of the provided DEFAULT-TYPE, using the DATUM
function as its format control and ARGUMENTS as format arguments."
  (make-condition default-type
                  :format-control datum
                  :format-arguments arguments))

(defmethod coerce-to-condition (datum arguments default-type name)
  "Signals an error that the provided datum is not coercable to a condition
object."
  (error 'simple-type-error
         :datum datum
         :expected-type '(or condition symbol function string)
         :format-control "~S is not coercable to a condition."
         :format-arguments (list datum)))
