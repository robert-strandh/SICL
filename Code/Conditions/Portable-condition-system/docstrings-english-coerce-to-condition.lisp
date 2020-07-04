(cl:in-package #:portable-condition-system)

(setf (documentation 'coerce-to-condition 'function)
      #.(format
         nil
         "Attempts to coerce the provided arguments into a condition
object. The DEFAULT-TYPE argument describes the default condition type that
should be created if no condition type can be inferred from DATUM; the NAME
argument is the name of the coercing operator and is used during invalid
coercions to properly report the error."))

(setf (documentation
       (find-method #'coerce-to-condition '()
                    (list (find-class 'condition)
                          (find-class 't)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Returns the condition object that was passed to the function.~@
          If arguments are non-NIL, signals a continuable error."))

(setf (documentation
       (find-method #'coerce-to-condition '()
                    (list (find-class 'symbol)
                          (find-class 't)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Calls MAKE-CONDITION on the provided symbol and arguments."))

(setf (documentation
       (find-method #'coerce-to-condition '()
                    (list (find-class 'string)
                          (find-class 't)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Makes a SIMPLE-CONDITION of the provided DEFAULT-TYPE,~@
          using the DATUM string as its format control and ARGUMENTS~@
          as its format arguments."))

(setf (documentation
       (find-method #'coerce-to-condition '()
                    (list (find-class 'function)
                          (find-class 't)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Makes a SIMPLE-CONDITION of the provided DEFAULT-TYPE,~@
          using the DATUM function as its format control and ARGUMENTS~@
          as format arguments."))

(setf (documentation
       (find-method #'coerce-to-condition '()
                    (list (find-class 't)
                          (find-class 't)
                          (find-class 't)
                          (find-class 't)))
       t)
      #.(format
         nil
         "Signals an error that the provided datum is not coercable~@
          to a condition object."))
