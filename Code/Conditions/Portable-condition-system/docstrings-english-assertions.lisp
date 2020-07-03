(cl:in-package #:portable-condition-system)

(setf (documentation 'case-failure 'function)
      #.(format
         nil
         "Signals a CASE-FAILURE error using the provided datum,~@
          the name of the case operator, complex type specifier,~@
          and the case keys which were not matched."))

(setf (documentation 'case-transform-t-otherwise-cases 'function)
      #.(format
         nil
         "Transforms T/OTHERWISE cases to prevent them from having~@
          a special effect in CASE."))

(setf (documentation 'case-accumulate-keys 'function)
      #.(format
         nil
         "Collects all keys from the provided cases into a single list."))

(setf (documentation 'ecase 'function)
      #.(format
         nil
         "Evaluates the keyform and checks if it matches any of the keys~@
          in the provided cases. Signals an error otherwise."))

(setf (documentation 'etypecase 'function)
      #.(format
         nil
         "Evaluates the keyform and checks if it is of any of the types~@
          in the provided cases. Signals an error otherwise."))

(setf (documentation 'store-value-read-evaluated-form 'function)
      #.(format
         nil
         "Queries the user for a single form to be evaluated, then~@
          reads and evaluates that form."))

(setf (documentation 'with-store-value-restart 'function)
      #.(format
         nil
         "Evaluates the provided forms in an environment with a freshly~@
          established STORE-VALUE restart. The arguments accepted by~@
          the macro are a pair of places that should be set by the restart~@
          and the TAGBODY tag that control should be transferred to after~@
          setting the value."))

(setf (documentation 'ccase 'function)
      #.(format
         nil
         "Evaluates the keyform (which must be a place) and checks if it~@
          matches any of the keys in the provided cases. Signals a~@
          correctable error otherwise, allowing the programmer to modify~@
          the value stored in the keyform."))

(setf (documentation 'ctypecase 'function)
      #.(format
         nil
         "Evaluates the keyform (which must be a place) and checks if it~@
          is of any of the types in the provided cases. Signals a~@
          correctable error otherwise, allowing the programmer to modify~@
          the value stored in the keyform."))

(setf (documentation 'assert-restart-report 'function)
      #.(format
         nil
         "Reports the restart bound by ASSERT, printing the list of places~@
          supplied to ASSERT, if any."))

(setf (documentation 'assert-prompt 'function)
      #.(format
         nil
         "Queries the programmer whether they would like to modify the~@
          value of a given place. If not, returns the old value; if yes,~@
          queries the programmer for an expression to evaluate and returns~@
          its value.~@
          ~@
          If the old place is a symbol naming a variable, it is dynamically~@
          bound by PROGV to its old value, so the programmer can use it in~@
          the newly evaluated expression."))

(setf (documentation 'assert 'function)
      #.(format
         nil
         "Evaluates TEST-FORM and checks if it is true; otherwise, signals a~@
          correctable error that allows the programmer to retry the assertion.~@
          If any places are supplied, the CONTINUE restart allows the programmer~@
           to set their values before retrying the assertion. The optional~@
           arguments DATUM and ARGUMENTS, if supplied, are used to report the~@
           assertion error."))

(setf (documentation 'check-type-error 'function)
      #.(format
         nil
         "Instantiates an error object suitable to signal within CHECK-TYPE,~@
           using the provided place, value, and expected type or provided~@
           type string."))

(setf (documentation 'check-type 'function)
      #.(format
         nil
         "Evaluates PLACE and checks if its value is of the provided type.~@
           Otherwise, signals a correctable error with an established~@
           STORE-VALUE restart that allows the programmer to provide a~@
           new value for the place before the typecheck is retried.~@
           The optional TYPE-STRING argument is used to construct the~@
           report for the signaled error."))
