(cl:in-package #:portable-condition-system)

(setf (documentation '*break-on-signals* 'variable)
      #.(format
         nil
         "Declares a condition type for which all signaling functions~@
          will call BREAK before actually signaling the condition."))

(setf (documentation '*handler-clusters* 'variable)
      #.(format
         nil
         "A list containing a list of all handler clusters, where a~@
          cluster is a list of handlers established together."))

(setf (documentation 'signal 'function)
      #.(format
         nil
         "Coerces the provided arguments to a condition and signals it,~@
          calling all condition handlers which match the type of the~@
          signaled condition."))

(setf (documentation 'warn 'function)
      #.(format
         nil
         "Coerces the provided arguments to a warning condition,~@
          establishes a MUFFLE-WARNING restart, and signals the condition.~@
          If the condition is not handled and the MUFFLE-WARNING restart~@
          is not invoked, the condition is reported to the error output stream."))

(setf (documentation 'error 'function)
      #.(format
         nil
         "Coerces the provided arguments to an error condition and signals it.~@
          If the condition is not handled, invokes the debugger with that condition."))

(setf (documentation 'cerror 'function)
      #.(format
         nil
         "Coerces the provided arguments to an error condition,~@
          establishes a CONTINUE restart, and signals the condition.~@
          If the condition is not handled, invokes the debugger with~@
          that condition, allowing execution to continue if the CONTINUE~@
          restart is invoked."))
