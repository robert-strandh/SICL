(cl:in-package #:portable-condition-system)

(setf (documentation 'handler-bind 'function)
      #.(format
         nil
         "Executes the body forms in a dynamic context where the newly~@
          established handlers are available."))

(setf (documentation 'make-handler-case-with-no-error-case 'function)
      #.(format
         nil
         "Generates the HANDLER-CASE body in situation where a :NO-ERROR~@
          case is provided."))

(setf (documentation 'handler-case-parse-case 'function)
      #.(format
         nil
         "Parses and annotates a handler case with a unique go tag."))

(setf (documentation 'handler-case-make-handler-binding 'function)
      #.(format
         nil
         "Accepts an annotated HANDLER-CASE case and generates a~@
          HANDLER-BIND binding based on it."))

(setf (documentation 'handler-case-make-handler-case 'function)
      #.(format
         nil
         "Accepts an annotated HANDLER-CASE case and generates a~@
          TAGBODY case based on it."))

(setf (documentation 'make-handler-case-without-no-error-case 'function)
      #.(format
         nil
         "Generates the HANDLER-CASE body in situation where no :NO-ERROR~@
          case is provided."))

(setf (documentation 'handler-case 'function)
      #.(format
         nil
         "Executes the body forms in a dynamic context where the newly~@
          established handlers are available. Each handler immediately~@
          transfers control to its handler case upon invocation, executing~@
          the body forms of the case and returning their value from HANDLER-CASE."))

(setf (documentation 'ignore-errors 'function)
      #.(format
         nil
         "Executes the body forms in a dynamic context where a newly~@
          established ERROR handler is available. This handler handler~@
          immediately transfers control outside the form upon invocation,~@
          returning NIL as its primary value and the signaled condition~@
          object as its secondary value."))
