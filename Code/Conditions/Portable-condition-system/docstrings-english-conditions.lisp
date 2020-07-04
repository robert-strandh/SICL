(in-package #:portable-condition-system)

(setf (documentation 'condition 'type)
      #.(format
         nil
         "The base condition type that is the supertype of all~@
          condition objects."))

(setf (documentation
       (find-method #'print-object '() (list (find-class 'condition) (find-class 't)))
       t)
      #.(format
         nil
         "Default condition reporting method which prints the condition type."))

(setf (documentation
       (find-method #'print-object '(:around) (list (find-class 'condition) (find-class 't)))
       t)
      #.(format
         nil
         "Prints of reports a condition to the provided stream. If *PRINT-ESCAPE* is~@
          bound, the condition is print unreadably; otherwise, it is reported by means~@
          of calling the next printing method."))
  
(setf (documentation 'make-condition 'function)
      #.(format
         nil
         "Instantiates a new condition object of the provided type with the~@
          provided arguments."))

(setf (documentation 'expand-define-condition-report-method 'function)
      #.(format
         nil
         "Accepts the name of the condition being defined and the report option~@
          provided to DEFINE-CONDITION, and returns a DEFMETHOD PRINT-OBJECT form~@
          meant to be spliced into the generated DEFINE-CONDITION expansion."))  

(setf (documentation 'expand-define-condition-remove-report-method 'function)
      #.(format
         nil
         "Accepts the method name and expands into a form which removes any~@
          PRINT-OBJECT method defined on the class named by NAME."))

(setf (documentation 'expand-define-condition 'function)
      #.(format
         nil
         "Defines a new condition type via DEFCLASS, handling the :REPORT options~@
          via defining a PRINT-object method on the newly created class."))
  
(setf (documentation 'define-condition 'function)
      #.(format
         nil
         "Defines or redefines a condition type."))
