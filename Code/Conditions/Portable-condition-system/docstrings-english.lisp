(cl:in-package #:portable-condition-system)

(setf (documentation
       (find-method #'print-object '(:around) (list (find-class 'restart) (find-class t)))
       t)
      #.(format
         nil
         "Print the restart to the provided stream.~@
          ~@
          If *PRINT-ESCAPE* is TRUE, the restart is printed unreadably.~@
          Otherwise, it is reported by means of calling the next printing method."))

(setf (documentation
       (find-method #'print-object '() (list (find-class 'restart) (find-class t)))
       t)
      #.(format
         nil
         "Report a restart object to the provided stream.~@
         ~@
         If the report function of the restart is present, then it is called.~@
         Otherwise, if the name of the restart is present, then it is printed.~@
         Otherwise, the restart object is printed unreadably."))

(setf (documentation 'restart-visible-p 'function)
      #.(format
         nil
         "Return true if the restart should be visible when computed for a given~@
          condition object. If the condition is null, return true; otherwise, if the~@
          restart is associated with a different condition, return false; otherwise,~@
          return true."))

