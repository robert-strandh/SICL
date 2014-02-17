(cl:in-package #:sicl-clos)

(setf (fdefinition 'initialize-instance)
      (fdefinition 'cl:initialize-instance))

