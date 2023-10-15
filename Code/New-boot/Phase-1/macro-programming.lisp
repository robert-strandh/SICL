(cl:in-package #:sicl-new-boot-phase-1)

(defmethod cmd:defclass-compile-time-action
  ((client client)
   name
   superclass-names
   metaclass-name
   environment)
  (format *trace-output*
          "compile-time: ~s ~s ~s~%"
          name
          superclass-names
          metaclass-name))

(defmethod cmd:ensure-class
    ((client client)
     name
     superclass-names
     direct-slot-specs
     options
     environment)
  (format *trace-output*
          "run time: ~s ~s ~s ~s ~s~%"
          name
          superclass-names
          direct-slot-specs
          options
          environment))
