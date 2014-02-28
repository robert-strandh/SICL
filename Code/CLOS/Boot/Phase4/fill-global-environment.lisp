(cl:in-package #:sicl-clos)

(loop for (name . class) in *target-classes*
      do (push class (classes *global-environment*)))

(loop for function in *target-generic-functions*
      do (push function (functions *global-environment*)))
