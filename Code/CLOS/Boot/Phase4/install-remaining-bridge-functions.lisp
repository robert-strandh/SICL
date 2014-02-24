(cl:in-package #:sicl-clos)

(loop for (name . fun) in *bridge-generic-functions*
      do (setf (fdefinition name) fun))
