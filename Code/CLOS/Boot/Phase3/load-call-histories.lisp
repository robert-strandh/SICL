(cl:in-package #:sicl-clos)

(loop for entry in *bridge-generic-functions*
      do (satiate-generic-function (cdr entry)))
