(cl:in-package #:sicl-clos)

(loop for entry in *bridge-generic-functions*
      do (load-call-history (cdr entry)))
