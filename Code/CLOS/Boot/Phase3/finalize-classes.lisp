(cl:in-package #:sicl-clos)

(loop for entry in *bridge-classes*
      do (finalize-inheritance (cdr entry)))
