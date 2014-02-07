(cl:in-package #:sicl-clos)

(loop for entry in *classes*
      do (finalize-inheritance (cdr entry)))
