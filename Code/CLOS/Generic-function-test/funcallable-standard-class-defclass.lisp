(cl:in-package #:sicl-clos)

(setf (find-class 'funcallable-standard-class)
      (find-class 'closer-mop:funcallable-standard-class))
