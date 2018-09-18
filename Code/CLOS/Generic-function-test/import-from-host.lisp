(cl:in-package #:sicl-clos)

(setf (find-class 'funcallable-standard-class)
      (find-class 'closer-mop:funcallable-standard-class))

(setf (find-class 'metaobject)
      (find-class 'closer-mop:metaobject))

(setf (find-class 'funcallable-standard-object)
      (find-class 'closer-mop:funcallable-standard-object))

(setf (fdefinition 'set-funcallable-instance-function)
      #'closer-mop:set-funcallable-instance-function)
