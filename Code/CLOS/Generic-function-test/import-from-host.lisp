(cl:in-package #:sicl-clos)

(setf (find-class 'funcallable-standard-class)
      (find-class 'closer-mop:funcallable-standard-class))

(setf (find-class 'metaobject)
      (find-class 'closer-mop:metaobject))

(setf (find-class 'eql-specializer)
      (find-class 'closer-mop:eql-specializer))

(setf (find-class 'funcallable-standard-object)
      (find-class 'closer-mop:funcallable-standard-object))

(setf (fdefinition 'set-funcallable-instance-function)
      #'closer-mop:set-funcallable-instance-function)

(setf (fdefinition 'method-specializers)
      #'closer-mop:method-specializers)

(setf (fdefinition 'class-precedence-list)
      #'closer-mop:class-precedence-list)

(setf (fdefinition 'eql-specializer-object)
      #'closer-mop:eql-specializer-object)

(defun classp (object)
  (typep object 'class))

(defun general-instance-p (x)
  (declare (ignore x))
  t)

(defun general-instance-access (instance location)
  (closer-mop:standard-instance-access instance location))
