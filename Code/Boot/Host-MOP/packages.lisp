(cl:in-package #:common-lisp-user)

(defpackage #:sicl-host-mop
  (:use)
  (:import-from
   #+sbcl #:sb-mop
   . #1=(#:class-direct-subclasses
         #:class-direct-superclasses
         #:class-precedence-list
         #:class-prototype
         #:class-slots
         #:finalize-inheritance
         #:forward-referenced-class
         #:funcallable-standard-class
         #:funcallable-standard-object
         #:generic-function-method-class
         #:method-function
         #:set-funcallable-instance-function
         #:slot-definition-name
         #:specializer
         #:standard-direct-slot-definition
         #:standard-reader-method
         #:standard-writer-method
         #:validate-superclass))
  (:export . #1#))
                
