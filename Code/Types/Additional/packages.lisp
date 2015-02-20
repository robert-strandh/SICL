(cl:in-package #:common-lisp-user)

(defpackage #:sicl-additional-types
  (:use #:common-lisp)
  (:export
   #:proper-list #:dotted-list #:circular-list
   #:association-list #:property-list
   #:generalized-boolean
   #:symbol-predicate
   #:character-designator
   #:string-designator
   #:pathname-designator
   #:package-designator
   #:setf-function-name
   #:function-name
   #:function-designator
   #:extended-function-designator
   #:keyfun #:keyfun-designator
   #:testfun1 #:testfun1-designator
   #:testfun2 #:testfun2-designator
   #:radix
   #:nonnegative-fixnum ; Maybe remove this one?
   #:byte-specifier
   #:bounding-indexes
   ))
