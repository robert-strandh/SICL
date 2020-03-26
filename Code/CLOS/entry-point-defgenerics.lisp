(cl:in-package #:sicl-clos)

;; Given an instance of the class FUNCALLABLE-STANDARD-OBJECT, return
;; the entry point, i.e., the address of the first instruction of the
;; code to execute when the object is called of that object.
(defgeneric entry-point (funcallable-standard-object))

;; Given the address of the first instruction of some code to execute,
;; and an instance of the class FUNCALLABLE-STANDARD-OBJECT, make that
;; address the entry point of the object.
(defgeneric (setf entry-point) (address funcallable-standard-object))
