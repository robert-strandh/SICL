(cl:in-package #:sicl-clos)

(defparameter *eql-specializers* '())

(defun intern-eql-specializer (object)
  (let ((entry (assoc object *eql-specializers*)))
    (if (null entry)
        (let ((eql-specializer (make-instance 'eql-specializer
                                 :object object)))
          (push (cons object eql-specializer) *eql-specializers*)
          eql-specializer)
        (cdr entry))))

;;; This function is not required by the standard.
;;;
;;; GENERIC-FUNCTION-OR-NAME is either a generic-function metaobject,
;;; or the name of a generic function.  If it is a name, then we call
;;; ENSURE-GENERIC-FUNCTION to turn it into a generic function.

(defun ensure-method
    (generic-function-or-name
     &rest initargs
     &key specializer-designators
     &allow-other-keys)
  (let ((generic-function
          (if (or (symbolp generic-function-or-name)
                  (consp  generic-function-or-name))
              (ensure-generic-function generic-function-or-name)
              generic-function-or-name))
        (specializers
          (loop for specializer-designator in specializer-designators
                collect (cond ((consp specializer-designator)
                               (intern-eql-specializer
                                (second specializer-designator)))
                              ((symbolp specializer-designator)
                               (find-class specializer-designator))
                              (t specializer-designator)))))
    (apply #'^ensure-method-using-generic-function
           generic-function :specializers specializers initargs)))

;;  LocalWords:  specializer specializers SPECIALIZERS designators
;;  LocalWords:  designator metaobject
