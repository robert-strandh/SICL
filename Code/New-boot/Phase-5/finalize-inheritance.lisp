(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

;;;; The main purpose of phase 5 is to turn the objects in environment
;;;; E4 into a cyclic graph.  As a consequence, some classes in E4
;;;; will have instances.  However, most standard classes in E4 have
;;;; not been finalized, and a class can't have instances unless it is
;;;; finalized.  So this function is responsible for finalizing those
;;;; classes in E4 that may have instances.

(defparameter *class-names* '())

(defun initialize-class-names (client)
  (setf *class-names*
        (list 'standard-class
              'built-in-class
              @clostrophilia:forward-referenced-class
              @clostrophilia:funcallable-standard-class
              @clostrophilia:funcallable-standard-object
              'standard-generic-function
              @clostrophilia:standard-method
              @clostrophilia:standard-reader-method
              @clostrophilia:standard-writer-method
              @clostrophilia:standard-direct-slot-definition
              @clostrophilia:standard-effective-slot-definition)))

(defun finalize-inheritance (client e3 e4)
  (let ((find-class 
          (clo:fdefinition client e4 'find-class))
        (class-finalized-p
          (clo:fdefinition client e3 @clostrophilia:class-finalized-p))
        (finalize-inheritance
          (clo:fdefinition client e3 @clostrophilia:finalize-inheritance)))
    (initialize-class-names client)
    (loop for symbol in  *class-names*
          for class = (funcall find-class symbol)
          unless (funcall class-finalized-p class)
            do (format *trace-output* "Name: ~s~%" symbol)
               (funcall finalize-inheritance class))))
