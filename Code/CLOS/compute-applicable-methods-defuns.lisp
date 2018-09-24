(cl:in-package #:sicl-clos)

;;;; This file contains defintions of COMPUTE-APPLICABLE-METHODS and
;;;; COMPUTE-APPLICABLE-METHODS-USING-CLASSES as ordinary functions as
;;;; opposed to generic functions.

(defun compute-applicable-methods (generic-function arguments)
  (compute-applicable-methods-default generic-function arguments))

(defun compute-applicable-methods-using-classes
    (generic-function classes-of-arguments)
  (compute-applicable-methods-using-classes-default generic-function
                                                    classes-of-arguments))
