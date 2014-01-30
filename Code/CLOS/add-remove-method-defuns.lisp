(cl:in-package #:sicl-clos)

;;;; This file contains defintions of ADD-METHOD and REMOVE-METHOD as
;;;; ordinary functions as opposed to generic functions.  

(defun add-method (generic-function method)
  (add-method-default generic-function method))

(defun remove-method (generic-function method)
  (remove-method-default generic-function method))
