(cl:in-package #:sicl-clos)

;;;; This file contains defintions of ADD-DIRECT-METHOD and
;;;; REMOVE-DIRECT-METHOD as ordinary functions as opposed to generic
;;;; functions.

(defun add-direct-method (specializer method)
  (add-direct-method-default specializer method))

(defun remove-direct-method (specializer method)
  (remove-direct-method-default specializer method))
