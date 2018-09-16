(cl:in-package #:sicl-clos)

;;;; This file contains the support code for the generic function
;;;; COMPUTE-EFFECTIVE-METHOD.
;;;;
;;;; In this file, there are no definitions of generic functions, nor
;;;; of any methods.  
;;;;
;;;; FIXME: remove this file when we have method combinations work.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

(defun primary-method-p (method)
  (null (method-qualifiers method)))

(defun after-method-p (method)
  (equal (method-qualifiers method) '(:after)))

(defun before-method-p (method)
  (equal (method-qualifiers method) '(:before)))

(defun around-method-p (method)
  (equal (method-qualifiers method) '(:around)))
