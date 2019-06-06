(cl:in-package #:sicl-boot-phase-3)

;;; The problem we are solving here is that when we do
;;; (CALL-NEXT-METHOD) in the :AROUND method of SHARED-INITIALIZE, we
;;; attempt to take the METHOD-FUNCTION of the next method.  But
;;; METHOD-FUNCTION is a SICL function in E3 and it doesn't have any
;;; method for the host class STANDARD-METHOD.  We solve this problem
;;; by adding such a method.
(defun define-method-on-method-function (e3)
  (let ((temp (gensym)))
    (setf (fdefinition temp)
          (sicl-genv:fdefinition 'sicl-clos:method-function e3))
    (eval `(defmethod ,temp ((generic-function standard-method))
             (closer-mop:method-function generic-function)))
    (fmakunbound temp)))
