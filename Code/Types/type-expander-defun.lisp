(cl:in-package #:sicl-type)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPE-EXPANDER.
;;;
;;; This function is defined the way it is because we want it to be
;;; defined in an environment where we will ultimately undefine
;;; functions with names in the SICL-ENVIRONMENT package.  So the
;;; function SICL-ENVIRONMENT:TYPE-EXPANDER of the environment is
;;; closed over at load time so that it can be undefined after this
;;; definition has been loaded.

;;; By not closing over the CLIENT object, we make it possible to have
;;; a different client when this definition is loaded and when it is
;;; used.  I am not sure whether this is a good idea or not.

(symbol-macrolet ((client sicl-client:*client*))
  (let* ((environment (env:global-environment))
         (type-expander-function
           (env:fdefinition
            client environment 'env:type-expander)))
    (defun type-expander (type-descriptor)
      (funcall type-expander-function client environment type-descriptor))))
