(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class METHOD-COMBINATION.

(defgeneric variant-signature (method-combination))

(defclass method-combination (metaobject)
  ((%template :initarg :template :reader template)
   (%variant-signature :initarg :variant-signature :reader variant-signature)))
