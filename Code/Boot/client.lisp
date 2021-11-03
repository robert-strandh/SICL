(cl:in-package #:sicl-boot)

(defclass client (sicl-client:sicl
                  eclector.concrete-syntax-tree:cst-client)
  ((%environment :initarg :environment :reader environment)
   (%macro-environment
    :initarg :macro-environment
    :accessor macro-environment)))
