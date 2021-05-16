(cl:in-package #:sicl-boot)

(defclass client (sicl-client:sicl)
  ((%environment :initarg :environment :reader environment)))
