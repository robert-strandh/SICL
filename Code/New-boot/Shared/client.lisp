(cl:in-package #:sicl-new-boot)

(defclass client (cb:macro-transforming-client
                  eclector.concrete-syntax-tree:cst-client
                  parcl-low-class:client
                  cbe:client)
  ((%environment :initarg :environment :reader environment)))
