(cl:in-package #:sicl-new-boot)

(defclass client (cb:macro-transforming-client
                  eclector.concrete-syntax-tree:cst-client
                  parcl-class:client)
  ((%environment :initarg :environment :reader environment)))
