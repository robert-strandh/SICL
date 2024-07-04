(cl:in-package #:sicl-new-boot)

(defclass client (cb:macro-transforming-client
                  eclector.concrete-syntax-tree:cst-client
                  maclina.vm-cross:client
                  parcl-low-class:client)
  ((%environment :initarg :environment :reader environment)))
