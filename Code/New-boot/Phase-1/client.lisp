(cl:in-package #:sicl-new-boot-phase-1)

(defclass client (cb:macro-transforming-client
                  eclector.concrete-syntax-tree:cst-client
                  parcl-class:client)
  ())
