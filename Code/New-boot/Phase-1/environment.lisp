(cl:in-package #:sicl-new-boot-phase-1)

(defclass client (cb:macro-transforming-client
                  eclector.concrete-syntax-tree:cst-client)
  ())

(defun create-environment ()
  (let ((environment (cb:create-environment)))
    environment))
