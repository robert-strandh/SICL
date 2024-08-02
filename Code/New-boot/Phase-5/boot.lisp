(cl:in-package #:sicl-new-boot-phase-5)

(defun boot (boot)
  (format *trace-output* "**************** Phase 5~%")
  (let ((e3 (sb:e3 boot))
        (e4 (sb:e4 boot))
        (client (make-instance 'client)))
    (finalize-inheritance client e3 e4)))
