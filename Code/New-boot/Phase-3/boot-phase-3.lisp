(cl:in-package #:sicl-new-boot-phase-3)

(defclass environment (sicl-minimal-extrinsic-environment:environment)
  ())

(defun boot-phase-3 (boot)
  (format *trace-output* "Start of phase 3~%")
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (change-class e3 'environment)
    (import-functions-from-host
     '(find reverse last remove-duplicates reduce mapcar union find-if-not
       eql count)
     e2)))
