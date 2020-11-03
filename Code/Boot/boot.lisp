(cl:in-package #:sicl-boot)

(defun boot ()
  (let ((boot (make-instance 'boot
                :e0 (setf *e0* (make-instance 'environment :name "E0"))
                :e1 (setf *e1* (make-instance 'environment :name "E1"))
                :e2 (setf *e2* (make-instance 'environment :name "E2"))
                :e3 (setf *e3* (make-instance 'environment :name "E3"))
                :e4 (setf *e4* (make-instance 'environment :name "E4"))
                :e5 (setf *e5* (make-instance 'environment :name "E5"))
                :e6 (setf *e6* (make-instance 'environment :name "E6"))
                :e7 (setf *e7* (make-instance 'environment :name "E7")))))
    (uiop:delete-directory-tree
     (asdf:system-relative-pathname '#:sicl-boot "ASTs/")
     :validate t
     :if-does-not-exist :ignore)
    (sicl-boot-phase-0:boot boot)
    (sicl-boot-phase-1:boot boot)
    (sicl-boot-phase-2:boot boot)
    (sicl-boot-phase-4:boot boot)
    (sicl-boot-phase-5:boot boot)
    (sicl-boot-phase-6:boot boot)
    boot))
