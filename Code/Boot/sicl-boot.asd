(cl:in-package #:asdf-user)

(defsystem #:sicl-boot
  :depends-on (#:sicl-boot-base
               #:sicl-boot-phase-1
               #:sicl-boot-phase-2
               #:sicl-boot-phase-3
               #:sicl-boot-phase-4
               #:sicl-boot-phase-5
               #:sicl-boot-phase-6
               #:sicl-boot-phase-7
               #:sicl-boot-condition-system)
  :serial t
  :components
  ((:file "boot")
   (:file "trace")))

(defmethod asdf:operate :after
    ((operation asdf/lisp-action:load-op)
     (component (eql (asdf:find-system '#:sicl-boot)))
     &key)
  (unless (null (find-package '#:closer-common-lisp-user))
    (delete-package '#:closer-common-lisp-user))
  (unless (null (find-package '#:closer-common-lisp))
    (delete-package '#:closer-common-lisp))
  (unless (null (find-package '#:closer-mop))
    (rename-package '#:closer-mop (format nil "~a" (random 100000000)))))
