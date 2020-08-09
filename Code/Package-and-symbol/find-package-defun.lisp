(cl:in-package #:sicl-package)

(defun find-package (name)
  (if (typep name 'package)
      name
      (sicl-genv:find-package (string name) (sicl-genv:global-environment))))
