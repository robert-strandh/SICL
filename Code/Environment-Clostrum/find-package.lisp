(cl:in-package #:sicl-environment)

(defun find-package (package-or-name)
  (if (packagep package-or-name)
      package-or-name
      (let ((name (string package-or-name)))
        (clo:find-package *client* *environment* name))))

(defun (setf find-package) (package name-designator)
  (let ((name (string name-designator)))
    (setf (clo:find-package *client* *environment* name)
          package)))
