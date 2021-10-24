(cl:in-package #:sicl-package)

(defgeneric (setf home-package) (new-home-package symbol))

(defgeneric delete-package (package-designator))
