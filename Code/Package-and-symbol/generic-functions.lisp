(cl:in-package #:sicl-package)

(defgeneric home-package (symbol))

(defgeneric (setf home-package) (new-home-package symbol))

(defgeneric find-package (name))

(defgeneric (setf find-package) (package name))
