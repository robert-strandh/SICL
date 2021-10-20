(cl:in-package #:sicl-package)

(defgeneric home-package (symbol))

(defgeneric (setf home-package) (new-home-package symbol))

(defgeneric make-symbol (name))

(defgeneric delete-package (package-designator))
