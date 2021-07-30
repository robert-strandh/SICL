(cl:in-package #:sicl-package)

(defgeneric home-package (symbol))

(defgeneric (setf home-package) (new-home-package symbol))
