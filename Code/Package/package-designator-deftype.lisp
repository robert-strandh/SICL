(cl:in-package #:sicl-package)

(deftype package-designator ()
  `(or string-designator package))
