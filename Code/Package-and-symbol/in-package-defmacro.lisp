(cl:in-package #:sicl-package)

(defmacro in-package (string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setq *package* (find-package ',string-designator))))
