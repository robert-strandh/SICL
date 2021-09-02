(cl:in-package #:sicl-string)

(deftype string-designator ()
  '(or string symbol character))

(defun string (designator)
  (etypecase designator
    (string designator)
    (character (make-string 1 :initial-element designator))
    (symbol (symbol-name designator))))
