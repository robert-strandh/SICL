(cl:in-package #:sicl-loop-test)

(defmacro loop (&rest forms)
  (sicl-loop::expand-body forms))
