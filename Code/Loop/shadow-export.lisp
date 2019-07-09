(cl:in-package #:sicl-loop)

(defparameter *symbols* '(#:loop #:loop-finish))

(cl:loop
   with package = (find-package '#:sicl-loop)
   for symbol in *symbols*
   do (shadow (symbol-name symbol))
      (export (find-symbol (symbol-name symbol) package)))

