(cl:in-package #:sicl-run-time)

(proclaim '(ftype (function (symbol cons) t) symbol-value))

(proclaim '(ftype (function (t symbol cons) t) (setf symbol-value)))
