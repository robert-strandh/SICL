(cl:in-package #:sicl-run-time)

(proclaim '(ftype (function (symbol cons) t) symbol-value))

(proclaim '(ftype (function (t symbol cons) t) (setf symbol-value)))

(proclaim '(ftype (function (symbol cons) t) makunbound))

(proclaim '(ftype (function (symbol cons) t) boundp))

(proclaim '(ftype (function ((or symbol integer) list) t) throw))
