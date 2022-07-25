(cl:in-package #:sicl-run-time)

(declaim (ftype (function (symbol cons) t) symbol-value))

(declaim (ftype (function (t symbol cons) t) (setf symbol-value)))

(declaim (ftype (function (symbol cons) t) makunbound))

(declaim (ftype (function (symbol cons) t) boundp))

(declaim (ftype (function ((or symbol integer) list) t) throw))
