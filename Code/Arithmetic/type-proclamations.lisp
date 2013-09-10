(cl:in-package #:sicl-arithmetic)

(declaim (ftype (function (number number) number)
		binary-+ binary-- binary-* binary-/))

(declaim (ftype (function (number number) t)
		binary-< binary-<= binary-> binary->=))

(declaim (ftype (function (list) number)
		+ - * /))

(declaim (ftype (function (list) t)
		< <= > >= = /=))
