(in-package #:sicl-compiler-environment)

(proclaim '(ftype (function (datum &rest arguments) nil)
	    error))

(proclaim '(ftype (function (number &rest more-numbers) t)
	    >))

(proclaim '(ftype (function (number &rest more-numbers) t)
	    <))

(proclaim '(ftype (function (number &rest more-numbers) t)
	    >=))

(proclaim '(ftype (function (number &rest more-numbers) t)
	    <=))
