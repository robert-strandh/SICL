(cl:in-package #:sicl-symbol)

(proclaim '(ftype (function (&optional (or string (integer 0))) symbol)
            gensym))

(proclaim '(ftype (function (t) t)
            symbolp))
