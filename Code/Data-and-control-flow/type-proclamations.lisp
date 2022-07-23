(cl:in-package #:sicl-data-and-control-flow)

(proclaim '(ftype (function (t) (or t nil)) not))

(proclaim '(ftype (function (t t) (or t nil))
            eq eql equal))

(proclaim '(ftype (function (function sequence &rest sequence) t)
            every some notevery notany))
