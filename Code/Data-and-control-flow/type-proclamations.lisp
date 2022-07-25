(cl:in-package #:sicl-data-and-control-flow)

(declaim (ftype (function (t) (or t nil)) not))

(declaim (ftype (function (t t) (or t nil))
                eq eql equal))

(declaim (ftype (function (function sequence &rest sequence) t)
                every some notevery notany))

(declaim (ftype (function (&rest t) t) values))
