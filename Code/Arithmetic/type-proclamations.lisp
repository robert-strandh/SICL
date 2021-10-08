(cl:in-package #:sicl-arithmetic)

(declaim (ftype (function (number number) number)
                binary-add
                binary-subtract
                binary-multiply
                binary-divide
                binary-equal))

(declaim (ftype (function (real real) boolean)
                binary-less
                binary-not-greater
                binary-greater
                binary-not-less))

(declaim (ftype (function (list) number)
                + - * /))

(declaim (ftype (function (list) boolean)
                < <= > >= = /=))

(declaim (ftype (function (float) integer)
                float-radix))

(declaim (ftype (or (function (short-float) short-float)
                    (function (single-float) single-float)
                    (function (double-float) double-float)
                    (function (long-float) long-float)
                    (function (short-float short-float) short-float)
                    (function (short-float single-float) single-float)
                    (function (short-float double-float) double-float)
                    (function (short-float long-float) long-float)
                    (function (single-float short-float) single-float)
                    (function (single-float single-float) single-float)
                    (function (single-float double-float) double-float)
                    (function (single-float long-float) long-float)
                    (function (double-float short-float) double-float)
                    (function (double-float single-float) double-float)
                    (function (double-float double-float) double-float)
                    (function (double-float long-float) long-float)
                    (function (long-float short-float) long-float)
                    (function (long-float single-float) long-float)
                    (function (long-float double-float) long-float)
                    (function (long-float long-float) long-float))
                float-sign))

(declaim (ftype (function (float) (integer 0))
                float-digits))

(declaim (ftype (function (float) (integer 0))
                float-precision))

(declaim (ftype (function (number &optional (real 0))
                          (values (real 0) real))
                ffloor fceiling ftruncate fround))

(declaim (ftype (or (function (short-float)
                              (values short-float integer (member -1 1)))
                    (function (single-float)
                              (values single-float integer (member -1 1)))
                    (function (double-float)
                              (values double-float integer (member -1 1)))
                    (function (long-float)
                              (values long-float integer (member -1 1))))
                integer-decode-float))

(declaim (ftype (function (number) (real 0.0))
                abs))

(declaim (ftype (function (number) number)
                acos))

(declaim (ftype (function (number) number)
                acosh))

(declaim (ftype (function (integer integer) integer)
                ash))

(declaim (ftype (function (number) number)
                asin
                asinh
                atanh
                cosh
                sinh
                tanh))

(declaim (ftype (or (function (number) number)
                    (function (real real) number))
                atan))
