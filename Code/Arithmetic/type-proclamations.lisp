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

(declaim (ftype (function (float) float)
                float-sign))

(declaim (ftype (function (float) (integer 0))
                float-digits))

(declaim (ftype (function (float) (integer 0))
                float-precision))

(declaim (ftype (function (number &optional (real 0))
                          (values integer real))
                floor ceiling truncate round))

(declaim (ftype (function (number &optional (real 0))
                          (values float real))
                ffloor fceiling ftruncate fround))

(declaim (ftype (function (float)
                          (values float integer (member -1 1)))
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

(declaim (ftype (function (number &optional number) number)
                atan))

(declaim (ftype (function (real &optional random-state) real)
                random))

(declaim (ftype (function (number) number) 1+ 1-))

(declaim (ftype (function (number) t) zerop))

(declaim (ftype (function (integer) (member t nil))
                oddp evenp))

(declaim (ftype (function (real) (member t nil))
                plusp minusp))

(declaim (ftype (function (t) (member t nil))
                numberp integerp))

(declaim (ftype (function (number number) number)
                expt))

(declaim (ftype (function (real &rest real) real)
                max min))

(declaim (ftype (function (number &optional number) number)
                log))

(declaim (ftype (function (number) number)
                sqrt))

(declaim (ftype (function (real real) real)
                mod rem))
