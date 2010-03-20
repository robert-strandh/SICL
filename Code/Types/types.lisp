;;; Used for function arguments that apply a key function
;;; before comparing two objects.
(deftype keyfun () (function (t) t))

;;; Used for function arguments that test the equality 
;;; between to keys. 
(deftype testfun () (function (t t) t))

(deftype nonnegative-fixnum () (and fixnum unsigned-byte))

(declaim (ftype (function (&optional (or null condition)) nil)
                abort))

(declaim (ftype (function (number) (real 0.0))
                abs))

(declaim (ftype (function (t t list) list)
                acons))

(declaim (ftype (function (number) number)
                acos))

(declaim (ftype (function (number) number)
                acosh))

(declaim (ftype (function (function method) function)
                add-method))

(declaim (ftype (function (t list
                           &key (key (or keyfun null))
                                (test (or testfun null))
                                (test-not (or testfun null))))
                adjoin))

(declaim (ftype (function (array (or fixnum list)
                           &key (element-type t)
                                (initial-element t)
                                (initial-contents t)
                                (fill-pointer nonnegative-fixnum)
                                (displaced-to (or array null))
                                (displaced-index-offset nonnegative-fixnum))
                          array)
                adjust-array))

(declaim (ftype (function (number) number)
                asin))

(declaim (ftype (function (number) number)
                asinh))

(declaim (ftype (or (function (number) number)
                    (function (real real) number))
                atan))

(declaim (ftype (function (number) number)
                atanh))

(declaim (ftype (function (number) number)
                cosh))

(declaim (ftype (function (number) number)
                sinh))

(declaim (ftype (function (number) number)
                tanh))