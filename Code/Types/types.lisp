;;; Used for function arguments that apply a key function
;;; before comparing two objects.
(deftype keyfun () (function (t) t))

;;; Used for function arguments that test the equality 
;;; between to keys. 
(deftype testfun () (function (t t) t))

(deftype nonnegative-fixnum () (and fixnum unsigned-byte))

(deftype function-designator () (or function symbol))

(deftype string-designator () (or character symbol string))

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

(declaim (ftype (function (array) t)
                adjustable-array-p))

(declaim (ftype (function (class &rest list &key &allow-other-keys) t)
                allocate-instance))

(declaim (ftype (function (character) t)
                alpha-char-p))

(declaim (ftype (function (character) t)
                alphanumericp))

(declaim (ftype (function (&rest list) t)
                append))

(declaim (ftype (function (function-designator &rest args) t)
                apply))

(declaim (ftype (function (string-designator
                           &optional package-designator)
                          nil)
                apropos))

(declaim (ftype (function (string-designator
                           &optional package-designator)
                          list)
                apropos-list))
(declaim (ftype (function (array &rest list) t)
                aref))

(declaim (ftype (function (array (integer 0)) (integer 0))
                array-dimension))

(declaim (ftype (function (array) list)
                array-dimensions))

(declaim (ftype (function (array)
                          (values (or array nil)
                                  (and fixnum (integer 0))))
                array-displacement))

(declaim (ftype (function (array)
                          (or symbol list class))
                array-element-type))

(declaim (ftype (function (array) t)
                array-has-fill-pointer-p))

(declaim (ftype (function (array &rest list) t)
                (array-in-bounds-p)))

(declaim (ftype (function (array) (integer 0))
                array-rank))

(declaim (ftype (function (array &rest list) (and fixnum (integer 0)))
                array-row-major-index))

(declaim (ftype (function (array) (integer 0))
                array-total-size))

(declaim (ftype (function (array) t)
                arrayp))

(declaim (ftype (function (integer integer) integer)
                ash))
         
(declaim (ftype (function (t list
                           &key (key (or function-designator nil))
                                (test function-designator)
                                (test-not function-designator))
                          (or cons nil))
                assoc))

(declaim (ftype (function (function-designator list
                           &key (key (or function-designator nil))
                                (test function-designator)
                                (test-not function-designator))
                          (or cons nil))
                assoc-if
                assoc-if-not))

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

(declaim (ftype (function (t) t)
                atom))

         