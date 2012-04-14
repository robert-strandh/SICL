(declaim (ftype symbol-predicate name-of-length-1))

(deftype character-designator ()
  '(or character (string 1) (and symbol (satisfies name-of-length-1))))

;;; Used for function arguments that apply a key function
;;; before comparing two objects.
(deftype keyfun () '(function (t) t))

(deftype keyfun-designator () '(or keyfun symbol))

;;; Used for one-argument tests
(deftype testfun1 () '(function (t) generalized-boolean))

(deftype testfun1-designator () '(or testfun1 symbol))

;;; Used for two-argument tests
(deftype testfun2 () (function (t t) 'generalized-boolean))

(deftype testfun2-designator () '(or testfun2 symbol))

(deftype nonnegative-fixnum () '(and fixnum unsigned-byte))

(deftype function-designator () '(or function symbol))

(deftype extended-function-designator () '(or function symbol list))

(deftype string-designator () '(or character symbol string))

(deftype pathname-designator () '(or string stream pathname))

(deftype package-designator () '(or (string-designator package)))

(deftype byte-specifier () t)

(deftype radix () '(integer 2 36))

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
                           &key (key keyfun)
                                (test testfun2-designator)
                                (test-not testfun2-designator)))
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
                           &key (key keyfun-designator)
                                (test testfun2-designator)
                                (test-not testfun2-designator))
                          (or cons nil))
                assoc))

(declaim (ftype (function (testfun1-designator
                           list
                           &key (key keyfun-designator)
                                (test testfun1-designator)
                                (test-not testfun1-designator))
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

(declaim (ftype general-predicate
                atom))

(declaim (ftype (function ((array bit) &rest list) bit)
                bit))


(declaim (ftype (function ((array bit) (array bit)
                           &optional (or (array bit) (member t nil)))
                          (array bit))
                bit-and
                bit-andc1
                bit-andc2
                bit-eqv
                bit-ior
                bit-nand
                bit-nor
                bit-orc1
                bit-orc2
                bit-xor))

(declaim (ftype (function ((array bit)
                           &optional (or (array bit) (member t nil)))
                          (array bit))
                bit-not))

(declaim (ftype general-predicate
                bit-vector-p))

;;; This could be done better for any particular implementation.
(declaim (ftype (function (t integer integer)
                          integer)
                boole))

(declaim (ftype (function (character) t)
                upper-case-p
                lower-case-p
                both-case-p))

(declaim (ftype (function (symbol) t)
                boundp))

(declaim (ftype (function (&optional (or string function)) &rest list) null
                break))

(declaim (ftype (function (broadcast-stream) list)
                broadcast-stream-streams))

(declaim (ftype (function ((integer 0) (integer 0)) t)
                byte))

(declaim (ftype (function (t) (integer 0))
                byte-size
                byte-position))

(declaim (ftype (function (list) t)
                car
                cdr
                caar
                cadr
                cdar
                cddr
                caaar
                caadr
                cadar
                caddr
                cdaar
                cdadr
                cddar
                cdddr
                caaaar
                caaadr
                caadar
                caaddr
                cadaar
                cadadr
                caddar
                cadddr
                cdaaar
                cdaadr
                cdadar
                cdaddr
                cddaar
                cddadr
                cdddar
                cddddr))

(declaim (ftype (function (number &optional (real 0))
                          (values integer real))
                floor
                ceiling
                truncate
                round))

(declaim (ftype (function (cell-error) t)
                cell-error-name))

(declaim (ftype (function ((or string function) &rest list) null)
                cerror))

(declaim (ftype (function (t (or symbol class) &key &allow-other-keys) t)
                change-class))

(declaim (ftype (function (string (integer 0)) character)
                char))

(declaim (ftype (function (character) t)
                char-code))

(declaim (ftype (function (character) character)
                char-upcase char-downcase))

(declaim (ftype (function (&rest list) t)
                char=
                char/=
                char<
                char>
                char<=
                char>=
                char-equal
                char-not-equal
                char-lessp
                char-greaterp
                char-not-greaterp
                char-not-lessp))

(declaim (ftype (function (character) (integer 0))
                char-int))

(declaim (ftype (function (character) (or string null))
                char-name))

(declaim (ftype (function (character-designator)
                          character)
                character))

(declaim (ftype general-predicate
                characterp))

(declaim (ftype (function (real) complex)
                cis))

(declaim (ftype (function (class) symbol)
                class-name))

(declaim (ftype (function (t) class)
                class-of))

(declaim (ftype (function (&optional (or (member t nil) stream)) null)
                clear-input
                clear-output
                finish-output
                force-output))

(declaim (ftype (function (stream &key (abort t)) t)
                close))

(declaim (ftype (function (hash-table) hash-table)
                clrhash))

(declaim (ftype (function (t) (or character null)) code-char))

(declaim (ftype (function (t t) t)
                coerce))

(declaim (ftype (function (t &optional t)
                          (values t t t))
                compile))
         

(declaim (ftype (function (t &key
                             (output-file t)
                             (verbose t)
                             (print t)
                             (external-format t))
                          (values t t t))
                compile-file))
                
(declaim (ftype (function (t &key (output-file t)) t)
                compile-file-pathname))

(declaim (ftype general-predicate
                compiled-function-p))

(declaim (ftype (function (t &optional t) (or function null))
                compiler-macro-function))

(declaim (ftype (function (function) function)
                complement))

(declaim (ftype (function (real &optional real) complex)
                (or rational complex)
                complex))

(declaim (ftype (function (t) t) complexp))

(declaim (ftype (function (function list) list)
                compute-applicable-methods))

(declaim (ftype (function (t &rest list) sequence)
                concatenate))

(declaim (ftype (function (concatenated-stream) list)
                concatenated-stream-streams))

(declaim (ftype (function (number) number)
                conjugate))

(declaim (ftype (function (t t) cons)
                cons))

(declaim (ftype general-predicate
                consp))

(declaim (ftype (function (t) (function ()))
                constantly))

(declaim (ftype general-predicate
                constantp))

(declaim (ftype (function (&optional (or condition null)) null)
                continue))

(declaim (ftype (function (list) list)
                copy-alist
                copy-list))

(declaim (ftype (function (&optional t) t)
                copy-pprint-dispatch))

(declaim (ftype (function (&optional (or null readtable) (or null readtable))
                          (or null readtable))
                copy-readtable))

(declaim (ftype (function (sequence) sequence)
                copy-seq))

(declaim (ftype (function (structure-object) structure-object) copy-structure))

(declaim (ftype (function (symbol &optional t) symbol)
                copy-symbol))

(declaim (ftype (function (t) t)
                copy-tree))

(declaim (ftype (function (t
                           sequence
                           &key
                           (from-end generalized-boolean)
                           (start (integer 0))
                           (end (or null (integer 0)))
                           (key keyfun-designator)
                           (test testfun2-designator)
                           (test-not testfun2-designator)))
                count))

(declaim (ftype (function (testfun1-designator
                           sequence
                           &key
                           (from-end generalized-boolean)
                           (start (integer 0))
                           (end (or null (integer 0)))
                           (key keyfun-designator)))
                count-if
                count-if-not))

(declaim (ftype (or (function (short-float)
                              (values short-float integer (member 1S0 -1S0)))
                    (function (single-float)
                              (values single-float integer (member 1F0 -1F0)))
                    (function (double-float)
                              (values double-float integer (member 1D0 -1D0)))
                    (function (long-float)
                              (values long-float integer (member 1L0 -1L0))))
                decode-float))

(declaim (ftype (function ((integer 0))
                          (values (integer 0 59)
                                  (integer 0 59)
                                  (integer 0 23)
                                  (integer 1 31)
                                  (integer 1 12)
                                  (integer 1900)
                                  (ingeter 0 6)
                                  generalized-boolean
                                  rational))
                decode-universal-time))

(declaim (ftype (function (t
                           sequence
                           &key
                           (from-end generalized-boolean)
                           (test testfun2-designator)
                           (test-not testfun2-designator)
                           (start (integer 0))
                           (end (or null (integer 0)))
                           (count (or null integer))
                           (key keyfun-designator)))
                delete
                remove))

(declaim (ftype (function (testfun1-designator
                           sequence
                           &key
                           (from-end generalized-boolean)
                           (start (integer 0))
                           (end (or null (integer 0)))
                           (count (or null integer))
                           (key keyfun-designator)))
                delete-if
                delete-if-not
                remove-if
                remove-if-not))

(declaim (ftype (function (sequence
                           &key
                           (from-end generalized-boolean)
                           (test testfun2-designator)
                           (test-not testfun2-designator)
                           (start (integer 0))
                           (end (or null (integer 0)))
                           (key keyfun-designator)))
                delete-duplicates
                remove-duplicates))

(declaim (ftype (function (pathname-designator) (member t))
                delete-file))

(declaim (ftype (function (package-designator) generalized-boolean)
                delete-package))

(declaim (ftype (function (rational) integer)
                denominator
                numerator))

(declaim (ftype (function (integer byte-specifier integer) integer)
                deposit-field))

(declaim (ftype (function (t &optional stream) (values))
                describe))

(declaim (ftype (function (t stream) t)
                describe-object))

(declaim (ftype (function ((integer 0) &optional radix) (or character null))
                digit-char))

(declaim (ftype (function (character &optional radix) (or (integer 0) null))
                digit-char-p))

(declaim (ftype (function (pathname-designator) list)
                directory))

(declaim (ftype (function (pathname-designator) (or string null))
                directory-namestring
                file-namestring
                host-namestring
                namestring))

(declaim (ftype (function (extended-function-designator) null)
                disassemble))

(declaim (ftype (function (integer byte-specifier integer) integer)
                dpb))

(declaim (ftype (function (&optional pathname-designator) t)
                dribble))

(declaim (ftype (function (pathname-designator &optional pathname-designator)
                          (or string null))
                enough-namestring))

(declaim (ftype (function (float) integer)
                float-radix))

(declaim (ftype (or (function (short-float) short-float)
                    (function (single-float) single-float)
                    (function (double-float) double-float)
                    (function (long-float) long-float))
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
         float-sign)

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

(declaim (ftype (function (simple-string (integer 0)) character)
                schar))

(declaim (ftype (function ((simple-array bit) &rest list) bit)
                sbit))

(declaim (ftype (or (function (short-float integer) short-float)
                    (function (single-float integer) single-float)
                    (function (double-float integer) double-float)
                    (function (long-float integer) long-float))
                scale-float))


