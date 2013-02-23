(in-package #:sicl-type-proclamations)

(proclaim '(ftype symbol-predicate name-of-length-1))

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
(deftype testfun2 () '(function (t t) generalized-boolean))

(deftype testfun2-designator () '(or testfun2 symbol))

(deftype nonnegative-fixnum () '(and fixnum unsigned-byte))

(deftype function-designator () '(or function symbol))

(deftype extended-function-designator () '(or function symbol list))

(deftype string-designator () '(or character symbol string))

(deftype pathname-designator () '(or string stream pathname))

(deftype package-designator () '(or (string-designator package)))

(deftype byte-specifier () t)

(deftype radix () '(integer 2 36))

(proclaim '(ftype (function (&optional (or null condition)) nil)
	    abort))

(proclaim '(ftype (function (number) (real 0.0))
	    abs))

(proclaim '(ftype (function (t t list) list)
	    acons))

(proclaim '(ftype (function (number) number)
	    acos))

(proclaim '(ftype (function (number) number)
	    acosh))

(proclaim '(ftype (function (function method) function)
	    add-method))

(proclaim '(ftype (function (t list
			     &key (key keyfun)
			     (test testfun2-designator)
			     (test-not testfun2-designator)))
	    adjoin))

(proclaim '(ftype (function (array (or fixnum list)
			     &key (element-type t)
			     (initial-element t)
			     (initial-contents t)
			     (fill-pointer nonnegative-fixnum)
			     (displaced-to (or array null))
			     (displaced-index-offset nonnegative-fixnum))
		   array)
	    adjust-array))

(proclaim '(ftype (function (array) t)
	    adjustable-array-p))

(proclaim '(ftype (function (class &rest list &key &allow-other-keys) t)
	    allocate-instance))

(proclaim '(ftype (function (character) t)
	    alpha-char-p))

(proclaim '(ftype (function (character) t)
	    alphanumericp))

(proclaim '(ftype (function (&rest list) t)
	    append))

(proclaim '(ftype (function (function-designator &rest args) t)
	    apply))

(proclaim '(ftype (function (string-designator
			     &optional package-designator)
		   nil)
	    apropos))

(proclaim '(ftype (function (string-designator
			     &optional package-designator)
		   list)
	    apropos-list))

(proclaim '(ftype (function (array &rest list) t)
	    aref))

(proclaim '(ftype (function (array (integer 0)) (integer 0))
	    array-dimension))

(proclaim '(ftype (function (array) list)
	    array-dimensions))

(proclaim '(ftype (function (array)
		   (values (or array nil)
		    (and fixnum (integer 0))))
	    array-displacement))

(proclaim '(ftype (function (array)
		   (or symbol list class))
	    array-element-type))

(proclaim '(ftype (function (array) t)
	    array-has-fill-pointer-p))

(proclaim '(ftype (function (array &rest list) t)
	    array-in-bounds-p))

(proclaim '(ftype (function (array) (integer 0))
	    array-rank))

(proclaim '(ftype (function (array &rest list) (and fixnum (integer 0)))
	    array-row-major-index))

(proclaim '(ftype (function (array) (integer 0))
	    array-total-size))

(proclaim '(ftype (function (array) t)
	    arrayp))

(proclaim '(ftype (function (integer integer) integer)
	    ash))

(proclaim '(ftype (function (t list
			     &key (key keyfun-designator)
			     (test testfun2-designator)
			     (test-not testfun2-designator))
		   (or cons nil))
	    assoc))

(proclaim '(ftype (function (testfun1-designator
			     list
			     &key (key keyfun-designator)
			     (test testfun1-designator)
			     (test-not testfun1-designator))
		   (or cons nil))
	    assoc-if
	    assoc-if-not))

(proclaim '(ftype (function (number) number)
	    asin
	    asinh
	    atanh
	    cosh
	    sinh
	    tanh))

(proclaim '(ftype (or (function (number) number)
		   (function (real real) number))
	    atan))

(proclaim '(ftype general-predicate
	    atom))

(proclaim '(ftype (function ((array bit) &rest list) bit)
	    bit))


(proclaim '(ftype (function ((array bit) (array bit)
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

(proclaim '(ftype (function ((array bit)
			     &optional (or (array bit) (member t nil)))
		   (array bit))
	    bit-not))

(proclaim '(ftype general-predicate
	    bit-vector-p))

;;; This could be done better for any particular implementation.
(proclaim '(ftype (function (t integer integer)
		   integer)
	    boole))

(proclaim '(ftype (function (character) t)
	    upper-case-p
	    lower-case-p
	    both-case-p))

(proclaim '(ftype (function (symbol) t)
	    boundp))

(proclaim '(ftype (function (&optional (or string function)) &rest list) null
	    break))

(proclaim '(ftype (function (broadcast-stream) list)
	    broadcast-stream-streams))

(proclaim '(ftype (function ((integer 0) (integer 0)) t)
	    byte))

(proclaim '(ftype (function (t) (integer 0))
	    byte-size
	    byte-position))

(proclaim '(ftype (function (list) t)
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

(proclaim '(ftype (function (number &optional (real 0))
		   (values integer real))
	    floor
	    ceiling
	    truncate
	    round))

(proclaim '(ftype (function (cell-error) t)
	    cell-error-name))

(proclaim '(ftype (function ((or string function) &rest list) null)
	    cerror))

(proclaim '(ftype (function (t (or symbol class) &key &allow-other-keys) t)
	    change-class))

(proclaim '(ftype (function (string (integer 0)) character)
	    char))

(proclaim '(ftype (function (character) t)
	    char-code))

(proclaim '(ftype (function (character) character)
	    char-upcase char-downcase))

(proclaim '(ftype (function (&rest list) t)
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

(proclaim '(ftype (function (character) (integer 0))
	    char-int))

(proclaim '(ftype (function (character) (or string null))
	    char-name))

(proclaim '(ftype (function (character-designator)
		   character)
	    character))

(proclaim '(ftype general-predicate
	    characterp))

(proclaim '(ftype (function (real) complex)
	    cis))

(proclaim '(ftype (function (class) symbol)
	    class-name))

(proclaim '(ftype (function (t) class)
	    class-of))

(proclaim '(ftype (function (&optional (or (member t nil) stream)) null)
	    clear-input
	    clear-output
	    finish-output
	    force-output))

(proclaim '(ftype (function (stream &key (abort t)) t)
	    close))

(proclaim '(ftype (function (hash-table) hash-table)
	    clrhash))

(proclaim '(ftype (function (t) (or character null)) code-char))

(proclaim '(ftype (function (t t) t)
	    coerce))

(proclaim '(ftype (function (t &optional t)
		   (values t t t))
	    compile))


(proclaim '(ftype (function (t &key
                             (output-file t)
                             (verbose t)
                             (print t)
                             (external-format t))
		   (values t t t))
	    compile-file))

(proclaim '(ftype (function (t &key (output-file t)) t)
	    compile-file-pathname))

(proclaim '(ftype general-predicate
	    compiled-function-p))

(proclaim '(ftype (function (t &optional t) (or function null))
	    compiler-macro-function))

(proclaim '(ftype (function (function) function)
	    complement))

(proclaim '(ftype (function (real &optional real) (or rational complex))
	    complex))

(proclaim '(ftype (function (t) t) complexp))

(proclaim '(ftype (function (function list) list)
	    compute-applicable-methods))

(proclaim '(ftype (function (t &rest list) sequence)
	    concatenate))

(proclaim '(ftype (function (concatenated-stream) list)
	    concatenated-stream-streams))

(proclaim '(ftype (function (number) number)
	    conjugate))

(proclaim '(ftype (function (t t) cons)
	    cons))

(proclaim '(ftype general-predicate
	    consp))

(proclaim '(ftype (function (t) (function ()))
	    constantly))

(proclaim '(ftype general-predicate
	    constantp))

(proclaim '(ftype (function (&optional (or condition null)) null)
	    continue))

(proclaim '(ftype (function (list) list)
	    copy-alist
	    copy-list))

(proclaim '(ftype (function (&optional t) t)
	    copy-pprint-dispatch))

(proclaim '(ftype (function (&optional (or null readtable) (or null readtable))
		   (or null readtable))
	    copy-readtable))

(proclaim '(ftype (function (sequence) sequence)
	    copy-seq))

(proclaim '(ftype (function (structure-object) structure-object) copy-structure))

(proclaim '(ftype (function (symbol &optional t) symbol)
	    copy-symbol))

(proclaim '(ftype (function (t) t)
	    copy-tree))

(proclaim '(ftype (function (t
			     sequence
			     &key
			     (from-end generalized-boolean)
			     (start (integer 0))
			     (end (or null (integer 0)))
			     (key keyfun-designator)
			     (test testfun2-designator)
			     (test-not testfun2-designator)))
	    count))

(proclaim '(ftype (function (testfun1-designator
			     sequence
			     &key
			     (from-end generalized-boolean)
			     (start (integer 0))
			     (end (or null (integer 0)))
			     (key keyfun-designator)))
	    count-if
	    count-if-not))

(proclaim '(ftype (or (function (short-float)
		       (values short-float integer (member 1S0 -1S0)))
		   (function (single-float)
		    (values single-float integer (member 1F0 -1F0)))
		   (function (double-float)
		    (values double-float integer (member 1D0 -1D0)))
		   (function (long-float)
		    (values long-float integer (member 1L0 -1L0))))
	    decode-float))

(proclaim '(ftype (function ((integer 0))
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

(proclaim '(ftype (function (t
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

(proclaim '(ftype (function (testfun1-designator
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

(proclaim '(ftype (function (sequence
			     &key
			     (from-end generalized-boolean)
			     (test testfun2-designator)
			     (test-not testfun2-designator)
			     (start (integer 0))
			     (end (or null (integer 0)))
			     (key keyfun-designator)))
	    delete-duplicates
	    remove-duplicates))

(proclaim '(ftype (function (pathname-designator) (member t))
	    delete-file))

(proclaim '(ftype (function (package-designator) generalized-boolean)
	    delete-package))

(proclaim '(ftype (function (rational) integer)
	    denominator
	    numerator))

(proclaim '(ftype (function (integer byte-specifier integer) integer)
	    deposit-field))

(proclaim '(ftype (function (t &optional stream) (values))
	    describe))

(proclaim '(ftype (function (t stream) t)
	    describe-object))

(proclaim '(ftype (function ((integer 0) &optional radix) (or character null))
	    digit-char))

(proclaim '(ftype (function (character &optional radix) (or (integer 0) null))
	    digit-char-p))

(proclaim '(ftype (function (pathname-designator) list)
	    directory))

(proclaim '(ftype (function (pathname-designator) (or string null))
	    directory-namestring
	    file-namestring
	    host-namestring
	    namestring))

(proclaim '(ftype (function (extended-function-designator) null)
	    disassemble))

(proclaim '(ftype (function (integer byte-specifier integer) integer)
	    dpb))

(proclaim '(ftype (function (&optional pathname-designator) t)
	    dribble))

(proclaim '(ftype (function (pathname-designator &optional pathname-designator)
		   (or string null))
	    enough-namestring))

(proclaim '(ftype (function (float) integer)
	    float-radix))

(proclaim '(ftype (or (function (short-float) short-float)
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

(proclaim '(ftype (function (float) (integer 0))
	    float-digits))

(proclaim '(ftype (function (float) (integer 0))
	    float-precision))

(proclaim '(ftype (function (number &optional (real 0))
		   (values (real 0) real))
	    ffloor fceiling ftruncate fround))

(proclaim '(ftype (or (function (short-float)
		       (values short-float integer (member -1 1)))
		   (function (single-float)
		    (values single-float integer (member -1 1)))
		   (function (double-float)
		    (values double-float integer (member -1 1)))
		   (function (long-float)
		    (values long-float integer (member -1 1))))
	    integer-decode-float))

(proclaim '(ftype (function (simple-string (integer 0)) character)
	    schar))

(proclaim '(ftype (function ((simple-array bit) &rest list) bit)
	    sbit))

(proclaim '(ftype (or (function (short-float integer) short-float)
		   (function (single-float integer) single-float)
		   (function (double-float integer) double-float)
		   (function (long-float integer) long-float))
	    scale-float))


