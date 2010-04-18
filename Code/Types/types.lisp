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

(declaim (ftype (function (t) t)
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

(declaim (ftype (function (&optional (or string function)) &rest list) null)
         break)

(declaim (ftype (function (broadcast-stream) list)
                broadcast-stream-streams))

(declaim (ftype (function ((integer 0) (integer 0)) t)
                byte))

(declaim (ftype (function (t) (integer 0))
                byte-size
                byte-position))

(declaim (ftype (function (list) t)
                car cdr
                caar cadr cdar cddr
                caaar caadr cadar caddr cdaar cdadr cddar cdddr
                caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))

(declaim (ftype (function (number &optional (real 0))
                          (values integer real))
                floor ceiling truncate round))

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
                char= char/= char< char> char<= char>=
                char-equal char-not-equal char-lessp
                char-greaterp char-not-greaterp char-not-lessp))

(declaim (ftype (function (character) (integer 0))
                char-int))

(declaim (ftype (function (character) (or string null))
                char-name))

(declaim (ftype (function ((or (string 1) symbol character))
                          character)
                character))

(declaim (ftype (function (t) t) characterp))

(declaim (ftype (function (real) complex) cis))

(declaim (ftype (function (class) symbol) class-name))

(declaim (ftype (function (t) class) class-of))

(declaim (ftype (function (&optional (or (member t nil) stream)) null)
                clear-input
                clear-output
                finish-output
                force-output))

(declaim (ftype (function (stream &key (abort t)) t)
                close))

(declaim (ftype (function (hash-table) hash-table) clrhash))

(declaim (ftype (function (t) (or character null)) code-char))

(declaim (ftype (function (t t) t) coerce))

(declaim (ftype (function (t &optional t)
                          (values t t t))
                compile))
         

(declaim (ftype (function (t &key
                             (output-file t)
                             (verbose t)
                             (print t)
                             (external-format t))
                          (values t t t))
                (compile-file)))
                
(declaim (ftype (function (t &key (output-file t)) t)
                compile-file-pathname))

(declaim (ftype (function (t) t) compiled-function-p))

(declaim (ftype (function (t &optional t) (or function null))
                compiler-macro-function))

(declaim (ftype (function (function) function) complement))

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

(declaim (ftype (function (number) number) conjugate))

(declaim (ftype (function (t t) cons) cons))

(declaim (ftype (function (t) t) consp))

(declaim (ftype (function (t) function) constantly))

(declaim (ftype (function (t &optional t) t) constantp))

(declaim (ftype (function (&optional (or condition null)) null)
                continue))

(declaim (ftype (function (list) list) copy-alist copy-list))

(declaim (ftype (function (&optional t) t) copy-pprint-dispatch))

(declaim (ftype (function (&optional (or null readtable) (or null readtable))
                          (or null readtable))
                copy-readtable))

(declaim (ftype (function (sequence) sequence) copy-seq))

(declaim (ftype (function (structure-object) structure-object) copy-structure))

(declaim (ftype (function (symbol &optional t) symbol) copy-symbol))

(declaim (ftype (function (t) t) copy-tree))

(declaim (ftype (function (t sequence
                             &key
                             (from-end t)
                             (start (or null (integer 0)))
                             (end (or null (integer 0)))
                             (key (or symbol function))
                             (test (or symbol function))
                             (test-not (or symbol function))))
                count))

(declaim (ftype (function (function sequence
                             &key
                             (from-end t)
                             (start (or null (integer 0)))
                             (end (or null (integer 0)))
                             (key (or symbol function))))
                count-if count-if-not))

(declaim (ftype (function (number &optional (real 0))
                          (values (real 0) real))
                ffloor fceiling ftruncate fround))

(declaim (ftype (function (simple-string (integer 0)) character)
                schar))

(declaim (ftype (function ((simple-array bit) &rest list) bit)
                sbit))


