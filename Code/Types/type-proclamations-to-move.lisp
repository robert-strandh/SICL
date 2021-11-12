(cl:in-package #:sicl-type-proclamations)

;;; like FUNCTION but returning an exact number of values.
;;; does not accept values types with lambda list keywords.

;;; CLHS 1.6 says standard functions can't return more values than
;;; they are specified to. But some types here are arguments, like
;;; keyfun, and in that case conforming programs can pass a
;;; function that returns more values if they really want to.

;;; Having (values ... &rest nil) should conformingly mean no more
;;; values, so we do that here. Works in Cleavir and works in SBCL.
(deftype sfunction (&optional args result)
  `(function ,args ,(if (consp result)
			`(,@result &rest nil)
			`(values ,result &rest nil))))

(declaim (ftype symbol-predicate name-of-length-1))

(deftype character-designator ()
  "Glossary definition."
  '(or character (string 1) (and symbol (satisfies name-of-length-1))))

(deftype function-designator (&optional (ftype 'function))
  "Glossary definition plus FTYPE.
E.g. argument to FUNCALL."
  `(or ,ftype symbol))

(deftype function-name ()
  "Glossary definition.
Some implementations define their own function names, like SBCL's (SB-PCL:CTOR ...), sometimes including extensibility."
  '(or symbol (cons (member setf) (cons symbol null))))

(deftype keyfun ()
  "Used for function arguments that apply a key function before comparing two objects."
  '(function (t) t))

(deftype keyfun-designator ()
  '(function-designator keyfun))

(deftype predicate (&optional (input 't))
  "Glossary definition plus INPUT.
This type should be restricted to non-side-effectful functions for human-understandability.
INPUT is an optional restriction of input types to the predicate."
  ;; i'm (Bike) not sure that (function (*)) is a legal type.
  `(sfunction (,input) generalized-boolean))
(deftype predicate-designator (&optional input) `(function-designator (predicate ,input)))

(deftype type-predicate (type &optional input)
  "A function from objects to a generalized boolean indicating whether an object is of a given type.
For human documentation only at this time - ignores TYPE and expands to PREDICATE.
In the future, more information could possibly be stored here."
  (declare (ignore type))
  `(predicate ,input))

(deftype testfun1 ()
  "Used for one-argument tests, e.g. in ASSOC"
  'predicate)

(deftype testfun1-designator () '(function-designator testfun1))

(deftype testfun2 ()
  "Used for two-argument tests, e.g. in COUNT"
  '(function (t t) generalized-boolean))

(deftype testfun2-designator () '(or testfun2 symbol))

(deftype nonnegative-fixnum () '(and fixnum unsigned-byte))

(deftype extended-function-designator ()
  "Glossary definition."
  '(or function function-name))

(deftype string-designator ()
  "Glossary definition."
  '(or character symbol string))

(deftype pathname-designator ()
  "Glossary definition."
  '(or string stream pathname))

(deftype package-designator ()
  "Glossary definition."
  '(or string-designator package))

(deftype stream-designator ()
  ;; CLHS distinguishes output and input stream designators but they have (except streams?) the same value set
  "Glossary definition."
  '(or stream (member nil t)))

(deftype byte-specifier ()
  "Type of return values of BYTE. Could be specialized for implementations."
  t)

(deftype formatter ()
  "A formatter function, as returned by FORMATTER."
  '(function (stream &rest t) list))

(deftype format-control ()
  "Glossary definition. A format string, or a formatter function."
  ;; the format of format strings is, hypothetically, compile-time determinable,
  ;; but you'd basically need SATISFIES for verification.
  '(or string formatter))

;;; In fact, this type is entirely correct only for the END bounding
;;; index.  The START boudning index is more restricted, because NIL
;;; is not allowed as a START boudning index.
(deftype bounding-index-designator ()
  "Glossary definition."
  '(or (integer 0) null))

(deftype proper-sequence ()
  "Glossary definition.
Ideally, this would include null-termination for conses, but that's probably infeasible."
  'sequence)

(deftype lambda-expression ()
  "A valid lambda expression.
Some implementations define their own lambda forms, like CCL's nlambda."
  ;; more detail not worth it at this time
  '(cons (eql lambda)))

(deftype valid-array-dimension ()
  "Possible results of CL:ARRAY-DIMENSION."
  ;; one hopes that this can be inferred to be a fixnum, so i've left out that constraint.
  `(integer 0 (,array-dimension-limit)))
(deftype array-index ()
  `(integer 0 (,array-dimension-limit)))
(deftype valid-array-rank ()
  "Possible results of CL:ARRAY-RANK."
  `(integer 0 (,array-rank-limit)))

(deftype radix ()
  "Glossary definition. A radix usable for numeric representations."
  '(integer 2 36))

(deftype type-specifier ()
  '(or symbol list class))

(deftype condition-designator-datum (&optional (condition-superclass 'condition))
  "A valid argument for the 'datum' argument of condition-handling functions like ERROR.
See CLHS 9.1.2.1."
  `(or symbol format-control ,condition-superclass))

(deftype class-designator ()
  "Glossary definition."
  '(or class symbol))

(deftype character-code ()
  "Glossary definition."
  `(integer 0 (,char-code-limit)))

(deftype readtable-designator ()
  "Glossary definition."
  '(or null readtable))

(declaim (ftype (sfunction (function method) function)
		add-method))

(declaim (ftype (sfunction (t list
			     &key (:key keyfun)
			     (:test testfun2-designator)
			      (:test-not testfun2-designator))
			   list)
		adjoin))

(declaim (ftype (sfunction (array (or array-dimension list) ; "list of valid array dimensions" is complex and inexpressible
				 &key (:element-type type-specifier)
				 (:initial-element t)
				 (:initial-contents t) ; could be more complicated
				 (:fill-pointer (or nonnegative-fixnum null (eql t))
				 (:displaced-to (or array null))
				 (:displaced-index-offset nonnegative-fixnum))
			  array)
		adjust-array))

;; kind of a type predicate... but not really.
(declaim (ftype (sfunction (array) generalized-boolean)
		adjustable-array-p))

;; note that the list is specifically a plist
(declaim (ftype (sfunction (class &rest list &key &allow-other-keys) t)
		allocate-instance))

;; again not really type predicates
(declaim (ftype (sfunction (character) generalized-boolean)
		alpha-char-p
		alphanumericp))

;;; "each must be a proper list except the last, which can be
;;;  any object"
(declaim (ftype (sfunction (&rest t) t)
		append))

(declaim (ftype (function (function-designator &rest args) *)
		apply))

(declaim (ftype (function (string-designator
			   &optional (or package-designator null))
			  (values &rest nil)) ; no values
		apropos))

(declaim (ftype (sfunction (string-designator
			   &optional (or package-designator null))
			  list) ; a list of symbols
		apropos-list))

(declaim (ftype (sfunction (array &rest array-index) t)
		aref))

(declaim (ftype (sfunction (array valid-array-rank) array-dimension)
		array-dimension))

(declaim (ftype (sfunction (array) list) ; a list of array dimensions
		array-dimensions))

(declaim (ftype (sfunction (array)
			  (values (or array nil) non-negative-fixnum))
		array-displacement))

(declaim (ftype (sfunction (array)
			  type-specifier)
		array-element-type))

(declaim (ftype (sfunction (array) generalized-boolean)
		array-has-fill-pointer-p))

(declaim (ftype (sfunction (array &rest integer) generalized-boolean)
		array-in-bounds-p))

(declaim (ftype (sfunction (array) valid-array-rank)
		array-rank))

(declaim (ftype (sfunction (array &rest array-index) non-negative-fixnum)
		array-row-major-index))

(declaim (ftype (sfunction (array) (integer 0))
		array-total-size))

(declaim (ftype (type-predicate array)
		arrayp))

(declaim (ftype (type-predicate vectr)
		vectorp))

(declaim (ftype (sfunction (t list
			     &key (:key keyfun-designator)
			     (:test testfun2-designator)
			     (:test-not testfun2-designator))
			  (or cons null))
		assoc))

(declaim (ftype (sfunction (testfun1-designator
			   list
			   &key (:key keyfun-designator)
			  (or cons null))
		assoc-if
		assoc-if-not))

(declaim (ftype (type-predicate atom)
		atom))

(declaim (ftype (sfunction ((array bit) &rest array-index) bit)
		bit))

(declaim (ftype (sfunction ((array bit) (array bit)
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

(declaim (ftype (sfunction ((array bit)
			   &optional (or (array bit) (member t nil)))
			  (array bit))
		bit-not))

(declaim (ftype (type-predicate (vector bit))
		bit-vector-p))

;;; This could be done better for any particular implementation.
(declaim (ftype (sfunction (t integer integer)
			  integer)
		boole))

(declaim (ftype (sfunction (character) generalized-boolean)
		upper-case-p
		lower-case-p
		both-case-p))

(declaim (ftype (sfunction (symbol) generalized-boolean)
		boundp))

(declaim (ftype (sfunction (&optional format-control &rest t) null)
		break))

(declaim (ftype (sfunction (broadcast-stream) list) ; list of streams
		broadcast-stream-streams))

(declaim (ftype (sfunction ((integer 0) (integer 0)) byte-specifier)
		byte))

(declaim (ftype (sfunction (byte-specifier) (integer 0))
		byte-size
		byte-position))

(declaim (ftype (sfunction (real &optional (real 0))
			  (values integer real))
		floor
		ceiling
		truncate
		round))

(declaim (ftype (sfunction (cell-error) t)
		cell-error-name))

;; note that condition designator arguments have to be a plist
(declaim (ftype (sfunction (format-control condition-designator-datum &rest t) nil)
		cerror))

(declaim (ftype (sfunction (t class-designator &key &allow-other-keys) t)
		change-class))

(declaim (ftype (sfunction (real) complex)
		cis))

(declaim (ftype (sfunction (class) symbol)
		class-name))

(declaim (ftype (sfunction (t) class)
		class-of))

(declaim (ftype (sfunction (stream &key (:abort generalized-boolean)) t)
		close))

(declaim (ftype (sfunction (hash-table) hash-table)
		clrhash))

(declaim (ftype (sfunction (character-code) (or character null))
		code-char))

(declaim (ftype (sfunction (t type-specifier) t)
		coerce))

(declaim (ftype (sfunction ((or function-name null)
			   &optional (or function lambda-expression))
			  (values (or function-name compiled-function)
				  generalized-boolean
				  generalized-boolean))
		compile))

(declaim (ftype (sfunction (pathname-designator &key
                             (:output-file pathname-designator)
                             (:verbose generalized-boolean)
                             (:print generalized-boolean)
                             (:external-format t)) ; external file format designator
			  (values (or pathname null)
				  generalized-boolean
				  generalized-boolean))
		compile-file))

(declaim (ftype (sfunction (pathname-designator &key (:output-file pathname-designator) &allow-other-keys) pathname)
		compile-file-pathname))

(declaim (ftype (type-predicate compiled-function)
		compiled-function-p))

(declaim (ftype (sfunction (function-name &optional t #|an environment|#) (or function null))
		compiler-macro-function))

(declaim (ftype (sfunction (function) (function * generalized-boolean))
		complement))

(declaim (ftype (sfunction (real &optional real) (or rational complex))
		complex))

(declaim (ftype (type-predicate complex) complexp))

(declaim (ftype (sfunction (generic-function list) list #|of methods|#)
		compute-applicable-methods))

;; I'm not sure that concatenate is defined on improper sequences.
(declaim (ftype (sfunction (type-specifier &rest sequence) proper-sequence)
		concatenate))

(declaim (ftype (sfunction (concatenated-stream) list #|of streams|#)
		concatenated-stream-streams))

(declaim (ftype (sfunction (number) number)
		conjugate))

(declaim (ftype (sfunction (t t) cons)
		cons))

(declaim (ftype (type-predicate cons)
		consp))

(declaim (ftype (sfunction (t) (sfunction * t))
		constantly))

(declaim (ftype (sfunction (t &optional t #|an environment|#) generalized-boolean)
		constantp))

(declaim (ftype (sfunction (&optional (or condition null)) null)
		continue))

(declaim (ftype (sfunction (list) list)
		copy-alist
		copy-list))

(declaim (ftype (sfunction (&optional t) t) ; objects are pprint dispatch tables
		copy-pprint-dispatch))

(declaim (ftype (sfunction (&optional readtable-designator (or null readtable))
			  readtable)
		copy-readtable))

(declaim (ftype (sfunction (proper-sequence) proper-sequence)
		copy-seq))

(declaim (ftype (sfunction (structure-object) structure-object) copy-structure))

(declaim (ftype (sfunction (symbol &optional generalized-boolean) symbol)
		copy-symbol))

(declaim (ftype (sfunction (t) t)
		copy-tree))

(declaim (ftype (sfunction (t
			   sequence
			   &key
			   (:from-end generalized-boolean)
			   (:start bounding-index-designator)
			   (:end bounding-index-designator)
			   (:key keyfun-designator)
			   (:test testfun2-designator)
			   (:test-not testfun2-designator))
			  (integer 0))
		count))

(declaim (ftype (sfunction (testfun1-designator
			   sequence
			   &key
			   (:from-end generalized-boolean)
			   (:start bounding-index-designator)
			   (:end bounding-index-designator)
			   (:key keyfun-designator))
			  (integer 0))
		count-if
		count-if-not))

(declaim (ftype (sfunction (float) (values float integer float))
		decode-float))

(declaim (ftype (sfunction ((integer 0))
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

(declaim (ftype (sfunction (t
			   proper-sequence
			   &key
			   (:from-end generalized-boolean)
			   (:test testfun2-designator)
			   (:test-not testfun2-designator)
			   (:start bounding-index-designator)
			   (:end bounding-index-designator)
			   (:count (or null integer))
			   (:key keyfun-designator)))
		delete
		remove))

(declaim (ftype (sfunction (testfun1-designator
			   proper-sequence
			   &key
			   (:from-end generalized-boolean)
			   (:start (integer 0))
			   (:end (or null (integer 0)))
			   (:count (or null integer))
			   (:key keyfun-designator)))
		delete-if
		delete-if-not
		remove-if
		remove-if-not))

(declaim (ftype (sfunction (proper-sequence
			   &key
			   (:from-end generalized-boolean)
			   (:test testfun2-designator)
			   (:test-not testfun2-designator)
			   (:start bounding-index-designator)
			   (:end bounding-index-designator)
			   (:key keyfun-designator)))
		delete-duplicates
		remove-duplicates))

(declaim (ftype (sfunction (pathname-designator) (eql t))
		delete-file))

(declaim (ftype (sfunction (package-designator) generalized-boolean)
		delete-package))

(declaim (ftype (sfunction (rational) integer)
		denominator
		numerator))

(declaim (ftype (sfunction (integer byte-specifier integer) integer)
		deposit-field))

(declaim (ftype (sfunction (t &optional stream-designator) (values))
		describe))

(declaim (ftype (function (t stream)
			  ;; no values
			  (values &rest nil))
		describe-object))

(declaim (ftype (sfunction ((integer 0) &optional radix) (or character null))
		digit-char))

(declaim (ftype (sfunction (character &optional radix) (or (integer 0) null))
		digit-char-p))

(declaim (ftype (sfunction (pathname-designator) list)
		directory))

(declaim (ftype (sfunction (pathname-designator) (or string null))
		directory-namestring
		file-namestring
		host-namestring
		namestring))

(declaim (ftype (sfunction ((or extended-function-designator lambda-expression)) null)
		disassemble))

(declaim (ftype (sfunction (integer byte-specifier integer) integer)
		dpb))

(declaim (ftype (sfunction (&optional pathname-designator) t)
		dribble))

(declaim (ftype (sfunction (pathname-designator &optional pathname-designator)
			  (or string null))
		enough-namestring))

(declaim (ftype (sfunction (simple-string array-index) character)
		schar))

(declaim (ftype (sfunction ((simple-array bit) &rest array-index) bit)
		sbit))

(declaim (ftype (sfunction (float integer) float)
		scale-float))

(declaim (ftype (sfunction (function function-name) function)
		(setf fdefinition)))

(declaim (ftype (sfunction (symbol &optional generalized-boolean t))
		find-class))

(declaim (ftype (sfunction (proper-sequence
			   &key (:from-end generalized-boolean)
			   (:test testfun2)
			   (:test-not testfun2)
			   (:start bounding-index-designator)
			   (:end bounding-index-designator)
			   (:key keyfun-designator))
			  t)
		find))

(declaim (ftype (sfunction (predicate-designator
			   proper-sequence
			   &key (:from-end generalized-boolean)
			   (:start bounding-index-designator)
			   (:end bounding-index-designator)
			   (:key keyfun-designator))
			  t)
		find-if
		find-if-not))

(declaim (ftype (sfunction (generic-function
			   list
			   list
			   generalized-boolean)
			  (or method null))
		find-method))

;; CLHS doesn't say "package designator" even though this is the same, why rock the boat
(declaim (ftype (sfunction (or string-designator package) (or package null))
		find-package))

(declaim (ftype (sfunction ((or (and symbol (not null)) restart)
			   &optional (or condition null))
			  (or restart null))
		find-restart))

(declaim (ftype (sfunction (string ; not a string designator
			   &optional package-designator)
			  (values (or symbol null) ; redundant, but human-helpful
				  (member :inherited :external :internal nil)))
		find-symbol))

(declaim (ftype (sfunction (&optional stream-designator) null)
		finish-output
		force-output
		clear-input
		clear-output))

(declaim (ftype (sfunction list t)
		first
		second
		third
		fourth
		fifth
		sixth
		seventh
		eighth
		ninth
		tenth))

(declaim (ftype (sfunction function-name function-name)
		fmakunbound))

(declaim (ftype (sfunction ((or null (eql t) stream string) ; note "string with fill-pointer" specified
			   format-control
			   &rest t)
			  (or null string))
		format))

(declaim (ftype (sfunction (&optional stream-designator) generalized-boolean)
		fresh-line))

(declaim (ftype (function (function-designator &rest t) *)
		funcall))

(declaim (ftype (sfunction (function)
			  (values (or lambda-expression null) generalized-boolean t))
		function-lambda-expression))

(declaim (ftype (type-predicate function)
		functionp))

(declaim (ftype (type-predicate null) null))
