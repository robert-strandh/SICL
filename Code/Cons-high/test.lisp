(in-package #:sicl-cons-high-test)

;;; A function for coercing truth values to BOOLEAN

(defun notnot (x) (not (not x)))

(defmacro notnot-mv (form)
  `(notnot-mv-fn (multiple-value-list ,form)))

(defun notnot-mv-fn (results)
  (if (null results)
      (values)
    (apply #'values
	   (not (not (first results)))
	   (rest results))))

(defmacro not-mv (form)
  `(not-mv-fn (multiple-value-list ,form)))

(defun not-mv-fn (results)
  (if (null results)
      (values)
    (apply #'values
	   (not (first results))
	   (rest results))))

(declaim (ftype (function (t) function) to-function))

(defun to-function (fn)
  (etypecase fn
    (function fn)
    (symbol (symbol-function fn))
    ((cons (eql setf) (cons symbol null)) (fdefinition fn))))

;;; Macro to check that a function is returning a specified number of values
;;; (defaults to 1)
(defmacro check-values (form &optional (num 1))
  (let ((v (gensym))
	(n (gensym)))
   `(let ((,v (multiple-value-list ,form))
	  (,n ,num))
      (check-values-length ,v ,n ',form)
      (car ,v))))

(defun check-values-length (results expected-number form)
  (declare (type fixnum expected-number))
  (let ((n expected-number))
    (declare (type fixnum n))
    (dolist (e results)
      (declare (ignore e))
      (decf n))
    (unless (= n 0)
      (error "Expected ~A results from ~A, got ~A results instead.~%~
Results: ~A~%" expected-number form n results))))

;;; Do multiple-value-bind, but check # of arguments
(defmacro multiple-value-bind* ((&rest vars) form &body body)
  (let ((len (length vars))
	(v (gensym)))
    `(let ((,v (multiple-value-list ,form)))
       (check-values-length ,v ,len ',form)
       (destructuring-bind ,vars ,v ,@body))))
  
;;; Comparison functions that are like various builtins,
;;; but are guaranteed to return T for true.

(defun eqt (x y)
  "Like EQ, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eq x y)))))

(defun eqlt (x y)
  "Like EQL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (eql x y)))))

(defun equalt (x y)
  "Like EQUAL, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equal x y)))))

(defun equalpt (x y)
  "Like EQUALP, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (equalp x y)))))

(defun equalpt-or-report (x y)
  "Like EQUALPT, but return either T or a list of the arguments."
  (or (equalpt x y) (list x y)))

(defun string=t (x y)
  (notnot-mv (string= x y)))

(defun =t (x &rest args)
  "Like =, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (apply #'=  x args)))))

(defun <=t (x &rest args)
  "Like <=, but guaranteed to return T for true."
  (apply #'values (mapcar #'notnot (multiple-value-list (apply #'<=  x args)))))

(defun make-int-list (n)
  (loop for i from 0 below n collect i))

(defun make-int-array (n &optional (fn #'make-array))
  (when (symbolp fn)
    (assert (fboundp fn))
    (setf fn (symbol-function (the symbol fn))))
  (let ((a (funcall (the function fn) n)))
    (declare (type (array * *) a))
    (loop for i from 0 below n do (setf (aref a i) i))
    a))

;;; *universe* is defined elsewhere -- it is a list of various
;;; lisp objects used when stimulating things in various tests.
(declaim (special *universe*))

;;; The function EMPIRICAL-SUBTYPEP checks two types
;;; for subtypeness, first using SUBTYPEP*, then (if that
;;; fails) empirically against all the elements of *universe*,
;;; checking if all that are in the first are also in the second.
;;; Return T if this is the case, NIL otherwise.  This will
;;; always return T if type1 is truly a subtype of type2,
;;; but may return T even if this is not the case.

(defun empirical-subtypep (type1 type2)
  (multiple-value-bind (sub good)
      (subtypep* type1 type2)
    (if good
	sub
      (loop for e in *universe*
	    always (or (not (typep e type1)) (typep e type2))))))

(defun check-type-predicate (P TYPE)
  "Check that a predicate P is the same as #'(lambda (x) (typep x TYPE))
   by applying both to all elements of *UNIVERSE*.  Print message
   when a mismatch is found, and return number of mistakes."

  (when (symbolp p)
    (assert (fboundp p))
    (setf p (symbol-function p)))
  (assert (typep p 'function))

  (loop
      for x in *universe*
      when
	(block failed
	  (let ((p1 (handler-case
			(normally (funcall (the function p) x))
		      (error () (format t "(FUNCALL ~S ~S) failed~%"
					P x)
			(return-from failed t))))
		(p2 (handler-case
			(normally (typep x TYPE))
		      (error () (format t "(TYPEP ~S '~S) failed~%"
					x TYPE)
			(return-from failed t)))))
	      (when (or (and p1 (not p2))
			(and (not p1) p2))
		(format t "(FUNCALL ~S ~S) = ~S, (TYPEP ~S '~S) = ~S~%"
			P x p1 x TYPE p2)
		t)))
	collect x))

;;; We have a common idiom where a guarded predicate should be
;;; true everywhere

(defun check-predicate (predicate &optional guard (universe *universe*))
  "Return all elements of UNIVERSE for which the guard (if present) is false
   and for which PREDICATE is false."
  (remove-if #'(lambda (e) (or (and guard (funcall guard e))
			       (funcall predicate e)))
	     universe))

(declaim (special *catch-error-type*))

(defun catch-continue-debugger-hook (condition dbh)
  "Function that when used as *debugger-hook*, causes
   continuable errors to be continued without user intervention."
  (declare (ignore dbh))
  (let ((r (find-restart 'continue condition)))
    (cond
     ((and *catch-error-type*
	   (not (typep condition *catch-error-type*)))
      (format t "Condition ~S is not a ~A~%" condition *catch-error-type*)
      (cond (r (format t "Its continue restart is ~S~%" r))
	    (t (format t "It has no continue restart~%")))
      (throw 'continue-failed nil))
     (r (invoke-restart r))
     (t (throw 'continue-failed nil)))))

#|
(defun safe (fn &rest args)
  "Apply fn to args, trapping errors.  Convert type-errors to the
   symbol type-error."
  (declare (optimize (safety 3)))
  (handler-case
   (apply fn args)
   (type-error () 'type-error)
   (error (c) c)))
|#

;;; Use the next macro in place of SAFE

(defmacro catch-type-error (form)
"Evaluate form in safe mode, returning its value if there is no error.
If an error does occur, return type-error on TYPE-ERRORs, or the error
condition itself on other errors."
`(locally (declare (optimize (safety 3)))
  (handler-case (normally ,form)
     (type-error () 'type-error)
     (error (c) c))))

(defmacro classify-error* (form)
"Evaluate form in safe mode, returning its value if there is no error.
If an error does occur, return a symbol classify the error, or allow
the condition to go uncaught if it cannot be classified."
`(locally (declare (optimize (safety 3)))
  (handler-case (normally ,form)
     (undefined-function () 'undefined-function)
     (program-error () 'program-error)
     (package-error () 'package-error)
     (type-error    () 'type-error)
     (control-error () 'control-error)
     (parse-error   () 'parse-error)
     (stream-error  () 'stream-error)
     (reader-error  () 'reader-error)
     (file-error    () 'file-error)
     (cell-error    () 'cell-error)
     (division-by-zero () 'division-by-zero)
     (floating-point-overflow () 'floating-point-overflow)
     (floating-point-underflow () 'floating-point-underflow)
     (arithmetic-error () 'arithmetic-error)
     (error         () 'error)
  )))

(defmacro classify-error (form)
  `(classify-error** ',form))

(defmacro signals-error-always (form error-name)
  `(values
    (signals-error ,form ,error-name)
    (signals-error ,form ,error-name :safety 0)))


(declaim (special *mini-universe*))

(defun check-type-error* (pred-fn guard-fn &optional (universe *mini-universe*))
  "Check that for all elements in some set, either guard-fn is true or
   pred-fn signals a type error."
  (let (val)
    (loop for e in universe
	  unless (or (funcall guard-fn e)
		     (equal
		      (setf val (multiple-value-list
				 (signals-type-error x e (funcall pred-fn x) :inline t)))
		      '(t)))
	collect (list e val))))

(defmacro check-type-error (&body args)
  `(locally (declare (optimize safety)) (check-type-error* ,@args)))

(defun printable-p (obj)
  "Returns T iff obj can be printed to a string."
  (with-standard-io-syntax
   (let ((*print-readably* nil)
	 (*print-escape* nil))
     (declare (optimize safety))
     (handler-case (and (stringp (write-to-string obj)) t)
		   (condition (c) (declare (ignore c)) nil)))))

;;;
;;; The function SUBTYPEP should return two generalized booleans.
;;; This auxiliary function returns booleans instead
;;; (which makes it easier to write tests).
;;;
(defun subtypep* (type1 type2)
  (apply #'values
	 (mapcar #'notnot
		 (multiple-value-list (subtypep type1 type2)))))

(defun subtypep*-or-fail (type1 type2)
  (let ((results (multiple-value-list (subtypep type1 type2))))
    (and (= (length results) 2)
	 (or (not (second results))
	     (notnot (first results))))))

(defun subtypep*-not-or-fail (type1 type2)
  (let ((results (multiple-value-list (subtypep type1 type2))))
    (and (= (length results) 2)
	 (or (not (second results))
	     (not (first results))))))

;; (declaim (ftype (function (&rest function) (values function &optional))
;;		compose))

(defun compose (&rest fns)
  (let ((rfns (reverse fns)))
    #'(lambda (x) (loop for f
			in rfns do (setf x (funcall (the function f) x))) x)))

(defun evendigitp (c)
  (notnot (find c "02468")))

(defun odddigitp (c)
  (notnot (find c "13579")))

(defun nextdigit (c)
  (cadr (member c '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))))

(defun is-eq-p (x) #'(lambda (y) (eqt x y)))
(defun is-not-eq-p (x) #'(lambda (y) (not (eqt x y))))

(defun is-eql-p (x) #'(lambda (y) (eqlt x y)))
(defun is-not-eql-p (x) #'(lambda (y) (not (eqlt x y))))

(defun onep (x) (eql x 1))

(defun char-invertcase (c)
  (if (upper-case-p c) (char-downcase c)
    (char-upcase c)))

(defun string-invertcase (s)
  (map 'string #'char-invertcase s))

(defun symbol< (x &rest args)
  (apply #'string< (symbol-name x) (mapcar #'symbol-name args)))


(defun make-list-expr (args)
  "Build an expression for computing (LIST . args), but that evades
   CALL-ARGUMENTS-LIMIT."
  (if (cddddr args)
      (list 'list*
	    (first args) (second args) (third args) (fourth args)
	    (make-list-expr (cddddr args)))
    (cons 'list args)))  

(defparameter +standard-chars+
  (coerce
  "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789~!@#$%^&*()_+|\\=-`{}[]:\";'<>?,./
 " 'simple-base-string))

(defparameter
  +base-chars+ #.(coerce
		  (concatenate 'string
			       "abcdefghijklmnopqrstuvwxyz"
			       "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
			       "0123456789"
			       "<,>.?/\"':;[{]}~`!@#$%^&*()_-+= \\|")
		  'simple-base-string))
		  

(declaim (type simple-base-string +base-chars+))

(defparameter +num-base-chars+ (length +base-chars+))

(defparameter +alpha-chars+ (subseq +standard-chars+ 0 52))
(defparameter +lower-case-chars+ (subseq +alpha-chars+ 0 26))
(defparameter +upper-case-chars+ (subseq +alpha-chars+ 26 52))
(defparameter +alphanumeric-chars+ (subseq +standard-chars+ 0 62))
(defparameter +digit-chars+ "0123456789")
(defparameter +extended-digit-chars+ (coerce
				      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
				      'simple-base-string))

(declaim (type simple-base-string +alpha-chars+ +lower-case-chars+
	       +upper-case-chars+ +alphanumeric-chars+ +extended-digit-chars+
	       +standard-chars+))

(defparameter +code-chars+
  (coerce (loop for i from 0 below 256
		for c = (code-char i)
		when c collect c)
	  'simple-string))

(declaim (type simple-string +code-chars+))

(defparameter +rev-code-chars+ (reverse +code-chars+))

;;; Used in checking for continuable errors

(defun has-non-abort-restart (c)
  (throw 'handled
	 (if (position 'abort (the list (compute-restarts c))
		       :key #'restart-name :test-not #'eq)
	     'success
	   'fail)))

(defmacro handle-non-abort-restart (&body body)
  `(catch 'handled
     (handler-bind ((error #'has-non-abort-restart))
		   ,@body)))

;;; used in elt.lsp
(defun elt-v-6-body ()
  (let ((x (make-int-list 1000)))
    (let ((a (make-array '(1000) :initial-contents x)))
      (loop
	  for i from 0 to 999 do
	    (unless (eql i (elt a i)) (return nil))
	  finally (return t)))))

(defun make-adj-array (n &key initial-contents)
  (if initial-contents
      (make-array n :adjustable t :initial-contents initial-contents)
    (make-array n :adjustable t)))

;;; used in elt.lsp
(defun elt-adj-array-6-body ()
  (let ((x (make-int-list 1000)))
    (let ((a (make-adj-array '(1000) :initial-contents x)))
      (loop
	  for i from 0 to 999 do
	    (unless (eql i (elt a i)) (return nil))
	  finally (return t)))))

(defparameter *displaced* (make-int-array 100000))

(defun make-displaced-array (n displacement)
  (make-array n :displaced-to *displaced*

	      :displaced-index-offset displacement))

;;; used in fill.lsp
(defun array-unsigned-byte-fill-test-fn (byte-size &rest fill-args)
  (let* ((a (make-array '(5) :element-type (list 'unsigned-byte byte-size)
			:initial-contents '(1 2 3 4 5)))
	 (b (apply #'fill a fill-args)))
    (values (eqt a b)
	    (map 'list #'identity a))))

;;; used in fill-strings.lsp
(defun array-string-fill-test-fn (a &rest fill-args)
  (setq a (copy-seq a))
  (let ((b (apply #'fill a fill-args)))
    (values (eqt a b) b)))

;;; From types-and-class.lsp

(defparameter +float-types+
  '(long-float double-float short-float single-float))

(defparameter *subtype-table*
(let ((table
       '(
	 (null symbol)
	 (symbol t)
	 (boolean symbol)
	 (standard-object t)
	 (function t)
	 (compiled-function function)
	 (generic-function function)
	 (standard-generic-function generic-function)
	 (class standard-object)
	 (built-in-class class)
	 (structure-class class)
	 (standard-class class)
	 (method standard-object)
	 (standard-method method)
	 (structure-object t)
	 (method-combination t)
	 (condition t)
	 (serious-condition condition)
	 (error serious-condition)
	 (type-error error)
	 (simple-type-error type-error)
	 (simple-condition condition)
	 (simple-type-error simple-condition)
	 (parse-error error)
	 (hash-table t)
	 (cell-error error)
	 (unbound-slot cell-error)
	 (warning condition)
	 (style-warning warning)
	 (storage-condition serious-condition)
	 (simple-warning warning)
	 (simple-warning simple-condition)
	 (keyword symbol)
	 (unbound-variable cell-error)
	 (control-error error)
	 (program-error error)
	 (undefined-function cell-error)
	 (package t)
	 (package-error error)
	 (random-state t)
	 (number t)
	 (real number)
	 (complex number)
	 (float real)
	 (short-float float)
	 (single-float float)
	 (double-float float)
	 (long-float float)
	 (rational real)
	 (integer rational)
	 (ratio rational)
	 (signed-byte integer)
	 (integer signed-byte)
	 (unsigned-byte signed-byte)
	 (bit unsigned-byte)
	 (fixnum integer)
	 (bignum integer)
	 (bit fixnum)
	 (arithmetic-error error)
	 (division-by-zero arithmetic-error)
	 (floating-point-invalid-operation arithmetic-error)
	 (floating-point-inexact arithmetic-error)
	 (floating-point-overflow arithmetic-error)
	 (floating-point-underflow arithmetic-error)
	 (character t)
	 (base-char character)
	 (standard-char base-char)
	 (extended-char character)
	 (sequence t)
	 (list sequence)
	 (null list)
	 (null boolean)
	 (cons list)
	 (array t)
	 (simple-array array)
	 (vector sequence)
	 (vector array)
	 (string vector)
	 (bit-vector vector)
	 (simple-vector vector)
	 (simple-vector simple-array)
	 (simple-bit-vector bit-vector)
	 (simple-bit-vector simple-array)
	 (base-string string)
	 (simple-string string)
	 (simple-string simple-array)
	 (simple-base-string base-string)
	 (simple-base-string simple-string)
	 (pathname t)
	 (logical-pathname pathname)
	 (file-error error)
	 (stream t)
	 (broadcast-stream stream)
	 (concatenated-stream stream)
	 (echo-stream stream)
	 (file-stream stream)
	 (string-stream stream)
	 (synonym-stream stream)
	 (two-way-stream stream)
	 (stream-error error)
	 (end-of-file stream-error)
	 (print-not-readable error)
	 (readtable t)
	 (reader-error parse-error)
	 (reader-error stream-error)
	 )))
  (when (subtypep* 'character 'base-char)
    (setq table
	  (append
	   '((character base-char)
	     ;; (string base-string)
	     ;; (simple-string simple-base-string)
	     )
	   table)))
  
  table))

(defun trim-list (list n)
  (let ((len (length list)))
    (if (<= len n) list
      (append (subseq list 0 n)
	      (format nil "And ~A more omitted." (- len n))))))

(defun is-t-or-nil (e)
  (or (eqt e t) (eqt e nil)))

(defun is-builtin-class (type)
  (when (symbolp type) (setq type (find-class type nil)))
  (typep type 'built-in-class))

(defun even-size-p (a)
  (some #'evenp (array-dimensions a)))


(defun safe-elt (x n)
  (classify-error* (elt x n)))

(defmacro defstruct* (&body args)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (handler-case (eval '(defstruct ,@args))
		      (serious-condition () nil))))

(defun safely-delete-package (package-designator)
  (let ((package (find-package package-designator)))
    (when package
      (let ((used-by (package-used-by-list package)))
	(dolist (using-package used-by)
	  (unuse-package package using-package)))
      (delete-package package))))

#-(or allegro openmcl lispworks)
(defun delete-all-versions (pathspec)
  "Replace the versions field of the pathname specified by pathspec with
   :wild, and delete all the files this refers to."
  (let* ((wild-pathname (make-pathname :version :wild :defaults (pathname pathspec)))
	 (truenames (directory wild-pathname)))
    (mapc #'delete-file truenames)))    

;;; This is a hack to get around an ACL bug; OpenMCL also apparently
;;; needs it
#+(or allegro openmcl lispworks)
(defun delete-all-versions (pathspec)
  (when (probe-file pathspec) (delete-file pathspec)))

(defconstant +fail-count-limit+ 20)

(defun frob-simple-condition (c expected-fmt &rest expected-args)
  "Try out the format control and format arguments of a simple-condition C,
   but make no assumptions about what they print as, only that they
   do print."
  (declare (ignore expected-fmt expected-args))
  (and (typep c 'simple-condition)
       (let ((fc (simple-condition-format-control c))
	     (args (simple-condition-format-arguments c)))
	 (and
	  (stringp (apply #'format nil fc args))
	  t))))

(defun frob-simple-error (c expected-fmt &rest expected-args)
  (and (typep c 'simple-error)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defun frob-simple-warning (c expected-fmt &rest expected-args)
  (and (typep c 'simple-warning)
       (apply #'frob-simple-condition c expected-fmt expected-args)))

(defparameter *array-element-types*
  '(t (integer 0 0)
      bit (unsigned-byte 8) (unsigned-byte 16)
      (unsigned-byte 32) float short-float
      single-float double-float long-float
      nil character base-char symbol boolean null))

(defun collect-properties (plist prop)
  "Collect all the properties in plist for a property prop."
  (loop for e on plist by #'cddr
	when (eql (car e) prop)
	collect (cadr e)))

(defmacro def-macro-test (test-name macro-form)
  (let ((macro-name (car macro-form)))
    (assert (symbolp macro-name))
    `(define-test ,test-name
       (assert-equal
        '(t t t)
        (list
          (signals-error (funcall (macro-function ',macro-name))
                         program-error)
          (signals-error (funcall (macro-function ',macro-name)
                                  ',macro-form)
                         program-error)
          (signals-error (funcall (macro-function ',macro-name)
                                  ',macro-form nil nil)
                         program-error))))))

(defun typep* (element type)
  (not (not (typep element type))))

(defun applyf (fn &rest args)
  (etypecase fn
    (symbol
     #'(lambda (&rest more-args) (apply (the symbol fn) (append args more-args))))
    (function
     #'(lambda (&rest more-args) (apply (the function fn) (append args more-args))))))

(defun slot-boundp* (object slot)
  (notnot (slot-boundp object slot)))

(defun slot-exists-p* (object slot)
  (notnot (slot-exists-p object slot)))

(defun map-slot-boundp* (c slots)
  (mapcar (applyf #'slot-boundp c) slots))

(defun map-slot-exists-p* (c slots)
  (mapcar (applyf #'slot-exists-p* c) slots))

(defun map-slot-value (c slots)
  (mapcar (applyf #'slot-value c) slots))

(defun map-typep* (object types)
  (mapcar (applyf #'typep* object) types))

(defun slot-value-or-nil (object slot-name)
  (and (slot-exists-p object slot-name)
       (slot-boundp object slot-name)
       (slot-value object slot-name)))

(defun is-noncontiguous-sublist-of (list1 list2)
  (loop
   for x in list1
   do (loop
       when (null list2) do (return-from is-noncontiguous-sublist-of nil)
       when (eql x (pop list2)) do (return))
   finally (return t)))

;;; This defines a new metaclass to allow us to get around
;;; the restriction in section 11.1.2.1.2, bullet 19 in some
;;; object system tests

;;; (when (typep (find-class 'standard-class) 'standard-class)
;;;  (defclass substandard-class (standard-class) ())
;;;  (defparameter *can-define-metaclasses* t))

;;; Macro for testing that something is undefined but 'harmless'

(defmacro defharmless (name form)
  `(define-test ,name
     (assert-equal
      :good
      (block done
        (let ((*debugger-hook* #'(lambda (&rest args)
                                   (declare (ignore args))
                                   (return-from done :good))))
          (handler-case
              (unwind-protect (eval ',form) (return-from done :good))
            (condition () :good)))))))

(defun rational-safely (x)
  "Rational a floating point number, making sure the rational
   number isn't 'too big'.  This is important in implementations such
   as clisp where the floating bounds can be very large."
  (assert (floatp x))
  (multiple-value-bind (significand exponent sign)
      (integer-decode-float x)
    (let ((limit 1000)
	  (radix (float-radix x)))
      (cond
       ((< exponent (- limit))
	(* significand (expt radix (- limit)) sign))
       ((> exponent limit)
	(* significand (expt radix limit) sign))
       (t (rational x))))))

(declaim (special *similarity-list*))

(defun is-similar (x y)
  (let ((*similarity-list* nil))
    (is-similar* x y)))

(defgeneric is-similar* (x y))

(defmethod is-similar* ((x number) (y number))
  (and (eq (class-of x) (class-of y))
       (= x y)
       t))

(defmethod is-similar* ((x character) (y character))
  (and (char= x y) t))

(defmethod is-similar* ((x symbol) (y symbol))
  (if (null (symbol-package x))
      (and (null (symbol-package y))
	   (is-similar* (symbol-name x) (symbol-name y)))
    ;; I think the requirements for interned symbols in
    ;; 3.2.4.2.2 boils down to EQ after the symbols are in the lisp
    (eq x y))
  t)

(defmethod is-similar* ((x random-state) (y random-state))
  (let ((copy-of-x (make-random-state x))
	(copy-of-y (make-random-state y))
	(bound (1- (ash 1 24))))
    (and
     ;; Try 50 values, and assume the random state are the same
     ;; if all the values are the same.  Assuming the RNG is not
     ;; very pathological, this should be acceptable.
     (loop repeat 50
	   always (eql (random bound copy-of-x)
		       (random bound copy-of-y)))
     t)))

(defmethod is-similar* ((x cons) (y cons))
  (or (and (eq x y) t)
      (and (loop for (x2 . y2) in *similarity-list*
		 thereis (and (eq x x2) (eq y y2)))
	   t)
      (let ((*similarity-list*
	     (cons (cons x y) *similarity-list*)))
	(and (is-similar* (car x) (car y))
	     ;; If this causes stack problems,
	     ;; convert to a loop
	     (is-similar* (cdr x) (cdr y))))))

(defmethod is-similar* ((x vector) (y vector))
  (or (and (eq x y) t)
      (and
       (or (not (typep x 'simple-array))
	   (typep x 'simple-array))
       (= (length x) (length y))
       (is-similar* (array-element-type x)
		    (array-element-type y))
       (loop for i below (length x)
	     always (is-similar* (aref x i) (aref y i)))
       t)))

(defmethod is-similar* ((x array) (y array))
  (or (and (eq x y) t)
      (and
       (or (not (typep x 'simple-array))
	   (typep x 'simple-array))
       (= (array-rank x) (array-rank y))
       (equal (array-dimensions x) (array-dimensions y))
       (is-similar* (array-element-type x)
		    (array-element-type y))
       (let ((*similarity-list*
	      (cons (cons x y) *similarity-list*)))
	 (loop for i below (array-total-size x)
	       always (is-similar* (row-major-aref x i)
				   (row-major-aref y i))))
       t)))

(defmethod is-similar* ((x hash-table) (y hash-table))
  ;; FIXME  Add similarity check for hash tables
  (error "Sorry, we're not computing this yet."))

(defmethod is-similar* ((x pathname) (y pathname))
  (and
   (is-similar* (pathname-host x) (pathname-host y))
   (is-similar* (pathname-device x) (pathname-device y))
   (is-similar* (pathname-directory x) (pathname-directory y))
   (is-similar* (pathname-name x) (pathname-name y))
   (is-similar* (pathname-type x) (pathname-type y))
   (is-similar* (pathname-version x) (pathname-version y))
   t))

(defmethod is-similar* ((x t) (y t))
  (and (eql x y) t))

(defparameter *initial-print-pprint-dispatch* (if (boundp '*print-pprint-dispatch*)
						  *print-pprint-dispatch*
						nil))

(defmacro my-with-standard-io-syntax (&body body)
  `(let ((*package* (find-package "COMMON-LISP-USER"))
	 (*print-array* t)
	 (*print-base* 10)
	 (*print-case* :upcase)
	 (*print-circle* nil)
	 (*print-escape* t)
	 (*print-gensym* t)
	 (*print-length* nil)
	 (*print-level* nil)
	 (*print-lines* nil)
	 (*print-miser-width* nil)
	 (*print-pprint-dispatch* *initial-print-pprint-dispatch*)
	 (*print-pretty* nil)
	 (*print-radix* nil)
	 (*print-readably* t)
	 (*print-right-margin* nil)
	 (*read-base* 10)
	 (*read-default-float-format* 'single-float)
	 (*read-eval* t)
	 (*read-suppress* nil)
	 (*readtable* (copy-readtable nil)))
     ,@body))

;;; Function to produce a non-simple string

(defun make-special-string (string &key fill adjust displace base)
  (let* ((len (length string))
	 (len2 (if fill (+ len 4) len))
	 (etype (if base 'base-char 'character)))
    (if displace
	(let ((s0 (make-array (+ len2 5)
			      :initial-contents
			      (concatenate 'string
					   (make-string 2 :initial-element #\X)
					   string
					   (make-string (if fill 7 3)
							:initial-element #\Y))
			      :element-type etype)))
	  (make-array len2 :element-type etype
		      :adjustable adjust
		      :fill-pointer (if fill len nil)
		      :displaced-to s0
		      :displaced-index-offset 2))
      (make-array len2 :element-type etype
		  :initial-contents
		  (if fill (concatenate 'string string "ZZZZ") string)
		  :fill-pointer (if fill len nil)
		  :adjustable adjust))))

(defmacro do-special-strings ((var string-form &optional ret-form) &body forms)
  (let ((string (gensym))
	(fill (gensym "FILL"))
	(adjust (gensym "ADJUST"))
	(base (gensym "BASE"))
	(displace (gensym "DISPLACE")))
    `(let ((,string ,string-form))
       (dolist (,fill '(nil t) ,ret-form)
	 (dolist (,adjust '(nil t))
	   (dolist (,base '(nil t))
	     (dolist (,displace '(nil t))
	       (let ((,var (make-special-string
			    ,string
			    :fill ,fill :adjust ,adjust
			    :base ,base :displace ,displace)))
		 ,@forms))))))))

(defun make-special-integer-vector (contents &key fill adjust displace (etype 'integer))
  (let* ((len (length contents))
	 (min (reduce #'min contents))
	 (max (reduce #'max contents))
	 (len2 (if fill (+ len 4) len)))
    (unless (and (typep min etype)
		 (typep max etype))
      (setq etype `(integer ,min ,max)))
    (if displace
	(let ((s0 (make-array (+ len2 5)
			      :initial-contents
			      (concatenate 'list
					   (make-list 2 :initial-element
						      (if (typep 0 etype) 0 min))
					   contents
					   (make-list (if fill 7 3)
						      :initial-element
						      (if (typep 1 etype) 1 max)))
			      :element-type etype)))
	  (make-array len2 :element-type etype
		      :adjustable adjust
		      :fill-pointer (if fill len nil)
		      :displaced-to s0
		      :displaced-index-offset 2))
      (make-array len2 :element-type etype
		  :initial-contents
		  (if fill (concatenate 'list
					contents
					(make-list 4 :initial-element
						   (if (typep 2 etype) 2 (floor (+ min max) 2))))
		    contents)
		  :fill-pointer (if fill len nil)
		  :adjustable adjust))))

(defmacro do-special-integer-vectors ((var vec-form &optional ret-form) &body forms)
  (let ((vector (gensym))
	(fill (gensym "FILL"))
	(adjust (gensym "ADJUST"))
	(etype (gensym "ETYPE"))
	(displace (gensym "DISPLACE")))
    `(let ((,vector ,vec-form))
       (dolist (,fill '(nil t) ,ret-form)
	 (dolist (,adjust '(nil t))
	   (dolist (,etype ',(append (loop for i from 1 to 32 collect `(unsigned-byte ,i))
				     (loop for i from 2 to 32 collect `(signed-byte ,i))
				     '(integer)))
	     (dolist (,displace '(nil t))
	       (let ((,var (make-special-integer-vector
			    ,vector
			    :fill ,fill :adjust ,adjust
			    :etype ,etype :displace ,displace)))
		 ,@forms))))))))

;;; Return T if arg X is a string designator in this implementation

(defun string-designator-p (x)
  (handler-case
   (progn (string x) t)
   (error nil)))

;;; Approximate comparison of numbers
#|
(defun approx= (x y)
  (let ((eps 1.0d-4))
    (<= (abs (- x y))
       (* eps (max (abs x) (abs y))))))
|#

;;; Approximate equality function
(defun approx= (x y &optional (eps (epsilon x)))
  (<= (abs (/ (- x y) (max (abs x) 1))) eps))

(defun epsilon (number)
  (etypecase number
    (complex (* 2 (epsilon (realpart number)))) ;; crude
    (short-float short-float-epsilon)
    (single-float single-float-epsilon)
    (double-float double-float-epsilon)
    (long-float long-float-epsilon)
    (rational 0)))

(defun negative-epsilon (number)
  (etypecase number
    (complex (* 2 (negative-epsilon (realpart number)))) ;; crude
    (short-float short-float-negative-epsilon)
    (single-float single-float-negative-epsilon)
    (double-float double-float-negative-epsilon)
    (long-float long-float-negative-epsilon)
    (rational 0)))

(defun sequencep (x) (typep x 'sequence))

(defun typef (type) #'(lambda (x) (typep x type)))

(defun package-designator-p (x)
  "TRUE if x could be a package designator.  The package need not
   actually exist."
  (or (packagep x)
      (handler-case (and (locally (declare (optimize safety))
				  (string x))
			 t)
		    (type-error () nil))))

(defmacro def-fold-test (name form)
  "Create a test that FORM, which should produce a fresh value,
   does not improperly introduce sharing during constant folding."
  `(define-test ,name
     (assert-equal 
      nil
      (flet ((%f () (declare (optimize (speed 3) (safety 0) (space 0)
                                       (compilation-speed 0) (debug 0)))
	       ,form))
        (eq (%f) (%f))))))

;;; Macro used in tests of environments in system macros
;;; This was inspired by a bug in ACL 8.0 beta where CONSTANTP
;;; was being called in some system macros without the proper
;;; environment argument

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

;;; These tests have been taken from the cxr.lisp file of the
;;; ansi test suite by Paul Dietz, and have been adapted for 
;;; use with lisp-unit.

(define-test cons.25
  (assert-equal 'a
                (caar '((a)))))

(define-test cons.26
  (assert-equal 'b
                (cdar '((a . b)))))

(define-test cons.27
  (assert-equal 'b
                (cadr '(a b))))

(define-test cons.28
  (assert-equal 'c
                (cddr '(a b . c))))

(define-test cons.29
  (assert-equal 'a
                (caaar '(((a))))))

(define-test cons.30
  (assert-equal 'b
                (cdaar '(((a . b))))))

(define-test cons.31
  (assert-equal 'b
                (cadar (cons (cons 'a (cons 'b 'c)) 'd))))

(define-test cons.32
  (assert-equal 'c
                (cddar (cons (cons 'a (cons 'b 'c)) 'd))))

(define-test cons.33
  (assert-equal 'b
                (caadr (cons 'a (cons (cons 'b 'c) 'd)))))

(define-test cons.34
  (assert-equal 'c
                (caddr (cons 'a (cons 'b (cons 'c 'd))))))

(define-test cons.36
  (assert-equal 'c
                (cdadr (cons 'a (cons (cons 'b 'c) 'd)))))

(define-test cons.37
  (assert-equal 'd
                (cdddr (cons 'a (cons 'b (cons 'c 'd))))))

(defvar *cons-test-4*
  (cons (cons (cons (cons 'a 'b)
		    (cons 'c 'd))
	      (cons (cons 'e 'f)
		    (cons 'g 'h)))
	(cons (cons (cons 'i 'j)
		    (cons 'k 'l))
	      (cons (cons 'm 'n)
		    (cons 'o 'p)))))

(define-test cons.38
  (assert-equal 'a
                (caaaar *cons-test-4*)))

(define-test cons.39
  (assert-equal 'b
                (cdaaar *cons-test-4*)))

(define-test cons.40
  (assert-equal 'c
                (cadaar *cons-test-4*)))

(define-test cons.41
  (assert-equal 'd
                (cddaar *cons-test-4*)))

(define-test cons.42
  (assert-equal 'e
                (caadar *cons-test-4*)))

(define-test cons.43
  (assert-equal 'f
                (cdadar *cons-test-4*)))

(define-test cons.44
  (assert-equal 'g
                (caddar *cons-test-4*)))

(define-test cons.45
  (assert-equal 'h
                (cdddar *cons-test-4*)))

;;;

(define-test cons.46
  (assert-equal 'i
                (caaadr *cons-test-4*)))

(define-test cons.47
  (assert-equal 'j
                (cdaadr *cons-test-4*)))

(define-test cons.48
  (assert-equal 'k
                (cadadr *cons-test-4*)))

(define-test cons.49
  (assert-equal 'l
                (cddadr *cons-test-4*)))

(define-test cons.50
  (assert-equal 'm
                (caaddr *cons-test-4*)))

(define-test cons.51
  (assert-equal 'n
                (cdaddr *cons-test-4*)))

(define-test cons.52
  (assert-equal 'o
                (cadddr *cons-test-4*)))

(define-test cons.53
  (assert-equal 'p
                (cddddr *cons-test-4*)))

;;; Error checking of c*r functions

(define-test caar.error.1
  (assert-error 'type-error (caar 'a)))

(define-test caar.error.2
  (assert-error 'type-error (caar '(a))))

(define-test cadr.error.1
  (assert-error 'type-error (cadr 'a)))

(define-test cadr.error.2
  (assert-error 'type-error (cadr '(a . b))))

(define-test cdar.error.1
  (assert-error 'type-error (cdar 'a)))

(define-test cdar.error.2
  (assert-error 'type-error (cdar '(a . b))))

(define-test cddr.error.1
  (assert-error 'type-error (cddr 'a)))

(define-test cddr.error.2
  (assert-error 'type-error (cddr '(a . b))))

(define-test caaar.error.1
  (assert-error 'type-error (caaar 'a)))

(define-test caaar.error.2
  (assert-error 'type-error (caaar '(a))))

(define-test caaar.error.3
  (assert-error 'type-error (caaar '((a)))))

(define-test caadr.error.1
  (assert-error 'type-error (caadr 'a)))

(define-test caadr.error.2
  (assert-error 'type-error (caadr '(a . b))))

(define-test caadr.error.3
  (assert-error 'type-error (caadr '(a . (b)))))

(define-test cadar.error.1
  (assert-error 'type-error (cadar 'a)))

(define-test cadar.error.2
  (assert-error 'type-error (cadar '(a . b))))

(define-test cadar.error.3
  (assert-error 'type-error (cadar '((a . c) . b))))

(define-test caddr.error.1
  (assert-error 'type-error (caddr 'a)))

(define-test caddr.error.2
  (assert-error 'type-error (caddr '(a . b))))

(define-test caddr.error.3
  (assert-error 'type-error (caddr '(a c . b))))

(define-test cdaar.error.1
  (assert-error 'type-error (cdaar 'a)))

(define-test cdaar.error.2
  (assert-error 'type-error (cdaar '(a))))

(define-test cdaar.error.3
  (assert-error 'type-error (cdaar '((a . b)))))

(define-test cdadr.error.1
  (assert-error 'type-error (cdadr 'a)))

(define-test cdadr.error.2
  (assert-error 'type-error (cdadr '(a . b))))

(define-test cdadr.error.3
  (assert-error 'type-error (cdadr '(a b . c))))

(define-test cddar.error.1
  (assert-error 'type-error (cddar 'a)))

(define-test cddar.error.2
  (assert-error 'type-error (cddar '(a . b))))

(define-test cddar.error.3
  (assert-error 'type-error (cddar '((a . b) . b))))

(define-test cdddr.error.1
  (assert-error 'type-error (cdddr 'a)))

(define-test cdddr.error.2
  (assert-error 'type-error (cdddr '(a . b))))

(define-test cdddr.error.3
  (assert-error 'type-error (cdddr '(a c . b))))

;;

(define-test caaaar.error.1
  (assert-error 'type-error (caaaar 'a)))

(define-test caaaar.error.2
  (assert-error 'type-error (caaaar '(a))))

(define-test caaaar.error.3
  (assert-error 'type-error (caaaar '((a)))))

(define-test caaaar.error.4
  (assert-error 'type-error (caaaar '(((a))))))

(define-test caaadr.error.1
  (assert-error 'type-error (caaadr 'a)))

(define-test caaadr.error.2
  (assert-error 'type-error (caaadr '(a . b))))

(define-test caaadr.error.3
  (assert-error 'type-error (caaadr '(a . (b)))))

(define-test caaadr.error.4
  (assert-error 'type-error (caaadr '(a . ((b))))))

(define-test caadar.error.1
  (assert-error 'type-error (caadar 'a)))

(define-test caadar.error.2
  (assert-error 'type-error (caadar '(a . b))))

(define-test caadar.error.3
  (assert-error 'type-error (caadar '((a . c) . b))))

(define-test caadar.error.4
  (assert-error 'type-error (caadar '((a . (c)) . b))))

(define-test caaddr.error.1
  (assert-error 'type-error (caaddr 'a)))

(define-test caaddr.error.2
  (assert-error 'type-error (caaddr '(a . b))))

(define-test caaddr.error.3
  (assert-error 'type-error (caaddr '(a c . b))))

(define-test caaddr.error.4
  (assert-error 'type-error (caaddr '(a c . (b)))))

(define-test cadaar.error.1
  (assert-error 'type-error (cadaar 'a)))

(define-test cadaar.error.2
  (assert-error 'type-error (cadaar '(a))))

(define-test cadaar.error.3
  (assert-error 'type-error (cadaar '((a . b)))))

(define-test cadaar.error.4
  (assert-error 'type-error (cadaar '((a . (b))))))

(define-test cadadr.error.1
  (assert-error 'type-error (cadadr 'a)))

(define-test cadadr.error.2
  (assert-error 'type-error (cadadr '(a . b))))

(define-test cadadr.error.3
  (assert-error 'type-error (cadadr '(a b . c))))

(define-test cadadr.error.4
  (assert-error 'type-error (cadadr '(a (b . e) . c))))

(define-test caddar.error.1
  (assert-error 'type-error (caddar 'a)))

(define-test caddar.error.2
  (assert-error 'type-error (caddar '(a . b))))

(define-test caddar.error.3
  (assert-error 'type-error (caddar '((a . b) . b))))

(define-test caddar.error.4
  (assert-error 'type-error (caddar '((a  b . c) . b))))

(define-test cadddr.error.1
  (assert-error 'type-error (cadddr 'a)))

(define-test cadddr.error.2
  (assert-error 'type-error (cadddr '(a . b))))

(define-test cadddr.error.3
  (assert-error 'type-error (cadddr '(a c . b))))

(define-test cadddr.error.4
  (assert-error 'type-error (cadddr '(a c e . b))))

(define-test cdaaar.error.1
  (assert-error 'type-error (cdaaar 'a)))

(define-test cdaaar.error.2
  (assert-error 'type-error (cdaaar '(a))))

(define-test cdaaar.error.3
  (assert-error 'type-error (cdaaar '((a)))))

(define-test cdaaar.error.4
  (assert-error 'type-error (cdaaar '(((a . b))))))

(define-test cdaadr.error.1
  (assert-error 'type-error (cdaadr 'a)))

(define-test cdaadr.error.2
  (assert-error 'type-error (cdaadr '(a . b))))

(define-test cdaadr.error.3
  (assert-error 'type-error (cdaadr '(a . (b)))))

(define-test cdaadr.error.4
  (assert-error 'type-error (cdaadr '(a . ((b . c))))))

(define-test cdadar.error.1
  (assert-error 'type-error (cdadar 'a)))

(define-test cdadar.error.2
  (assert-error 'type-error (cdadar '(a . b))))

(define-test cdadar.error.3
  (assert-error 'type-error (cdadar '((a . c) . b))))

(define-test cdadar.error.4
  (assert-error 'type-error (cdadar '((a . (c . d)) . b))))

(define-test cdaddr.error.1
  (assert-error 'type-error (cdaddr 'a)))

(define-test cdaddr.error.2
  (assert-error 'type-error (cdaddr '(a . b))))

(define-test cdaddr.error.3
  (assert-error 'type-error (cdaddr '(a c . b))))

(define-test cdaddr.error.4
  (assert-error 'type-error (cdaddr '(a c b . d))))

(define-test cddaar.error.1
  (assert-error 'type-error (cddaar 'a)))

(define-test cddaar.error.2
  (assert-error 'type-error (cddaar '(a))))

(define-test cddaar.error.3
  (assert-error 'type-error (cddaar '((a . b)))))

(define-test cddaar.error.4
  (assert-error 'type-error (cddaar '((a . (b))))))

(define-test cddadr.error.1
  (assert-error 'type-error (cddadr 'a)))

(define-test cddadr.error.2
  (assert-error 'type-error (cddadr '(a . b))))

(define-test cddadr.error.3
  (assert-error 'type-error (cddadr '(a b . c))))

(define-test cddadr.error.4
  (assert-error 'type-error (cddadr '(a (b . e) . c))))

(define-test cdddar.error.1
  (assert-error 'type-error (cdddar 'a)))

(define-test cdddar.error.2
  (assert-error 'type-error (cdddar '(a . b))))

(define-test cdddar.error.3
  (assert-error 'type-error (cdddar '((a . b) . b))))

(define-test cdddar.error.4
  (assert-error 'type-error (cdddar '((a  b . c) . b))))

(define-test cddddr.error.1
  (assert-error 'type-error (cddddr 'a)))

(define-test cddddr.error.2
  (assert-error 'type-error (cddddr '(a . b))))

(define-test cddddr.error.3
  (assert-error 'type-error (cddddr '(a c . b))))

(define-test cddddr.error.4
  (assert-error 'type-error (cddddr '(a c e . b))))

;;; A scaffold is a structure that is used to remember the object
;;; identities of the cons cells in a (noncircular) data structure.
;;; This lets us check if the data structure has been changed by
;;; an operation.
;;;

(defstruct scaffold
  node
  car
  cdr)

(defun make-scaffold-copy (x)
  "Make a tree that will be used to check if a tree has been changed."
  (if
      (consp x)
      (make-scaffold :node x
		     :car (make-scaffold-copy (car x))
		     :cdr (make-scaffold-copy (cdr x)))
    (make-scaffold :node x
		   :car nil
		   :cdr nil)))

(defun check-scaffold-copy (x xcopy)
  "Return t if xcopy were produced from x by make-scaffold-copy,
   and none of the cons cells in the tree rooted at x have been
   changed."

  (and (eq x (scaffold-node xcopy))
       (or
	(not (consp x))
	(and
	 (check-scaffold-copy (car x) (scaffold-car xcopy))
	 (check-scaffold-copy (cdr x) (scaffold-cdr xcopy))))))

(defun create-c*r-test (n)
  (cond
   ((<= n 0) 'none)
   (t
    (cons (create-c*r-test (1- n))
	  (create-c*r-test (1- n))))))

(defun nth-1-body (x)
  (loop
      for e in x
       and i from 0
       count (not (eqt e (nth i x)))))

(defun check-cons-copy (x y)
  "Check that the tree x is a copy of the tree y,
   returning t if it is, nil if not."
  (cond
   ((consp x)
    (and (consp y)
	 (not (eqt x y))
	 (check-cons-copy (car x) (car y))
	 (check-cons-copy (cdr x) (cdr y))))
   ((eqt x y) t)
   (t nil)))

(defun check-sublis (a al &key (key 'no-key) test test-not)
  "Apply sublis al a with various keys.  Check that
   the arguments are not themselves changed.  Return nil
   if the arguments do get changed."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((acopy (make-scaffold-copy a))
	(alcopy (make-scaffold-copy al)))
    (let ((as
	   (apply #'sublis al a
		  `(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eqt key 'no-key) `(:key ,key))))))
      (and
       (check-scaffold-copy a acopy)
       (check-scaffold-copy al alcopy)
       as))))

(defun check-nsublis (a al &key (key 'no-key) test test-not)
  "Apply nsublis al a, copying these arguments first."
  (setf a (copy-tree a))
  (setf al (copy-tree al))
  (let ((as
	 (apply #'sublis (copy-tree al) (copy-tree a)
		`(,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))
		    ,@(unless (eqt key 'no-key) `(:key ,key))))))
    as))

(defun check-subst (new old tree &key (key 'no-key) test test-not)
  "Call subst new old tree, with keyword arguments if present.
   Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(oldcopy (make-scaffold-copy old))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst new old tree
		  `(,@(unless (eqt key 'no-key) `(:key ,key))
		    ,@(when test `(:test ,test))
		    ,@(when test-not `(:test-not ,test-not))))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy old oldcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))


(defun check-subst-if (new pred tree &key (key 'no-key))
  "Call subst-if new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if new pred tree
		  (unless (eqt key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(defun check-subst-if-not (new pred tree &key (key 'no-key))
  "Call subst-if-not new pred tree, with various keyword arguments
   if present.  Check that the arguments are not changed."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (let ((newcopy (make-scaffold-copy new))
	(predcopy (make-scaffold-copy pred))
	(treecopy (make-scaffold-copy tree)))
    (let ((result
	   (apply #'subst-if-not new pred tree
		  (unless (eqt key 'no-key) `(:key ,key)))))
      (and (check-scaffold-copy new newcopy)
	   (check-scaffold-copy pred predcopy)
	   (check-scaffold-copy tree treecopy)
	   result))))

(defun check-nsubst (new old tree &key (key 'no-key) test test-not)
  "Call nsubst new old tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf old (copy-tree old))
  (setf tree (copy-tree tree))
  (apply #'nsubst new old tree
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defun check-nsubst-if (new pred tree &key (key 'no-key))
  "Call nsubst-if new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if new pred tree
	 (unless (eqt key 'no-key) `(:key ,key))))

(defun check-nsubst-if-not (new pred tree &key (key 'no-key))
  "Call nsubst-if-not new pred tree, with keyword arguments if present."
  (setf new (copy-tree new))
  (setf tree (copy-tree tree))
  (apply #'nsubst-if-not new pred tree
		  (unless (eqt key 'no-key) `(:key ,key))))

(defun check-copy-list-copy (x y)
  "Check that y is a copy of the list x."
  (if
      (consp x)
      (and (consp y)
	   (not (eqt x y))
	   (eqt (car x) (car y))
	   (check-copy-list-copy (cdr x) (cdr y)))
    (and (eqt x y) t)))

(defun check-copy-list (x)
  "Apply copy-list, checking that it properly copies,
   and checking that it does not change its argument."
  (let ((xcopy (make-scaffold-copy x)))
    (let ((y (copy-list x)))
      (and
       (check-scaffold-copy x xcopy)
       (check-copy-list-copy x y)
       y))))

(defun append-6-body ()
  (let* ((cal (min 2048 call-arguments-limit))
	 (step (max 1 (floor (/ cal) 64))))
    (loop
     for n from 0
     below cal
     by step
     count
     (not
      (equal
       (apply #'append (loop for i from 1 to n
			     collect '(a)))
       (make-list n :initial-element 'a))))))

(defun is-intersection (x y z)
  "Check that z is the intersection of x and y."
  (and
   (listp x)
   (listp y)
   (listp z)
   (loop for e in x
	 always (or (not (member e y))
		    (member e z)))
   (loop for e in y
	 always (or (not (member e x))
		    (member e z)))
   (loop for e in z
	 always (and (member e x) (member e y)))
   t))

(defun shuffle (x)
  (cond
   ((null x) nil)
   ((null (cdr x)) x)
   (t
    (multiple-value-bind
	(y z)
	(split-list x)
      (append (shuffle y) (shuffle z))))))

(defun split-list (x)
  (cond
   ((null x) (values nil nil))
   ((null (cdr x)) (values x nil))
   (t
    (multiple-value-bind
	(y z)
	(split-list (cddr x))
      (values (cons (car x) y) (cons (cadr x) z))))))

(defun intersection-12-body (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size
			     collect (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size
			     collect (random maxelem state)))))
       (let ((z (intersection x y)))
	 (let ((is-good (is-intersection x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))

(defun nintersection-with-check (x y &key test)
  (let ((ycopy (make-scaffold-copy y)))
    (let ((result (if test
		      (nintersection x y :test test)
		    (nintersection x y))))
      (if (check-scaffold-copy y ycopy)
	  result
	'failed))))

(defun nintersection-12-body (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state t)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size
			     collect (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size
			     collect (random maxelem state)))))
       (let ((z (nintersection-with-check (copy-list x) y)))
	 (when (eqt z 'failed) (return (values x y z)))
	 (let ((is-good (is-intersection x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))


(defun union-with-check (x y &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (cond
		   (test (union x y :test test))
		   (test-not (union x y :test-not test-not))
		   (t (union x y)))))
      (if (and (check-scaffold-copy x xcopy)
	       (check-scaffold-copy y ycopy))
	  result
	'failed))))

(defun union-with-check-and-key (x y key &key test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result  (cond
		   (test (union x y :key key :test test))
		   (test-not (union x y :key key :test-not test-not))
		   (t (union x y :key key)))))
      (if (and (check-scaffold-copy x xcopy)
	       (check-scaffold-copy y ycopy))
	  result
	'failed))))

(defun check-union (x y z)
  (and (listp x)
       (listp y)
       (listp z)
       (loop for e in z always (or (member e x) (member e y)))
       (loop for e in x always (member e z))
       (loop for e in y always (member e z))
       t))

(defun do-random-unions (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (union x y)))
	      (let ((is-good (check-union x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(defun nunion-with-copy (x y &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :test test))
   (test-not (nunion x y :test-not test-not))
   (t (nunion x y))))

(defun nunion-with-copy-and-key (x y key &key test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (cond
   (test (nunion x y :key key :test test))
   (test-not (nunion x y :key key :test-not test-not))
   (t (nunion x y :key key))))

(defun do-random-nunions (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
       for i from 1 to niters do
	  (let ((x (shuffle (loop for j from 1 to size collect
				  (random maxelem state))))
		(y (shuffle (loop for j from 1 to size collect
				  (random maxelem state)))))
	    (let ((z (nunion-with-copy x y)))
	      (let ((is-good (check-union x y z)))
		(unless is-good (return (values x y z)))))))
    nil))

(defun set-difference-with-check (x y &key (key 'no-key)
					   test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-difference
			 x y
			 `(,@(unless (eqt key 'no-key) `(:key ,key))
			   ,@(when test `(:test ,test))
			   ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-difference (x y z &key (key #'identity)
					(test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (member e x :key key :test test))
   (loop for e in x always (or (member e y :key key :test test)
			       (member e z :key key :test test)))
   (loop for e in y never  (member e z :key key :test test))
   t))

(defun do-random-set-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (set-difference-with-check x y)))
	 (let ((is-good (check-set-difference x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
(defun nset-difference-with-check (x y &key (key 'no-key)
				     test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-difference
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

(defun check-nset-difference (x y z &key (key #'identity)
				(test #'eql))
  (and
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (member e x :key key :test test))
   (loop for e in x always (or (member e y :key key :test test)
			       (member e z :key key :test test)))
   (loop for e in y never  (member e z :key key :test test))
   t))

(defun do-random-nset-differences (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (nset-difference-with-check x y)))
	 (let ((is-good (check-nset-difference x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))

(defun set-exclusive-or-with-check (x y &key (key 'no-key)
				      test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result (apply #'set-exclusive-or
			 x y
			 `(,@(unless (eqt key 'no-key) `(:key ,key))
			     ,@(when test `(:test ,test))
			     ,@(when test-not `(:test-not ,test-not))))))  
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	result)
       (t
	'failed)))))

(defun check-set-exclusive-or (x y z &key (key #'identity)
				 (test #'eql))
  (and
   ;; (not (eqt 'failed z))
   (listp x)
   (listp y)
   (listp z)
   (loop for e in z always (or (member e x :key key :test test)
			       (member e y :key key :test test)))
   (loop for e in x always (if (member e y :key key :test test)
			       (not (member e z :key key :test test))
			     (member e z :key key :test test)))
   (loop for e in y always (if (member e x :key key :test test)
			       (not (member e z :key key :test test))
			     (member e z :key key :test test)))
   t))

#|
(defun do-random-set-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (set-exclusive-or-with-check x y)))
	 (let ((is-good (check-set-exclusive-or x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
|#

(defun nset-exclusive-or-with-check (x y &key (key 'no-key)
				       test test-not)
  (setf x (copy-list x))
  (setf y (copy-list y))
  (apply #'nset-exclusive-or
	 x y
	 `(,@(unless (eqt key 'no-key) `(:key ,key))
	     ,@(when test `(:test ,test))
	     ,@(when test-not `(:test-not ,test-not)))))

#|
(defun do-random-nset-exclusive-ors (size niters &optional (maxelem (* 2 size)))
  (let ((state (make-random-state)))
    (loop
     for i from 1 to niters do
     (let ((x (shuffle (loop for j from 1 to size collect
			     (random maxelem state))))
	   (y (shuffle (loop for j from 1 to size collect
			     (random maxelem state)))))
       (let ((z (nset-exclusive-or-with-check x y)))
	 (let ((is-good (check-set-exclusive-or x y z)))
	   (unless is-good (return (values x y z)))))))
    nil))
|#

(defun subsetp-with-check (x y &key (key 'no-key) test test-not)
  (let ((xcopy (make-scaffold-copy x))
	(ycopy (make-scaffold-copy y)))
    (let ((result
	   (apply #'subsetp x y
		  `(,@(unless (eqt key 'no-key)
			`(:key ,key))
		      ,@(when test `(:test ,test))
		      ,@(when test-not `(:test-not ,test-not))))))
      (cond
       ((and (check-scaffold-copy x xcopy)
	     (check-scaffold-copy y ycopy))
	(not (not result)))
       (t 'failed)))))

(defun my-set-exclusive-or (set1 set2 &key key test test-not)

  (assert (not (and test test-not)))

  (cond
   (test-not (when (symbolp test-not)
	       (setq test-not (symbol-function test-not)))
	     (setq test (complement test-not)))
   ((not test) (setq test #'eql)))

  ;;; (when (symbolp test) (setq test (symbol-function test)))
  (etypecase test
    (symbol (setq test (symbol-function test)))
    (function nil))

  (etypecase key
    (null nil)
    (symbol (setq key (symbol-function key)))
    (function nil))

  (let* ((keys1 (if key (mapcar (the function key) set1) set1))
	 (keys2 (if key (mapcar (the function key) set2) set2))
	 (mask1 (make-array (length set1) :element-type 'bit
			    :initial-element 0))
	 (mask2 (make-array (length set2) :element-type 'bit
			    :initial-element 0)))
    (loop for i1 from 0
	  for k1 in keys1
	  do
	  (loop for i2 from 0
		for k2 in keys2
		when (funcall (the function test) k1 k2)
		do (setf (sbit mask1 i1) 1
			 (sbit mask2 i2) 1)))
    (nconc
     (loop for e in set1
	   for i across mask1
	   when (= i 0)
	   collect e)
     (loop for e in set2
	   for i across mask2
	   when (= i 0)
	   collect e))))

(defun make-random-set-exclusive-or-input (n)
  (let* ((set1 (loop for i from 1 to n collect (random n)))
	 (set2 (loop for i from 1 to n collect (random n)))
	 (test-args
	  (random-case nil nil nil
		       (list :test 'eql)
		       (list :test #'eql)
		       (list :test (complement #'eql))))
	 (test-not-args
	  (and (not test-args)
	       (random-case nil nil (list :test-not 'eql)
			    (list :test-not #'eql)
			    (list :test-not (complement #'eql)))))
	 (key-args
	  (random-case nil nil nil nil
		       (list :key nil)
		       (list :key 'identity)
		       (list :key 'not))))
    (list* set1 set2
	  (reduce #'append (random-permute
			    (list test-args test-not-args key-args))))))

(defun random-set-exclusive-or-test (n reps &optional (fn 'set-exclusive-or))
  (let ((actual-fn (etypecase fn
		     (symbol (symbol-function fn))
		     (function fn))))
    (declare (type function actual-fn))
    (loop for i below reps
	  for args = (make-random-set-exclusive-or-input n)
	  for set1 = (car args)
	  for set2 = (cadr args)
	  for result1 = (apply #'remove-duplicates
			       (sort (copy-list (apply #'my-set-exclusive-or args))
				     #'<)
			       (cddr args))
	  for result2 = (apply #'remove-duplicates
			       (sort (copy-list (apply actual-fn
						       (copy-list set1)
						       (copy-list set2)
						       (cddr args)))
				     #'<)
			       (cddr args))
	  unless (equal result1 result2)
	  return (list (list 'remove-duplicates (list 'sort (cons fn args) '<) "...")
		       "actual: " result2 "should be: " result1))))

(defun rev-assoc-list (x)
  (cond
   ((null x) nil)
   ((null (car x))
    (cons nil (rev-assoc-list (cdr x))))
   (t
    (acons (cdar x) (caar x) (rev-assoc-list (cdr x))))))

(defvar *mapc.6-var* nil)
(defun mapc.6-fun (x)
  (push x *mapc.6-var*)
  x)

(define-test append.1
  (assert-equal nil (append)))

(define-test append.2
  (assert-equal 'x (append 'x)))

(define-test append.3
  (assert-equal
   '(a b c d e f g)
   (let ((x (list 'a 'b 'c 'd))
         (y (list 'e 'f 'g)))
     (let ((xcopy (make-scaffold-copy x))
           (ycopy (make-scaffold-copy y)))
       (let ((result (append x y)))
         (and
          (check-scaffold-copy x xcopy)
          (check-scaffold-copy y ycopy)
          result))))))

(define-test append.4
  (assert-equal
   '(a b c d e f g . h)
   (append (list 'a) (list 'b) (list 'c)
           (list 'd) (list 'e) (list 'f)
           (list 'g) 'h)))

(define-test append.5
  (assert-equal 'a (append nil nil nil nil nil nil nil nil 'a)))

(define-test append.6
  (assert-equal 0 (append-6-body)))

;;; Test suggested by Peter Graves
(define-test append.7
  (assert-equal nil
                (let ((x (list 'a 'b 'c 'd)))
                  (eq (append x nil) x))))

;;; Compiler macro expansion in correct env

(define-test append.8
  (assert-equal '(a b c)
                (macrolet ((%m (z) z))
                  (append (expand-in-current-env (%m '(a b c)))))))

(define-test append.9
  (assert-equal '(1 2 3 4 5 6)
                (macrolet ((%m (z) z))
                  (append (expand-in-current-env (%m (list 1 2 3))) (list 4 5 6)))))

(define-test append.10
  (assert-equal '(1 2 3 4 5 6)
                (macrolet ((%m (z) z))
                  (append (list 1 2 3) (expand-in-current-env (%m (list 4 5 6)))))))

;;; Order of evaluation tests

(define-test append.order.1
  (assert-equal
   '((a b c d e f g h i) 3 1 2 3)
   (let ((i 0) x y z)
     (list
       (append (progn (setf x (incf i)) (copy-list '(a b c)))
               (progn (setf y (incf i)) (copy-list '(d e f)))
               (progn (setf z (incf i)) (copy-list '(g h i))))
       i x y z))))

(define-test append.order.2
  (assert-equal '(1 1) (let ((i 0)) (append (list (incf i)) (list i)))))

(def-fold-test append.fold.1 (append '(a b c) nil))
(def-fold-test append.fold.2 (append nil '(x) nil))

;;; Error tests

(define-test append.error.1
  (assert-error 'type-error (append '(a . b) '(z))))

(define-test append.error.2
  (assert-error 'type-error (append '(x y z) '(a . b) '(z))))

;;; This test verifies that append preserves the structure of
;;; the last list.  We use (loop ... append ...) to implement
;;; the append function, and some implementations of loop are
;;; known not to respect that rule. 

(define-test append.sharing
  (assert-equal t
                (let ((list1 '(1 2))
                      (list2 '(3 4)))
                  (eq (cddr (append list1 list2)) list2))))

