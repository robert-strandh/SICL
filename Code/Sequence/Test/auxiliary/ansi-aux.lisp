;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Mar 28 17:10:18 1998
;;;; Contains: Aux. functions for CL-TEST

(in-package #:sicl-sequence-test)

(declaim (optimize (safety 3)))

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

;;; Return true if A1 and A2 are arrays with the same rank
;;; and dimensions whose elements are EQUAL

(defun equal-array (a1 a2)
  (and (typep a1 'array)
       (typep a2 'array)
       (= (array-rank a1) (array-rank a2))
       (if (= (array-rank a1) 0)
           (equal (regression-test::my-aref a1) (regression-test::my-aref a2))
         (let ((ad (array-dimensions a1)))
           (and (equal ad (array-dimensions a2))
                (locally
                 (declare (type (array * *) a1 a2))
                 (if (= (array-rank a1) 1)
                     (let ((as (first ad)))
                       (loop
                        for i from 0 below as
                        always (equal (regression-test::my-aref a1 i)
                                      (regression-test::my-aref a2 i))))
                   (let ((as (array-total-size a1)))
                     (and (= as (array-total-size a2))
                          (loop
                           for i from 0 below as
                           always
                           (equal
                            (regression-test::my-row-major-aref a1 i)
                            (regression-test::my-row-major-aref a2 i))
                           ))))))))))

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

(defun classify-error** (form)
  (handler-bind ((warning #'(lambda (c) (declare (ignore c))
                              (muffle-warning))))
                (proclaim '(optimize (safety 3)))
                (classify-error*
                 (if regression-test::*compile-tests*
                     (funcall (compile nil `(lambda ()
                                              (declare (optimize (safety 3)))
                                              ,form)))
                     (eval form))
                 )))

(defmacro classify-error (form)
  `(classify-error** ',form))

;;; The above is badly designed, since it fails when some signals
;;; may be in more than one class/

(defmacro signals-error (form error-name &key (safety 3) (name nil name-p) (inline nil))
  `(handler-bind
    ((warning #'(lambda (c) (declare (ignore c))
                              (muffle-warning))))
    (proclaim '(optimize (safety 3)))
    (handler-case
     (apply #'values
            nil
            (multiple-value-list
             ,(cond
               (inline form)
               (regression-test::*compile-tests*
                `(funcall (compile nil '(lambda ()
                                          (declare (optimize (safety ,safety)))
                                          ,form))))
               (t `(eval ',form)))))
     (,error-name (c)
                  (cond
                   ,@(case error-name
                       (type-error
                        `(((typep (type-error-datum c)
                                  (type-error-expected-type c))
                           (values
                            nil
                            (list (list 'typep (list 'quote
                                                     (type-error-datum c))
                                        (list 'quote
                                              (type-error-expected-type c)))
                                  "==> true")))))
                       ((undefined-function unbound-variable)
                        (and name-p
                             `(((not (eq (cell-error-name c) ',name))
                                (values
                                 nil
                                 (list 'cell-error-name "==>"
                                       (cell-error-name c)))))))
                       ((stream-error end-of-file reader-error)
                        `(((not (streamp (stream-error-stream c)))
                           (values
                            nil
                            (list 'stream-error-stream "==>"
                                  (stream-error-stream c))))))
                       (file-error
                        `(((not (pathnamep (pathname (file-error-pathname c))))
                           (values
                            nil
                            (list 'file-error-pathname "==>"
                                  (file-error-pathname c))))))
                       (t nil))
                   (t (printable-p c)))))))

(defmacro signals-error-always (form error-name)
  `(values
    (signals-error ,form ,error-name)
    (signals-error ,form ,error-name :safety 0)))

(defmacro signals-type-error (var datum-form form &key (safety 3) (inline nil))
  (let ((lambda-form
         `(lambda (,var)
            (declare (optimize (safety ,safety)))
            ,form)))
    `(let ((,var ,datum-form))
       (declare (optimize safety))
       (handler-bind
        ((warning #'(lambda (c) (declare (ignore c))
                      (muffle-warning))))
                                        ; (proclaim '(optimize (safety 3)))
        (handler-case
         (apply #'values
                nil
                (multiple-value-list
                 (funcall
                 ,(cond
                   (inline `(function ,lambda-form))
                   (regression-test::*compile-tests*
                     `(compile nil ',lambda-form))
                   (t `(eval ',lambda-form)))
                  ,var)))
         (type-error
          (c)
          (let ((datum (type-error-datum c))
                (expected-type (type-error-expected-type c)))
            (cond
             ((not (eql ,var datum))
              (list :datum-mismatch ,var datum))
             ((typep datum expected-type)
              (list :is-typep datum expected-type))
             (t (printable-p c))))))))))

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
;;              compose))

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

(defparameter *disjoint-types-list*
    '(cons symbol array
      number character hash-table function readtable package
      pathname stream random-state condition restart))

(defparameter *disjoint-types-list2*
  `((cons (cons t t) (cons t (cons t t)) (eql (nil)))
    (symbol keyword boolean null (eql a) (eql nil) (eql t) (eql *))
    (array vector simple-array simple-vector string simple-string
           base-string simple-base-string (eql #()))
    (character base-char standard-char (eql #\a)
               ,@(if (subtypep 'character 'base-char) nil
                   (list 'extended-char)))
    (function compiled-function generic-function standard-generic-function
              (eql ,#'car))
    (package (eql ,(find-package "COMMON-LISP")))
    (pathname logical-pathname (eql #p""))
    (stream broadcast-stream concatenated-stream echo-stream
            file-stream string-stream synonym-stream two-way-stream)
    (number real complex float integer rational ratio fixnum
            bit (integer 0 100) (float 0.0 100.0) (integer 0 *)
            (rational 0 *) (mod 10)
            (eql 0)
            ,@(and (not (subtypep 'bignum nil))
                   (list 'bignum)))
    (random-state)
    ,*condition-types*
    (restart)
    (readtable)))

(defparameter *types-list3*
  (reduce #'append *disjoint-types-list2* :from-end t))

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
    `(deftest ,test-name
       (values
        (signals-error (funcall (macro-function ',macro-name))
                       program-error)
        (signals-error (funcall (macro-function ',macro-name)
                                ',macro-form)
                       program-error)
        (signals-error (funcall (macro-function ',macro-name)
                                ',macro-form nil nil)
                       program-error))
       t t t)))

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
  `(deftest ,name
     (block done
       (let ((*debugger-hook* #'(lambda (&rest args)
                                  (declare (ignore args))
                                  (return-from done :good))))
         (handler-case
          (unwind-protect (eval ',form) (return-from done :good))
          (condition () :good))))
     :good))

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
  `(deftest ,name
     (flet ((%f () (declare (optimize (speed 3) (safety 0) (space 0)
                                      (compilation-speed 0) (debug 0)))
               ,form))
       (eq (%f) (%f)))
     nil))

;;; Macro used in tests of environments in system macros
;;; This was inspired by a bug in ACL 8.0 beta where CONSTANTP
;;; was being called in some system macros without the proper
;;; environment argument

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))
