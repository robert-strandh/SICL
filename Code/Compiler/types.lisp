(in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFTYPE.

(defmacro deftype (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (type-function ',name)
           ,(cleavir-code-utilities:parse-deftype
             name
             lambda-list
             body))))

;;; The next form requires some explanation.  In the native compiler,
;;; the symbols defmacro and cl:defmacro are the same, so then this
;;; next form only redefines the macro deftype.  In the cross
;;; compiler, however, the two symbols are different.  The effect of
;;; this form, then, is to define a host macro named
;;; sicl-global-environment:deftype and which puts host functions
;;; into the global SICL environment as type functions.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro deftype (name lambda-list &body body)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (type-function ',name)
             ,(cleavir-code-utilities:parse-deftype
               name
               lambda-list
               body)))))

(deftype mod (n)
  `(integer 0 (,n)))

(deftype unsigned-byte (&optional s)
  (if (eq s '*)
      `(integer 0 *)
      `(integer 0 ,(1- (expt 2 s)))))

(deftype signed-byte (&optional s)
  (if (eq s '*)
      `(integer * *)
      `(integer ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s))))))

(deftype vector (&optional element-type size)
  `(array ,element-type (,size)))

(deftype simple-vector (&optional size)
  `(simple-array t (,size)))

(deftype string (&optional size)
  `(array character (,size)))

(deftype simple-string (&optional size)
  `(simple-array character (,size)))

(deftype base-string (&optional size)
  `(array base-char (,size)))

(deftype simple-base-string (&optional size)
  `(simple-array base-char (,size)))

(deftype bit-vector (&optional size)
  `(array bit (,size)))

(deftype simple-bit-vector (&optional size)
  `(simple-array bit (,size)))
