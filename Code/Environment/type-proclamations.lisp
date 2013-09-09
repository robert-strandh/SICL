(in-package #:sicl-compiler-environment)

(deftype function-name ()
  `(or symbol (cons (eql setf) (cons symbol null))))

(deftype string-designator ()
  `(or string character symbol))

(declaim (ftype (function (symbol) t)
		symbol-value))

(declaim (ftype (function (t symbol) t)
		(setf symbol-value)))

(declaim (ftype (function (symbol) function)
		symbol-function))

(declaim (ftype (function (function symbol) function)
		(setf symbol-function)))

(declaim (ftype (function (function-name) t)
		fdefinition))

(declaim (ftype (function (function function-name) function)
		(setf fdefinition)))

(declaim (ftype (function (t) t)
		proclaim))

;;; FIXME: do something better for the generalized boolean value.
(declaim (ftype (function (symbol) t)
		boundp))

(declaim (ftype (function (symbol) symbol)
		makunbound))

;;; FIXME: do something better for the generalized boolean value.
(declaim (ftype (function (function-name) t)
		fboundp))

(declaim (ftype (function (function-name) function-name)
		fmakunbound))

;;; FIXME: do something better for the optional environment. 
(declaim (ftype (function (symbol &optional t) (or function null))
		macro-function))

(declaim (ftype (function (function symbol &optional null) function)
		(setf macro-function)))

;;; The function IN-PACKAGE-FUNCTION is used in the expansion of
;;; IN-PACKAGE.
(declaim (ftype (function (string-designator) package)
		in-package-function))

;;; The function DEFCONSTANT-FUNCTION is used in the expansion of
;;; DEFCONSTANT.
(declaim (ftype (function (symbol t) symbol)
		defconstant-function))

;;; FIXME: do something better for the optional environment. 
;;; FIXME: do something better for the generalized boolean value.
(declaim (ftype (function (t &optional t) t)
		constantp))

(declaim (ftype (function (symbol) (cons t null))
		find-value-cell))

(declaim (ftype (function (function-name) (cons t null))
		find-function-cell))

(declaim (ftype (function (symbol) null)
		ensure-defined-variable))

;;; FIXME: do something better for the generalized boolean value.
(declaim (ftype (function (symbol) t)
		special-operator-p))
