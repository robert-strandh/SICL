(load "packages.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phase 1.  We need to define host classes corresponding to the MOP
;;; classes defined in mop-class-hierarchy.lisp.

;;; These are the symbols that are mentioned explicitly in the file to
;;; be loaded, and that need to come from the host.
(shadowing-import
 '(t nil if let let* setq flet labels return-from setf progn prog1 eval-when
   loop-finish)
 '#:sicl-common-lisp)

(export
 '(t nil if let let* setq flet labels return-from setf progn prog1 eval-when
   loop-finish)
 '#:sicl-common-lisp)

;;; Import functions that have to do with arithmetic and numbers.
(loop for symbol in '(= /= < > <= >= max min minusp plusp zerop
		      floor ceiling truncate round
		      + - * / 1+ 1- abs evenp oddp mod rem)
      do (setf (fdefinition (intern (symbol-name symbol) '#:scl))
	       (fdefinition symbol)))

;;; Import functions that have to do with error signaling.
(loop for symbol in '(error warn)
      do (setf (fdefinition (intern (symbol-name symbol) '#:scl))
	       (fdefinition symbol)))

;;; Import low-level functions on conses.
(loop for symbol in '(cons car cdr rplaca rplacd consp listp atom length)
      do (setf (fdefinition (intern (symbol-name symbol) '#:scl))
	       (fdefinition symbol)))

;;; Import low-level SETF functions on conses.
(loop for symbol in '(car cdr gethash)
      do (setf (fdefinition `(setf ,(intern (symbol-name symbol) '#:scl)))
	       (fdefinition `(setf ,symbol))))

;;; Import some miscellaneous functions.
(loop for symbol in '(fmakunbound aref eql eq equal equalp typep
		      not null apply funcall get-setf-expansion
		      gensym coerce make-hash-table gethash values)
      do (setf (fdefinition (intern (symbol-name symbol) '#:scl))
	       (fdefinition symbol)))

;;; Import some macros.
(loop for symbol in '(incf decf defconstant defparameter defun defgeneric
		      defmethod deftype loop return in-package defpackage
		      defsetf define-setf-expander defmacro
		      define-compiler-macro multiple-value-bind)
      do (setf (macro-function (intern (symbol-name symbol) '#:scl))
	       (macro-function symbol)))

;;; Load SICL module for conditional macros.
(load "../Conditionals/conditionals.lisp")
;;; Load SICL module for high-level functions on CONSes.
(load "../Cons-high/cons-high.lisp")

;;; In phase 1, there is no difference between a built-in class and a
;;; standard class, so we can use CL:DEFCLASS to define it.  However,
;;; we do not want to define the class T, and since it does not
;;; contribute any slots, it is safe to omit it.
(defmacro sicl-clos:define-built-in-class (name superclasses)
  (if (eq name t)
      nil
      `(defclass ,name ,superclasses ())))

;;; Initially, we define DEFCLASS to be a synonym for CL:DEFCLASS,
;;; with two differences.  First, we do not consider metaclasses other
;;; than the default.  Second, since we never actually defined the
;;; class T, we must omit it from the superclasses.
(defmacro scl:defclass (name superclasses slots &rest class-options)
  `(defclass ,name ,(remove t superclasses)
     ,slots
     ,@(remove :metaclass class-options :key #'car)))

(load "mop-class-hierarchy.lisp")

;;; End of phase 1.
;;; We now have a complete class hierarchy corresponding to the 
;;; classes.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;