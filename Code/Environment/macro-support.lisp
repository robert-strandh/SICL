(cl:in-package #:sicl-standard-environment-macros)

;;;; The macros in this file have an important shared property,
;;;; namely, they all access or modify the global environment at
;;;; compile time.  For the extrinsic compiler, this property places
;;;; some restrictions on how these macros can be written in that they
;;;; can not use standard functions for accessing the environment such
;;;; as CL:COMPILER-MACRO or CL:GET-SETF-EXPANSION.  The reason for
;;;; that restriction is that in the extrinsic compiler, code that is
;;;; executed at compile time is executed by the host system, so that
;;;; standard environment-accessing functions access the host
;;;; environment, whereas we want to access the target environment.
;;;;
;;;; Furthermore, we can not use the target version of the standard
;;;; environment-accessing functions, because they would overwrite the
;;;; corresponding host functions.  It would be possible to use
;;;; different packages, but that solution would complicate the code.
;;;;
;;;; The solution we propose is to introduce a protocol for accessing
;;;; the (fist-class) global environment, and the names of the
;;;; functions of that protocol are in a dedicated package that can be
;;;; defined in the host without any risk of clashes.  In the target
;;;; environment, standard environment-accessing functions are defined
;;;; in terms of the protocol functions.  The macros that access the
;;;; environment at compile time use the protocol directly, without
;;;; using the standard functions.  That way, the target versions of
;;;; the standard environment-accessing functions do not have to exist
;;;; in the extrinsic compiler. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFVAR.
;;;
;;; The HyperSpec says that when DEFVAR is processed as a top-level
;;; form, then the rest of the compilation must treat the variable as
;;; special, but the initial-value form must not be evaluated, and
;;; there must be no assignment of any value to the variable.
;;;
;;; This is not the final version of DEFVAR, because we ignore the
;;; documentation for now.

(defun defvar-expander (name initial-value initial-value-p documentation)
  (declare (ignore documentation))
  (if initial-value-p
      `(progn
         (eval-when (:compile-toplevel)
           (setf (sicl-genv:special-variable
                  ',name
                  (load-time-value (sicl-genv:global-environment))
                  nil)
                 nil)
           ',name)
         (eval-when (:load-toplevel :execute)
           (unless (sicl-genv:boundp
                    ',name
                    (load-time-value (sicl-genv:global-environment)))
             (setf (sicl-genv:special-variable
                    ',name
                    (load-time-value (sicl-genv:global-environment))
                    t)
                   ,initial-value))
           ',name))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (sicl-genv:special-variable
                ',name
                (load-time-value (sicl-genv:global-environment))
                nil)
               nil)
         ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFPARAMETER.
;;;
;;; The HyperSpec says that when DEFPARAMETER is processed as a
;;; top-level form, then the rest of the compilation must treat the
;;; variable as special, but the initial-value form must not be
;;; evaluated, and there must be no assignment of any value to the
;;; variable.
;;;
;;; This is not the final version of DEFPARAMETER, because we ignore
;;; the documentation for now.

(defun defparameter-expander (env name initial-value documentation)
  (declare (ignore documentation))
  `(progn
     (eval-when (:compile-toplevel)
       (setf (sicl-genv:special-variable ',name ,env nil)
             nil))
     (eval-when (:load-toplevel :execute)
       (setf (sicl-genv:special-variable
              ',name
              (load-time-value (sicl-genv:global-environment))
              t)
             ,initial-value)
       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFTYPE.

(defun deftype-expander (name lambda-list body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (sicl-genv:type-expander
            ',name
            (load-time-value (sicl-genv:global-environment)))
           (function ,(cleavir-code-utilities:parse-deftype 
                       name
                       lambda-list
                       body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-COMPILER-MACRO.

(defun define-compiler-macro-expander (name lambda-list body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (sicl-genv:compiler-macro-function
            ',name
            (load-time-value (sicl-genv:global-environment)))
           (function ,(cleavir-code-utilities:parse-macro
                       name
                       lambda-list
                       body)))))
