(cl:in-package #:sicl-boot-phase-0)

(defun fill-environment (client environment)
  (import-from-host environment)
  (sicl-hir-interpreter:fill-environment environment)
  (define-defmacro environment)
  (define-backquote-macros environment)
  (define-setf-macro-function environment)
  (flet ((ld (relative-file-name)
           (format *trace-output* "Loading file ~a~%" relative-file-name)
           (load-file client relative-file-name environment)))
    ;; Load a file containing a definition of the macro LAMBDA.  This
    ;; macro is particularly simple, so it doesn't really matter how
    ;; it is expanded.  This is fortunate, because at the time this
    ;; file is loaded, the definition of DEFMACRO is still one we
    ;; created "manually" and which uses the host compiler to compile
    ;; the macro function in the null lexical environment.  We define
    ;; the macro LAMBDA before we redefine DEFMACRO as a target macro
    ;; because PARSE-MACRO returns a LAMBDA form, so we need this
    ;; macro in order to redefine DEFMACRO.
    (ld "Evaluation-and-compilation/lambda.lisp")
    ;; Load a file containing the definition of the macro
    ;; MULTIPLE-VALUE-BIND.  We need it early because it is used in the
    ;; expansion of SETF, which we also need early for reasons explained
    ;; below.
    (ld "Environment/multiple-value-bind.lisp")
    ;; Load a file containing a definition of the macro SETF.  We need
    ;; the SETF macro early, because it is needed in order to define
    ;; the macro DEFMACRO.  The reason for that, is that the expansion
    ;; of DEFMACRO uses SETF to set the macro function.  We could have
    ;; defined DEFMACRO to call (SETF MACRO-FUNCTION) directly, but
    ;; that would have been less "natural", so we do it this way
    ;; instead.
    (ld "Data-and-control-flow/setf-defmacro.lisp")
    ;; At this point, we have all the ingredients (the macros LAMBDA and
    ;; SETF) in order to redefine the macro DEFMACRO as a native macro.
    ;; SINCE we already have a primitive form of DEFMACRO, we use it to
    ;; define DEFMACRO.  The result of loading this file is that all new
    ;; macros defined subsequently will have their macro functions
    ;; compiled with the target compiler.  However, the macro function of
    ;; DEFMACRO is still compiled with the host compiler.
    (ld "Evaluation-and-compilation/defmacro-defmacro.lisp")
    ;; As mentioned above, at this point, we have a version of DEFMACRO
    ;; that will compile the macro function of the macro definition using
    ;; the target compiler.  However, the macro function of the macro
    ;; DEFMACRO itself is still the result of using the host compiler.
    ;; By loading the definition of DEFMACRO again, we fix this
    ;; "problem".
    (ld "Evaluation-and-compilation/defmacro-defmacro.lisp")
    ;; Up to this point, the macro function of the macro LAMBDA was
    ;; compiled using the host compiler.  Now that we have the final
    ;; version of the macro DEFMACRO, we can reload the file containing
    ;; the definition of the macro LAMBDA, which will cause the macro
    ;; function to be compiled with the target compiler.
    (ld "Evaluation-and-compilation/lambda.lisp")
    ;; Load a file containing the definition of the macro
    ;; MULTIPLE-VALUE-LIST.  This definition is needed, because it is
    ;; used in the expansion of the macro NTH-VALUE loaded below.
    (ld "Data-and-control-flow/multiple-value-list-defmacro.lisp")
    (ld "Data-and-control-flow/multiple-value-call-defmacro.lisp")
    ;; Load a file containing the definition of macro DEFUN.
    (ld "Data-and-control-flow/defun-defmacro.lisp")
    ;; Load a file containing definition of function GET-SETF-EXPANSION.
    ;; We can not use the version of this function provided by the host,
    ;; because it takes an environment argument, and the host version
    ;; does not work with the Cleavir/SICL environment objects.
    (ld "Data-and-control-flow/get-setf-expansion-defun.lisp")
    ;; Load a file containing definitions of standard conditional
    ;; macros, such as AND, OR, CASE, etc.
    (ld "Conditionals/macros.lisp")
    ;; Load a file containing the definitions of the macros DEFVAR,
    ;; DEFPARAMETER, DEFCONSTANT, DEFTYPE, and DEFINE-COMPILER-MACRO.
    (ld "Environment/standard-environment-macros.lisp")
    ;; Load a file containing the definition of the function
    ;; MACROEXPAND-1.
    (ld "Evaluation-and-compilation/macroexpand-1-defun.lisp")
    ;; Load a file containing the definition of the macro DECLAIM.
    (ld "Evaluation-and-compilation/declaim-defmacro.lisp")
    ;; Load a file containing the definitions of the macros INCF and DECF.
    (ld "Arithmetic/incf-decf-defmacro.lisp")
    (ld "Loop/loop-defmacro.lisp")
    ;; Load a file containing the definitions of the macros PUSH and POP.
    (ld "Cons/push-pop-defmacro.lisp")
    ;; Load a file containing the definition of the macro RETURN.
    (ld "Data-and-control-flow/return-defmacro.lisp")
    ;; Load a file containing the definitions of the macros PROG1 and PROG2.
    (ld "Data-and-control-flow/prog1-prog2-defmacro.lisp")
    ;; Load a file containing the definitions of the macros PROG and PROG*.
    (ld "Data-and-control-flow/prog-progstar-defmacro.lisp")
    ;; Load a file containing the definition of the macro PSETQ.
    (ld "Data-and-control-flow/psetq-defmacro.lisp")
    ;; Load a file containing the definition of the macro PSETF.
    (ld "Data-and-control-flow/psetf-defmacro.lisp")
    ;; Load a file containing the definition of the macro ROTATEF.
    (ld "Data-and-control-flow/rotatef-defmacro.lisp")
    ;; Load a file containing the definition of the macro
    ;; DESTRUCTURING-BIND.
    (ld "Data-and-control-flow/destructuring-bind-defmacro.lisp")
    ;; Load a file containing the definition of the macro SHIFTF.
    (ld "Data-and-control-flow/shiftf-defmacro.lisp")
    ;; Load a file containing the definition of the macro PUSHNEW.
    (ld "Cons/pushnew-defmacro.lisp")
    ;; Load a file containing the definition of the macro DOTIMES.
    (ld "Iteration/dotimes-defmacro.lisp")
    ;; Load a file containing the definition of the macro DOLIST.
    (ld "Iteration/dolist-defmacro.lisp")
    ;; Load a file containing the definition of the macros DO and DO*.
    (ld "Iteration/do-dostar-defmacro.lisp")
    ;; Load a file containing the definition of the macro REMF.
    (ld "Cons/remf-defmacro.lisp")
    ;; Load a file containing the definition of the macro
    ;; WITH-PROPER-LIST-RESTS used by the functions MEMBER, MEMBER-IF,
    ;; and MEMBER-IF-NOT.
    (ld "Cons/with-proper-list-rests-defmacro.lisp")
    ;; Load a file containing the definition of the macro
    ;; WITH-PROPER-LIST-ELEMENTS used by several functions such as
    ;; SET-DIFFERENCE, UNION, etc.
    (ld "Cons/with-proper-list-elements-defmacro.lisp")
    ;; Load a file containing the definition of the macro
    ;; WITH-ALIST-ELEMENTS used by functions in the ASSOC family.
    (ld "Cons/with-alist-elements-defmacro.lisp")
    ;; Load a file containing the definition of the macro DEFCLASS.
    (ld "CLOS/defclass-defmacro.lisp")
    ;; Load a file containing the definition of the macro DEFGENERIC.
    (ld "CLOS/defgeneric-defmacro.lisp")
    ;; Load a file containing the definition of the macro DEFMETHOD.
    (ld "CLOS/defmethod-defmacro.lisp")
    (ld "Method-combination/define-method-combination-defmacro.lisp")
    (ld "Printer/print-unreadable-object-defmacro.lisp")))
