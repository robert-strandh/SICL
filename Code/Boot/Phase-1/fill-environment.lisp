(cl:in-package #:sicl-boot-phase-1)

(defun define-function-global-environment (client environment)
  ;; This function is used by macros in order to find the current
  ;; global environment.  If no argument is given, the run-time
  ;; environment (or startup environment) is returned.  If a macro
  ;; supplies an argument, and then it will typically be the
  ;; environment given to it by the &ENVIRONMENT parameter, then
  ;; the compilation environment is returned.  Macros use this
  ;; function to find information that is truly global, and that
  ;; Trucler does not manage, such as compiler macros. type
  ;; definitions, SETF expanders, etc.
  (setf (env:fdefinition
         client
         environment
         ;; There has got to be an easier way to define the
         ;; package so that it exists before this system is
         ;; compiled.
         'sicl-environment:global-environment)
        (lambda (&optional env)
          (if (null env)
              environment
              (trucler:global-environment client env)))))

(defun fill-environment (environment)
  (let ((client (env:client environment)))
    (define-defmacro client environment)
    (define-backquote-macros client environment)
    (import-cleavir-primops client environment)
    (import-code-utilities client environment)
    (import-trucler-functions client environment)
    (import-conditionals-support client environment)
    (import-function client environment 'error)
    (import-function client environment 'typep)
    (when (null (find-package '#:sicl-sequence))
      (make-package '#:sicl-sequence :use '(#:common-lisp)))
    (define-defgeneric-expander client environment)
    (flet ((ld (relative-file-name)
             (load-source-file relative-file-name environment)))
      (host-load "Evaluation-and-compilation/packages.lisp")
      (host-load "Data-and-control-flow/packages.lisp")
      (define-function-global-environment client environment)
      ;; Load a file containing a definition of the macro LAMBDA.
      ;; This macro is particularly simple, so it doesn't really
      ;; matter how it is expanded.  This is fortunate, because at the
      ;; time this file is loaded, the definition of DEFMACRO is still
      ;; one we created "manually" and which uses the host compiler to
      ;; compile the macro function in the null lexical environment.
      ;; We define the macro LAMBDA before we redefine DEFMACRO as a
      ;; target macro because PARSE-MACRO returns a LAMBDA form, so we
      ;; need this macro in order to redefine DEFMACRO.
      (ld "Evaluation-and-compilation/lambda.lisp")
      ;; Load a file containing the definition of the macro
      ;; MULTIPLE-VALUE-BIND.  We need it early because it is used in the
      ;; expansion of SETF, which we also need early for reasons explained
      ;; below.
      (ld "Data-and-control-flow/multiple-value-bind-defmacro.lisp")
      ;; Load a file containing definitions of standard conditional
      ;; macros, such as AND, OR, CASE, etc.
      (ld "Conditionals/macros.lisp")
      ;; Define a temporary version of SETF so that we can set the
      ;; MACRO-FUNCTION of the macros to be defined until we have a
      ;; proper version of DEFMACRO.  We need the SETF macro early,
      ;; because it is needed in order to define the macro DEFMACRO.
      ;; The reason for that, is that the expansion of DEFMACRO uses
      ;; SETF to set the macro function.  We could have defined
      ;; DEFMACRO to call (SETF MACRO-FUNCTION) directly, but that
      ;; would have been less "natural", so we do it this way instead.
      (setf (env:macro-function client environment 'setf)
            (lambda (form env)
              (declare (ignore env))
              (destructuring-bind (place value) (rest form)
                (check-type place (cons (eql macro-function)))
                `(funcall ,#'(setf env:macro-function)
                          ,value ,client ,environment ,(second place)))))
      ;; At this point, we have all the ingredients (the macros LAMBDA
      ;; and SETF) in order to redefine the macro DEFMACRO as a native
      ;; macro.  SINCE we already have a primitive form of DEFMACRO,
      ;; we use it to define DEFMACRO.  The result of loading this
      ;; file is that all new macros defined subsequently will have
      ;; their macro functions compiled with the target compiler.
      ;; However, the macro function of DEFMACRO is still compiled
      ;; with the host compiler.
      (ld "Evaluation-and-compilation/defmacro-defmacro.lisp")
      ;; As mentioned above, at this point, we have a version of
      ;; DEFMACRO that will compile the macro function of the macro
      ;; definition using the target compiler.  However, the macro
      ;; function of the macro DEFMACRO itself is still the result of
      ;; using the host compiler.  By loading the definition of
      ;; DEFMACRO again, we fix this "problem".
      (ld "Evaluation-and-compilation/defmacro-defmacro.lisp")
      ;; Up to this point, the macro function of the macro LAMBDA was
      ;; compiled using the host compiler.  Now that we have the final
      ;; version of the macro DEFMACRO, we can reload the file
      ;; containing the definition of the macro LAMBDA, which will
      ;; cause the macro function to be compiled with the target
      ;; compiler.
      (ld "Evaluation-and-compilation/lambda.lisp")
      ;; Similarly, the macro MULTIPLE-VALUE-BIND was compiled using
      ;; the host compiler.  By loading this file again, we will
      ;; compile the macro function again, this time with the target
      ;; compiler.
      (ld "Data-and-control-flow/multiple-value-bind-defmacro.lisp")
      ;; Similarly, the macros for conditional were compiled using the
      ;; host compiler.  By loading this file again, we will compile
      ;; those macro functions again, this time with the target
      ;; compiler.
      (ld "Conditionals/macros.lisp")
      ;; Load a file containing the definition of the macro
      ;; MULTIPLE-VALUE-LIST.  This definition is needed, because it
      ;; is used in the expansion of the macro NTH-VALUE loaded below.
      (ld "Data-and-control-flow/multiple-value-list-defmacro.lisp")
      (ld "Data-and-control-flow/nth-value.lisp")
      ;; We define MULTIPLE-VALUE-CALL as a macro.  This macro expands
      ;; to a primop that takes a function, rather than a function
      ;; designator, as its first argument.
      (ld "Data-and-control-flow/multiple-value-call-defmacro.lisp")
      (ld "Data-and-control-flow/setf-defmacro.lisp")
      (import-function
       client environment 'sicl-data-and-control-flow:defun-expander)
      ;; Load a file containing the definition of macro DEFUN.
      (ld "Data-and-control-flow/defun-defmacro.lisp")
      (ld "Data-and-control-flow/defconstant-defmacro.lisp")
      (ld "Data-and-control-flow/defvar-defmacro.lisp")
      (ld "Data-and-control-flow/defparameter-defmacro.lisp")
      (ld "Evaluation-and-compilation/macroexpand-hook-defparameter.lisp")
      (ld "Evaluation-and-compilation/macroexpand-1-defun.lisp")
      (host-load "Evaluation-and-compilation/declaim-support.lisp")
      (import-function
       client environment 'sicl-evaluation-and-compilation:declaim-expander)
      (ld "Evaluation-and-compilation/declaim-defmacro.lisp")
      ;; Load a file containing the definitions of the macros INCF and
      ;; DECF.
      (ld "Arithmetic/incf-decf-defmacro.lisp")
      (import-function client environment 'sicl-loop:expand-body)
      (ld "Loop/loop-defmacro.lisp")
      ;; Load a file containing the definitions of the macros PUSH and
      ;; POP.
      (import-function client environment 'sicl-cons:push-expander)
      (import-function client environment 'sicl-cons:pop-expander)
      (ld "Cons/push-pop-defmacro.lisp")
      ;; Load a file containing the definition of the macro RETURN.
      (ld "Data-and-control-flow/return-defmacro.lisp")
      ;; Load a file containing the definitions of the macros PROG1
      ;; and PROG2.
      (ld "Data-and-control-flow/prog1-prog2-defmacro.lisp")
      ;; Load a file containing the definitions of the macros PROG and
      ;; PROG*.
      (import-function
       client environment 'cleavir-code-utilities:separate-ordinary-body)
      (ld "Data-and-control-flow/prog-progstar-defmacro.lisp")
      (import-function
       client environment 'sicl-data-and-control-flow:psetf-expander)
      (ld "Data-and-control-flow/psetf-defmacro.lisp")
      (import-function
       client environment 'sicl-data-and-control-flow:rotatef-expander)
      (ld "Data-and-control-flow/rotatef-defmacro.lisp")
      (import-function
       client environment 'sicl-data-and-control-flow:destructuring-bind-expander)
      (ld "Data-and-control-flow/destructuring-bind-defmacro.lisp")
      (import-function
       client environment 'sicl-data-and-control-flow:shiftf-expander)
      (ld "Data-and-control-flow/shiftf-defmacro.lisp")
      ;; Load a file containing the definition of the macro PUSHNEW.
      (import-function client environment 'sicl-cons:pushnew-expander)
      (ld "Cons/pushnew-defmacro.lisp")
      ;; Load a file containing the definition of the macro DOTIMES.
      (import-function client environment 'sicl-iteration:dotimes-expander)
      (ld "Iteration/dotimes-defmacro.lisp")
      ;; Load a file containing the definition of the macro DOLIST.
      (import-function client environment 'sicl-iteration:dolist-expander)
      (ld "Iteration/dolist-defmacro.lisp")
      ;; Load a file containing the definition of the macros DO and DO*.
      (import-function client environment 'sicl-iteration:do-dostar-expander)
      (ld "Iteration/do-dostar-defmacro.lisp")
      ;; Load a file containing the definition of the macro REMF.
      (import-function client environment 'sicl-cons:remf-expander)
      (ld "Cons/remf-defmacro.lisp")
      ;; Load a file containing the definition of the macro
      ;; WITH-PROPER-LIST-RESTS used by the functions MEMBER,
      ;; MEMBER-IF, and MEMBER-IF-NOT.
      (ld "Cons/with-proper-list-rests-defmacro.lisp")
      ;; Load a file containing the definition of the macro
      ;; WITH-PROPER-LIST-ELEMENTS used by several functions such as
      ;; SET-DIFFERENCE, UNION, etc.
      (ld "Cons/with-proper-list-elements-defmacro.lisp")
      ;; Load a file containing the definition of the macro
      ;; WITH-ALIST-ELEMENTS used by functions in the ASSOC family.
      (ld "Cons/with-alist-elements-defmacro.lisp")
      (import-function
       client environment 'sicl-conditions:define-condition-expander)
      (ld "Conditions/define-condition-defmacro.lisp")
      (ld "Conditions/assert-defmacro.lisp")
      (ld "Conditions/check-type-defmacro.lisp")
      (ld "Conditions/handler-bind-defmacro.lisp")
      (import-function
       client environment 'sicl-conditions:make-handler-case-without-no-error-case)
      (import-function
       client environment 'sicl-conditions:make-handler-case-with-no-error-case)
      (ld "Conditions/handler-case-defmacro.lisp")
      (ld "Conditions/ignore-errors-defmacro.lisp")
      (import-function
       client environment 'sicl-conditions:restart-bind-transform-binding)
      (ld "Conditions/restart-bind-defmacro.lisp")
      (import-function
       client environment 'sicl-conditions:restart-case-make-restart-binding)
      (import-function
       client environment 'sicl-conditions:restart-case-make-restart-case)
      (import-function
       client environment 'sicl-conditions:restart-case-signaling-form-p)
      (import-function
       client environment 'sicl-conditions:restart-case-expand-signaling-form)
      (import-function
       client environment 'sicl-conditions:restart-case-parse-case)
      (ld "Conditions/restart-case-defmacro.lisp")
      (ld "Conditions/with-simple-restart-defmacro.lisp")
      (ld "Conditions/with-condition-restarts-defmacro.lisp")
      (import-function client environment 'sicl-clos:with-slots-expander)
      (ld "CLOS/with-slots-defmacro.lisp")
      (import-function client environment 'sicl-clos:defclass-expander)
      (ld "CLOS/defclass-defmacro.lisp")
      (ld "CLOS/defgeneric-defmacro.lisp"))))
