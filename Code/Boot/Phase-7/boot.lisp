(cl:in-package #:sicl-boot-phase-7)

(defmethod sicl-genv:class-of
    ((object sicl-boot::header) environment)
  (slot-value object 'sicl-boot::%class))

(defun boot (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (sicl-boot:load-source "CLOS/ensure-generic-function-using-class-support.lisp" e5)
    (sicl-boot:load-source "CLOS/ensure-generic-function-using-class-defgenerics.lisp" e5)
    (sicl-boot:load-source "CLOS/ensure-generic-function-using-class-defmethods.lisp" e5)
    (sicl-boot:load-source "Arithmetic/plus-defun.lisp" e5)
    (sicl-boot:load-source "Arithmetic/binary-add-defgeneric.lisp" e5)
    (sicl-boot:load-source "Arithmetic/binary-add-defmethods.lisp" e5)
    (load-printer e5)
    (load-make-instance e5)
    (sicl-boot:load-source "CLOS/ensure-generic-function-defun.lisp" e5)
    (satiate-generic-functions e3 e4 e5)
    (patch-classes e4 e5)
    (patch-functions e3 e4 e5)
    (move-functions e5 e6)
    (setf (sicl-genv:fdefinition 'make-instance e5)
          (sicl-genv:fdefinition 'sicl-clos::make-instance-temp e5))
    (sicl-boot:import-functions-from-host
     '(cleavir-code-utilities:lambda-list-type-specifier
       sicl-genv:fdefinition)
     e5)
    (allocate-class-prototypes e5)
    ;; Up until now, LOAD-FASL in E5 will tie a host code object by
    ;; calling make MAKE-INSTANCE in E4 in order to create a SICL code
    ;; object and a SICL function.  But now that that we have tied the
    ;; knot in E5, we want to call MAKE-INSTANCE in E5.
    (sicl-boot::define-load-fasl-2 e5 e5)
    (sicl-boot:load-source "Cons/accessor-defuns.lisp" e5)
    (sicl-boot:load-source "Cons/cxr.lisp" e5)
    (sicl-boot:load-source "Cons/setf-cxr.lisp" e5)
    (sicl-boot:load-source "Cons/getf-defun.lisp" e5)
    (sicl-boot:load-source "CLOS/defgeneric-support.lisp" e5)
    (sicl-boot:load-source "CLOS/defgeneric-defmacro.lisp" e5)
    (sicl-boot:load-source "CLOS/defmethod-support.lisp" e5)
    (sicl-boot:load-source "CLOS/defmethod-defmacro.lisp" e5)
    (sicl-boot:load-source "Conditionals/support.lisp" e5)
    (setf (sicl-genv:fdefinition 'proclaim e5)
          (lambda (&rest arguments)
            (format *trace-output* "args: ~s~%" arguments)))
    (sicl-boot:load-source "Cons/member-if-not-defun.lisp" e5)
    (sicl-boot:load-source "CLOS/defclass-support.lisp" e5)
    (sicl-boot:load-source "CLOS/defclass-defmacro.lisp" e5)))
