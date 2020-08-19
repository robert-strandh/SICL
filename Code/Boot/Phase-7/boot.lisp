(cl:in-package #:sicl-boot-phase-7)

(defmethod sicl-genv:class-of
    ((object sicl-boot::header) environment)
  (slot-value object 'sicl-boot::%class))

(defun boot (boot)
  (format *trace-output* "Start of phase 7~%")
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (load-source "CLOS/ensure-generic-function-using-class-support.lisp" e5)
    (load-source "CLOS/ensure-generic-function-using-class-defgenerics.lisp" e5)
    (load-source "CLOS/ensure-generic-function-using-class-defmethods.lisp" e5)
    (load-source "Arithmetic/plus-defun.lisp" e5)
    (load-source "Arithmetic/binary-add-defgeneric.lisp" e5)
    (load-source "Arithmetic/binary-add-defmethods.lisp" e5)
    (load-source "Arithmetic/binary-subtract-defgeneric.lisp" e5)
    (load-source "Arithmetic/binary-subtract-defmethods.lisp" e5)
    (load-source "Arithmetic/binary-less-defgeneric.lisp" e5)
    (load-source "Arithmetic/binary-less-defmethods.lisp" e5)
    (load-source "Arithmetic/binary-not-greater-defgeneric.lisp" e5)
    (load-source "Arithmetic/binary-not-greater-defmethods.lisp" e5)
    (load-source "Arithmetic/binary-equal-defgeneric.lisp" e5)
    (load-source "Arithmetic/binary-equal-defmethods.lisp" e5)
    (load-source "Arithmetic/binary-multiply-defgeneric.lisp" e5)
    (load-source "Arithmetic/binary-multiply-defmethods.lisp" e5)
    (load-source "Arithmetic/generic-floor-defgeneric.lisp" e5)
    (load-source "Arithmetic/generic-floor-defmethods.lisp" e5)
    (load-source "Arithmetic/generic-ceiling-defgeneric.lisp" e5)
    (load-source "Arithmetic/generic-ceiling-defmethods.lisp" e5)
    (load-source "Arithmetic/generic-truncate-defgeneric.lisp" e5)
    (load-source "Arithmetic/generic-truncate-defmethods.lisp" e5)
    (load-source "Arithmetic/generic-round-defgeneric.lisp" e5)
    (load-source "Arithmetic/generic-round-defmethods.lisp" e5)
    (load-printer e5)
    (load-make-instance e5)
    (load-source "CLOS/ensure-generic-function-defun.lisp" e5)
    (satiate-generic-functions e3 e4 e5)
    (patch-classes e4 e5)
    (set-class-unique-number-counter e5)
    (move-functions e5 e6)
    (patch-functions e3 e4 e5)
    (patch-method-specializers e5)
    (setf (sicl-genv:fdefinition 'make-instance e5)
          (sicl-genv:fdefinition 'sicl-clos::make-instance-temp e5))
    (setf (sicl-genv:fdefinition 'sicl-clos::find-accessor-method-class e5)
          (sicl-genv:fdefinition 'sicl-clos::find-class e5))
    (satiate-generic-functions e5 e5 e5)
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
    (enable-compilation e5)
    ;; During bootstrapping, the default method on
    ;; EFFECTIVE-SLOT-DEFINITION-CLASS in E5 calls FIND-CLASS in E4,
    ;; which is obviously wrong once the knot has been tied.  We just
    ;; have to reload the following file to fix that problem:
    (load-source "CLOS/effective-slot-definition-class-support.lisp" e5)
    (load-source "Cons/accessor-defuns.lisp" e5)
    (load-source "Cons/cxr.lisp" e5)
    (load-source "Cons/setf-cxr.lisp" e5)
    (load-source "Cons/getf-defun.lisp" e5)
    (load-source "Cons/member-if-not-defun.lisp" e5)))
