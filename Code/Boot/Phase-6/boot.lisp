(cl:in-package #:sicl-boot-phase-6)

(defun load-alexandria (e5)
  (load-source-file "Data-and-control-flow/psetq-defmacro.lisp" e5)
  (load-asdf-system '#:alexandria e5))

(defun load-clostrum (e5)
  ;; FIXME: undefine all environment functions here.
  (loop for name in '(env:type-expander
                      env:compiler-macro-function (setf env:compiler-macro-function)
                      env:fdefinition)
        do (env:fmakunbound (env:client e5) e5 name))
  (load-source-file "Cons/getf-define-setf-expander.lisp" e5)
  ;; Since we are not using file-compilation semantics, Clostrum is
  ;; not definining these variables.
  (setf (env:special-variable
         (env:client e5) e5 'clostrum-implementation::*run-time-operators* t)
        '())
  (setf (env:special-variable
         (env:client e5) e5 'clostrum-implementation::*run-time-accessors* t)
        '())
  (setf (env:special-variable
         (env:client e5) e5 'clostrum-implementation::*compilation-operators* t)
        '())
  (load-asdf-system '#:clostrum/virtual e5))

(defun boot (boot)
  (format *trace-output* "Start phase 6~%")
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (prepare-this-phase e3 e4 e5)
    (load-source-file "Data-and-control-flow/defsetf.lisp" e5)
    (load-source-file "String/make-string-defun.lisp" e5)
    (setf *symbol-names* (make-hash-table :test #'eq))
    (load-asdf-system '#:sicl-filename e5)
    (load-asdf-system '#:sicl-printer-support e5)
    (load-source-file "Array/array-element-type-defgeneric.lisp" e5)
    (load-source-file "Array/array-element-type-defmethods.lisp" e5)
    ;; Avoid several warnings by making a temporary definition of SUBTYPEP.
    (define-error-functions '(subtypep) e5)
    (load-source-file "Array/upgraded-array-element-type-defun.lisp" e5)
    (setf (env:fdefinition (env:client e5) e5 'sicl-clos::update-header)
          (lambda (to from)
            (setf (slot-value to 'sicl-boot::%class)
                  (slot-value from 'sicl-boot::%class))
            (setf (slot-value to 'sicl-boot::%rack)
                  (slot-value from 'sicl-boot::%rack))))
    (load-source-file "CLOS/update-instance-defgenerics.lisp" e5)
    (load-source-file "CLOS/change-class-defgenerics.lisp" e5)
    (load-source-file "CLOS/built-in-method-combinations.lisp" e5)
    (load-source-file "CLOS/validate-superclass.lisp" e5)
    (load-source-file "Data-and-control-flow/setf-defmacro.lisp" e5)
    (load-source-file "Data-and-control-flow/values-define-setf-expander.lisp" e5)
    (load-source-file "Data-and-control-flow/equalp-defgeneric.lisp" e5)
    (load-source-file "Data-and-control-flow/values-list-defun.lisp" e5)
    (load-source-file "Evaluation-and-compilation/compiler-macro-function-defun.lisp" e5)
    (load-source-file "Evaluation-and-compilation/setf-compiler-macro-function-defun.lisp" e5)
    (load-source-file "CLOS/with-accessors-defmacro.lisp" e5)
    (load-source-file "Data-and-control-flow/define-modify-macro-defmacro.lisp" e5)
    (load-source-file "Array/vector-push-defun.lisp" e5)
    ;; Fake this macro for now
    (setf (env:macro-function (env:client e5) e5 'with-standard-io-syntax)
          (lambda (form environment)
            (declare (ignore environment))
            `(progn ,@(rest form))))
    (setf (env:fdefinition (env:client e5) e5 'proclaim)
          (constantly nil))
    (setf (env:fdefinition (env:client e5) e5 'documentation)
          (constantly nil))
    (setf (env:fdefinition (env:client e5) e5 '(setf documentation))
          (lambda (new-value x doc-type)
            (declare (ignore x doc-type))
            new-value))
    (enable-deftype e5)
    (enable-conditions e5)
    (load-asdf-system '#:sicl-hash-table e5)
    (load-asdf-system '#:sicl-ascii-character e5)
    (load-source-file "Cons/member-defun.lisp" e5)
    (load-source-file "Cons/union-defun.lisp" e5)
    (load-source-file "Cons/nunion-defun.lisp" e5)
    (load-alexandria e5)
    (enable-printing e5)
    (load-clostrum e5)
    (load-asdf-system '#:acclimation e5)
    (load-asdf-system '#:trucler-reference e5)
    (load-ctype e5)
    (load-source-file "Types/subtypep-defun.lisp" e5)
    (load-closer-mop e5)
    (load-eclector e5)
    (load-asdf-system '#:sicl-loop-support e5)
    (load-asdf-system '#:sicl-loop e5)
    (load-asdf-system '#:cleavir-code-utilities e5)
    (load-asdf-system '#:sicl-arithmetic-defuns e5)
    (load-asdf-system '#:sicl-cons-defuns e5)))
