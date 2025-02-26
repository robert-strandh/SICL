(cl:in-package #:sicl-new-boot-phase-1)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun common-boot::expand-compiler-macro (compiler-macro cst environment)
  (declare (ignore compiler-macro environment))
  (cst:raw cst))

(defun load-ctype (client environment global-environment)
  ;; The ctype library defines a method combination.  In phase 1, this
  ;; needs to be a host method combination, but it needs to be
  ;; available to DEFGENERIC.  We have programmed the DEFGENERIC macro
  ;; to search for method combinations in the host, so there is
  ;; nothing else we need to do.  Doing it this way is safe because
  ;; the symbol used to name the method combination will have been
  ;; translated by Eclector and the extrinsic package system to an
  ;; uninterned host symbol.  And in phase 1, we do not load any
  ;; DEFINE-METHOD-COMBINATION forms defining standardized method
  ;; combination types, so no name is a symbol in the COMMON-LISP
  ;; package.
  (setf (clo:macro-function
         client global-environment 'define-method-combination)
        (lambda (form environment)
          (declare (ignore environment))
          (eval form)
          nil))
  ;; I have no idea why this is necessary.
  (let ((symbol (find-symbol "LIST-STRUCTURE" "ECCLESIA")))
    (setf (clo:fdefinition client global-environment symbol)
          (fdefinition symbol)))
  ;; We don't expect to see any floating-point numbers during
  ;; bootstrapping.
  (setf (clo:fdefinition
         client global-environment @sicl-arithmetic:single-float-p)
        (constantly nil))
  (setf (clo:fdefinition
         client global-environment @sicl-arithmetic:double-float-p)
        (constantly nil))
  ;; The ctype library needs for the classes in the REGALIA.
  ;; to be defined, and referred to in package SICL-ARRAY.
  (sb:ensure-asdf-system client environment "regalia-base-intrinsic")
  (sb:ensure-asdf-system client environment "regalia-class-hierarchy")
  (sb:ensure-asdf-system client environment "sicl-array-support")
  ;; The ctype library defines a method on MAKE-LOAD-FORM that calls
  ;; MAKE-LOAD-FORM-SAVING-SLOTS.
  (setf (clo:fdefinition client global-environment 'make-load-form)
        #'make-load-form)
  (setf (clo:fdefinition
         client global-environment 'make-load-form-saving-slots)
        #'make-load-form-saving-slots)
  (setf (clo:fdefinition client global-environment 'print-object)
        #'print-object)
  (clo:make-variable client global-environment 'most-negative-fixnum
                     (- (expt 2 62)))
  (clo:make-variable client global-environment 'most-positive-fixnum
                     (1- (expt 2 62)))
  (clo:make-variable client global-environment 'char-code-limit
                     (1- (expt 2 24)))
  (sb:with-intercepted-function-cells
      ((make-instance (cons #'my-make-instance nil)))
    (let ((*features* '(:sicl)))
      (sb:ensure-asdf-system client environment "ctype"))))
