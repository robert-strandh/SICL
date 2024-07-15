(cl:in-package #:sicl-new-boot-phase-3)

(defun load-ctype (client environment global-environment)
  ;; I have no idea why this is necessary.
  (let ((symbol (find-symbol "LIST-STRUCTURE" "ECCLESIA")))
    (setf (clo:fdefinition client global-environment symbol)
          (fdefinition symbol)))
  ;; The ctype library needs for the system SICL-ARITHMETIC to be
  ;; loaded.
  (sb:ensure-asdf-system client environment "sicl-arithmetic-base")
  ;; The ctype library needs for the classes in the module SICL-ARRAY
  ;; to be defined.
  (sb:ensure-asdf-system client environment "sicl-array-support")
  (sb:ensure-asdf-system client environment "sicl-array-load-time")
  ;; The ctype library calls the function SICL-TYPE:TYPE-EXPAND, so we
  ;; need to have the package SICL-TYPE defined.
  (sb:ensure-asdf-system client environment "sicl-type-support")
  (setf (clo:fdefinition
         client global-environment 'make-load-form-saving-slots)
        #'make-load-form-saving-slots)
  (clo:make-variable client global-environment 'most-negative-fixnum
                     (- (expt 2 62)))
  (clo:make-variable client global-environment 'most-positive-fixnum
                     (1- (expt 2 62)))
  (clo:make-variable client global-environment 'char-code-limit
                     (1- (expt 2 24)))
  (let ((*features* '(:sicl)))
    (sb:ensure-asdf-system client environment "ctype"))
  (sb:ensure-asdf-system client environment "sicl-typep-subtypep"))
