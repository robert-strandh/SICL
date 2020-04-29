(cl:in-package #:sicl-boot-phase-5)

;;; In phase 5, the purpose of class finalization is to finalize the
;;; ersatz classes in E4, so that we can create ersatz generic
;;; functions in E6, and ersatz classes in E5.  In other words, we are
;;; using accessors that operate on the ersatz classes in E4, and
;;; those accessors are found in E4 as well.  For that reason, most of
;;; the code in this file refers to E4.

(defun define-effective-slot-definition-class (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)) boot
    (sicl-boot:with-straddled-function-definitions
        ((sicl-clos::effective-slot-definition-class-default)
         e3 e4)
      (load-fasl "CLOS/effective-slot-definition-class-support.fasl" e3))
    (load-fasl "CLOS/effective-slot-definition-class-defgeneric.fasl" e4)
    (load-fasl "CLOS/effective-slot-definition-class-defmethods.fasl" e4)))

(defun define-class-finalization (boot)
  (with-accessors ((e4 sicl-boot:e4)) boot
    (load-fasl "CLOS/class-finalization-defgenerics.fasl" e4)
    (load-fasl "CLOS/class-finalization-support.fasl" e4)
    ;; FIXME: Temporary import.
    (import-function-from-host 'error (sicl-boot:e3 boot))
    (load-fasl "CLOS/class-finalization-defmethods.fasl" e4)))

(defun enable-class-finalization (boot)
  (define-effective-slot-definition-class boot)
  (define-class-finalization boot))
