(cl:in-package #:sicl-boot-phase-6)

;;; In phase 6, the purpose of class finalization is to finalize the
;;; ersatz classes in E5, so that we can create ersatz generic
;;; functions in E7, and ersatz classes in E6.  In other words, we are
;;; using accessors that operate on the ersatz classes in E5, and
;;; those accessors are found in E5 as well.  For that reason, most of
;;; the code in this file refers to E5.

(defun define-effective-slot-definition-class (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (sicl-boot:with-straddled-function-definitions
        ((sicl-clos::effective-slot-definition-class-default)
         e4 e5)
      (load-fasl "CLOS/effective-slot-definition-class-support.fasl" e4))
    (load-fasl "CLOS/effective-slot-definition-class-defgeneric.fasl" e5)
    (load-fasl "CLOS/effective-slot-definition-class-defmethods.fasl" e5)))

(defun define-class-finalization (boot)
  (with-accessors ((e5 sicl-boot:e5)) boot
    (load-fasl "CLOS/class-finalization-defgenerics.fasl" e5)
    ;; FIND-IF-NOT is used to traverse the list of direct slot
    ;; definitions to find the first one that has a non-null
    ;; initfunction, which will then be used as the initfunction of
    ;; the effective slot-definition.
    (import-function-from-host 'find-if-not e5)
    ;; MAPCAR is used for several things during class finalization.
    ;; It is used to obtain the class-precedence-list of all
    ;; superclasses of a class.  It is used to find the initargs of
    ;; each slot definition object in a list.  And more.
    (import-function-from-host 'mapcar e5)
    ;; REMOVE-DUPCLICATES is used in serveral places for class
    ;; finalization, for computing a list of superclasses, a list of
    ;; initargs, a list of slots, etc.
    (import-function-from-host 'remove-duplicates e5)
    ;; UNION is used by class finalization to compute a list of
    ;; effective slot definitions.
    (import-function-from-host 'union e5)
    ;; REDUCE is used by class finalization in several places to combine
    ;; lists of superclasses, slots, and initargs from superclasses.
    (import-function-from-host 'reduce e5)
    ;; COUNT is used by class finalization to determine the number of
    ;; slots with :INSTANCE allocation.
    (import-function-from-host 'count e5)
    ;; Although LAST is not mentioned explicitly, it is needed, because
    ;; the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'last e5)
    ;; Although COPY-LIST is not mentioned explicitly, it is needed,
    ;; because the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'copy-list e5)
    (load-fasl "CLOS/class-finalization-support.fasl" e5)
    ;; FIXME: Temporary import.
    (import-function-from-host 'error (sicl-boot:e4 boot))
    (load-fasl "CLOS/class-finalization-defmethods.fasl" e5)))

(defun enable-class-finalization (boot)
  (define-effective-slot-definition-class boot)
  (define-class-finalization boot))
