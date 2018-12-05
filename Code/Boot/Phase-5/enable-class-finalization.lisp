(cl:in-package #:sicl-boot-phase-5)

;;; In phase 4, the purpose of class finalization is to finalize the
;;; ersatz classes in E4, so that we can create ersatz generic
;;; functions in E6, and ersatz classes in E5.  In other words, we are
;;; using accessors that operate on the ersatz classes in E5, and
;;; those accessors are found in E4 as well.  For that reason, most of
;;; the code in this file refers to E4.

(defun define-effective-slot-definition-class (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)) boot
    (setf (sicl-genv:special-variable
           'sicl-clos::*standard-effective-slot-definition* e4 t)
          (sicl-genv:find-class 'sicl-clos:standard-effective-slot-definition e3))
    (load-file "CLOS/effective-slot-definition-class-support.lisp" e4)
    (load-file "CLOS/effective-slot-definition-class-defgeneric.lisp" e4)
    (load-file "CLOS/effective-slot-definition-class-defmethods.lisp" e4)))

(defun define-class-finalization (boot)
  (with-accessors ((e4 sicl-boot:e4)) boot
    (load-file "CLOS/class-finalization-defgenerics.lisp" e4)
    ;; FIND-IF-NOT is used to traverse the list of direct slot
    ;; definitions to find the first one that has a non-null
    ;; initfunction, which will then be used as the initfunction of
    ;; the effective slot-definition.
    (import-function-from-host 'find-if-not e4)
    ;; MAPCAR is used for several things during class finalization.
    ;; It is used to obtain the class-precedence-list of all
    ;; superclasses of a class.  It is used to find the initargs of
    ;; each slot definition object in a list.  And more.
    (import-function-from-host 'mapcar e4)
    ;; REMOVE-DUPCLICATES is used in serveral places for class
    ;; finalization, for computing a list of superclasses, a list of
    ;; initargs, a list of slots, etc.
    (import-function-from-host 'remove-duplicates e4)
    ;; UNION is used by class finalization to compute a list of
    ;; effective slot definitions.
    (import-function-from-host 'union e4)
    ;; REDUCE is used by class finalization in several places to combine
    ;; lists of superclasses, slots, and initargs from superclasses.
    (import-function-from-host 'reduce e4)
    ;; COUNT is used by class finalization to determine the number of
    ;; slots with :INSTANCE allocation.
    (import-function-from-host 'count e4)
    ;; Although LAST is not mentioned explicitly, it is needed, because
    ;; the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'last e4)
    ;; Although COPY-LIST is not mentioned explicitly, it is needed,
    ;; because the expansion of LOOP with and APPEND clause uses it.
    (import-function-from-host 'copy-list e4)
    (load-file "CLOS/class-finalization-support.lisp" e4)
    (load-file "CLOS/class-finalization-defmethods.lisp" e4)))

(defun enable-class-finalization (boot)
  (with-accessors ((e3 sicl-boot:e3)
                   (e4 sicl-boot:e4)) boot
    (define-effective-slot-definition-class boot)
    (define-class-finalization boot)))
