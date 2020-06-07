(cl:in-package #:sicl-boot-phase-7)

(defun load-printer (e5)
  (sicl-boot:import-function-from-host
   'sicl-printer::print-symbol
   e5)
  (import-functions-from-host
   '(zerop minusp float log integer-length * max expt schar princ mod
     print sicl-genv:boundp)
   e5)
  (load-source "Printer/variables.lisp" e5)
  (load-source "Printer/integer.lisp" e5)
  (load-source "Printer/print-object-defgeneric.lisp" e5)
  (load-source "Printer/print-object-defmethod-t.lisp" e5)
  (load-source "Printer/print-object-defmethod-integer.lisp" e5)
  (load-source "Printer/print-object-defmethod-symbol.lisp" e5)
  (load-source "Printer/print-object-defmethod-cons.lisp" e5)
  (load-source "Printer/print-object-defmethod-standard-object.lisp" e5)
  (load-source "Printer/print-object-defmethod-function.lisp" e5)
  (load-source "Printer/print-object-defmethod-class.lisp" e5)
  (load-source "Printer/print-object-defmethod-generic-function.lisp" e5))
