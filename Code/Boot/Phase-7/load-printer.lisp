(cl:in-package #:sicl-boot-phase-7)

(defun load-printer (e5)
  (sicl-boot:import-function-from-host
   'sicl-printer::print-symbol
   e5)
  (load-source "Printer/print-object-defgeneric.lisp" e5)
  (load-source "Printer/print-object-defmethod-t.lisp" e5)
  (load-source "Printer/print-object-defmethod-integer.lisp" e5)
  (load-source "Printer/print-object-defmethod-symbol.lisp" e5)
  (load-source "Printer/print-object-defmethod-cons.lisp" e5)
  (load-source "Printer/print-object-defmethod-standard-object.lisp" e5)
  (load-source "Printer/print-object-defmethod-function.lisp" e5)
  (load-source "Printer/print-object-defmethod-class.lisp" e5)
  (load-source "Printer/print-object-defmethod-generic-function.lisp" e5))

