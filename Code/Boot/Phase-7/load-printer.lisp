(cl:in-package #:sicl-boot-phase-7)

(defun load-printer (e5)
  (sicl-boot:import-function-from-host
   'sicl-printer::print-symbol
   e5)
  (load-fasl "Printer/print-object-defgeneric.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-t.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-integer.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-symbol.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-cons.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-standard-object.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-function.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-class.fasl" e5)
  (load-fasl "Printer/print-object-defmethod-generic-function.fasl" e5))

