(cl:in-package #:sicl-package)

;;; FIXME: check that the choice is a member of SYMBOLS.
(defun resolve-conflict (symbols package)
  (restart-case (error 'symbol-conflict
                       :conflicting-symbols symbols
                       :package)
    (choose-a-symbol ()
      :report
      (lambda (stream)
        (format stream "Choose a symbol."))
      :interactive 
      (lambda ()
        (format *debug-io* "Your choice: ")
        (return-from resolve-conflict
          (read *debug-io*))))))
