(cl:in-package #:sicl-package)

(defun unexport (symbols-designator &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for symbol in symbols
          for name = (symbol-name symbol)
          do (multiple-value-bind (accessible-symbol status)
                 (find-symbol name package)
               (cond ((or (null status) (not (eq symbol accessible-symbol)))
                      (error 'symbol-is-not-accessible
                             :package package
                             :symbol symbol))
                     ((eq status :external)
                      (remhash name (external-symbols package))
                      (setf (gethash name (internal-symbols package))
                            symbol))
                     (t
                      ;; The symbol is either present and internal, or
                      ;; not present but accessible, so do nothing.
                      nil))))))
