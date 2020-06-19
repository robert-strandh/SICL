(cl:in-package #:sicl-package)

(defun export-one-symbol (symbol package)
  (let ((external-symbols (external-symbols package))
        (internal-symbols (internal-symbols package))
        (symbol-name (symbol-name symbol)))
    (flet ((make-external ()
             (setf (gethash symbol-name external-symbols)
                   symbol)))
      (multiple-value-bind (accessible-symbol status)
          (find-symbol symbol-name package)
        (case status
          (:external
           (when (eq accessible-symbol symbol)
             ;; Then SYMBOL is already accessible in PACKAGE as an
             ;; external symbol.
             (return-from export-one-symbol nil)))
          (:internal
           (when (eq accessible-symbol symbol)
             ;; Then SYMBOL is PRESENT in PACKAGE as an internal
             ;; symbol. Change it to be external.
             (remhash symbol-name internal-symbols)
             (make-external symbol)))
          (:inherited
           (when (eq symbol accessible-symbol)
             ;; Then SYMBOL is accessible as an internal symbol via
             ;; USE-PACKAGE.  Then we first import it and then export
             ;; it.  We accomplish this effect by making it external
             ;; from the beginning.
             (setf (gethash symbol-name external-symbols) symbol)))
          (t
           (error "symbol ~s not accessible in package ~s"
                  symbol package))))
      (loop for using-package in (used-by-list package)
            do (multiple-value-bind (other-symbol status)
                   (find-symbol symbol-name using-package)
                 (unless (or (null status)
                             (eq other-symbol symbol))
                   (resolve-conflict symbol other-symbol using-package)))))))
