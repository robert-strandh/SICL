(cl:in-package #:sicl-package)

(defun symbol-is-present-p (symbol package)
  (or (eq symbol (gethash (symbol-name symbol) (internal-symbols package)))
      (eq symbol (gethash (symbol-name symbol) (external-symbols package)))))

(defun symbol-is-accessible-p (symbol package)
  (not (null (nth-value 1 (find-symbol (symbol-name symbol) package)))))

(defun resolve-conflict (new-symbol existing-symbol package)
  (restart-case (error 'symbol-conflict
                       :new-symbol new-symbol
                       :existing-symbol existing-symbol
                       :package)
    (keep-existing ()
      :report (lambda (stream)
                (format stream "Keep existing symbol."))
      (unless (symbol-is-present-p existing-symbol)
        (setf (gethash (symbol-name existing-symbol)
                       (internal-symbols package))
              existing-symbol))
      (push existing symbols (shadowing-symbols package)))
    (replace-existing
      :report (lambda (stream)
                (format stream "Replace existing symbol."))
      (when (symbol-is-present-p existing-symbol package)
        (unintern existing-symbol package))
      (when (symbol-is-accessible-p existing-symbol)
        (setf (gethash (symbol-name new-symbol)
                       (internal-symbols package))
              new-symbol)
        (push new-symbol (shadowing-symbols package))))))
