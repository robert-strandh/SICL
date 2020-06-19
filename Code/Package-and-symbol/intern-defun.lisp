(cl:in-package #:sicl-package)

(defun intern (symbol-name &optional (package-designator *package*))
  (let ((package (package-designator-to-package package-designator)))
    (multiple-value-bind (symbol-or-nil status)
        (find-symbol symbol-name package)
      (if (null status)
          (let ((new-symbol (make-symbol symbol-name)))
            (setf (sicl-symbol:package new-symbol) package)
            (setf (gethash
                   symbol-name
                   (if (eq package (find-package '#:keyword))
                       (external-symbols package)
                       (internal-symbols package)))
                  new-symbol)
            (values new-symbol nil))
          (values symbol-or-nil status)))))
