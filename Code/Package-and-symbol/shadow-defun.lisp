(cl:in-package #:sicl-package)

(defun shadow (symbol-names &optional (package *package*))
  (loop with internals = (internal-symbols package)
        with externals = (external-symbols package)
        for name in symbol-names
        for string-name = (string name)
        do (multiple-value-bind (internal internal-present-p)
               (gethash string-name internals)
             (multiple-value-bind (external external-present-p)
                 (gethash string-name externals)
               (let ((symbol (cond (internal-present-p internal)
                                   (external-present-p external)
                                   (t
                                    (let ((new (intern string-name package)))
                                      (setf (gethash string-name internals)
                                            new)
                                      new)))))
                 (pushnew symbol (shadowing-symbols package)))))))
