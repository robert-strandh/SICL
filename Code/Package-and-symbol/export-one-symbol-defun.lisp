(cl:in-package #:sicl-package)

(defun export-one-symbol (symbol package)
  (flet ((make-external (sym)
	   (setf (external-symbols package)
		 (cons sym (external-symbols package)))))
    (cond ((member symbol (external-symbols package))
           ;; do nothing
           (return-from export-one-symbol cl:t))
          ((member symbol (internal-symbols package))
           ;; change it to be external
           (setf (internal-symbols package)
                 (remove symbol (internal-symbols package)
                         :test #'eq))
           (make-external symbol))
          (t
           (loop for used = (package-use-list package)
                   then (cdr used)
                 while (consp used)
                 do (loop for syms = (package-use-list (car used))
                            then (cdr syms)
                          do (when (eq (car syms) symbol)
                               (make-external symbol)
                               (return-from export-one-symbol t))))
           ;; come here if the symbol is not accessible
           (error "symbol ~s not accessible in package ~s"
                  (symbol-name symbol)
                  ;; FIXME: This won't work for symbols
                  ;; without a home package.
                  (package-name (symbol-package symbol)))))))
