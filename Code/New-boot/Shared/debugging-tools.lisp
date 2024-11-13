(cl:in-package #:sicl-new-boot)

(defun function-names (environment)
  (let ((result '()))
    (maphash
     (lambda (name entry)
       (when (eq (clostrum-basic::status entry) :function)
         (push name result)))
     (clostrum-basic::functions environment))
    result))

;;; The name of this function means "find symbol" but is shorter for
;;; convenience.
(defun fs (name package-name)
  (let ((package (gethash package-name (sicl-new-boot::packages *b*))))
    (parcl-low:find-symbol (make-instance 'client) package name)))
