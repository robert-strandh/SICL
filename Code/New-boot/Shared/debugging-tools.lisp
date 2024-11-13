(cl:in-package #:sicl-new-boot)

(defun function-names (environment)
  (let ((result '()))
    (maphash
     (lambda (name entry)
       (when (eq (clostrum-basic::status entry) :function)
         (push name result)))
     (clostrum-basic::functions environment))
    result))
