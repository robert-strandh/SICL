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
(defun fs (name package-name boot)
  (let ((package (gethash package-name (sicl-new-boot::packages boot))))
    (parcl-low:find-symbol (make-instance 'client) package name)))

;;; The name of this function means "find function" but is shorter for
;;; convenience.
(defun ff (function-name package-name environment boot)
  (clostrum:fdefinition
   (make-instance 'client)
   environment
   (fs function-name package-name boot)))
