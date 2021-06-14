(cl:in-package #:sicl-type)

(defun typexpand (type-descriptor &optional environment)
  (let* ((name (if (consp type-descriptor)
                   (first type-descriptor)
                   type-descriptor))
         (expander (type-expander name)))
    (if (null expander)
        (values type-descriptor nil)
        (let ((expansion (funcall expander type-descriptor environment)))
          (values (typexpand expansion environment) t)))))

