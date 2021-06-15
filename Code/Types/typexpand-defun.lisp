(cl:in-package #:sicl-type)

(defun typexpand (type-specifier &optional environment)
  (let* ((name (if (consp type-specifier)
                   (first type-specifier)
                   type-specifier))
         (expander (type-expander name)))
    (if (null expander)
        (values type-specifier nil)
        (let ((expansion (funcall expander
                                  (if (consp type-specifier)
                                      type-specifier
                                      (list type-specifier))
                                  environment)))
          (values (typexpand expansion environment) t)))))

