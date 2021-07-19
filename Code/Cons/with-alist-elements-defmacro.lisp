(cl:in-package #:sicl-cons)

(defmacro with-alist-elements ((element-var alist function-name) &body body)
  ;; We can use for ... on, because it uses atom to test the end
  ;; of the list
  (let ((remaining (gensym))
        (alist-var (gensym)))
    `(loop with ,alist-var = ,alist
           for ,remaining on ,alist-var
           do (let ((,element-var (car ,remaining)))
                (cond ((consp ,element-var)
                       ,@body)
                      ((null ,element-var)
                       nil)
                      (t
                       (error 'must-be-list
                              :datum ,element-var
                              :name ',function-name))))
           finally (return nil))))
