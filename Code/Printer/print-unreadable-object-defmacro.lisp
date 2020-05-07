(cl:in-package #:sicl-printer)

;;; FIXME figure out how to print an identity
(defmacro print-unreadable-object
    ((object stream &key type identity) &body body)
  (let ((object-var (gensym))
        (stream-var (gensym))
        (type-var (gensym))
        (identity-var (gensym)))
    `(let ((,object-var ,object)
           (,stream-var ,stream)
           (,type-var ,type)
           (,identity-var ,identity))
       (format stream "#<")
       (when ,type-var
         (format stream "~s" (class-of ,object-var)))
       (when ,identity-var
         (format stream "ID"))
       ,@body
       (format stream ">"))))

