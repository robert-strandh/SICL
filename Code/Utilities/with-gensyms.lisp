(cl:in-package #:sicl-utilities)

(defmacro with-gensyms (specs &body body)
  `(let ,(mapcar #'parse-with-gensyms-spec specs)
     ,@body))

(defun parse-with-gensyms-spec (spec)
  (cond ((symbolp spec)
         `(,spec (gensym ,(symbol-name spec))))
        ((and (consp spec)
              (consp (cdr spec))
              (null (cddr spec)))
         (destructuring-bind (symbol string) spec
             (check-type symbol symbol)
           `(,symbol (gensym (string ',string)))))
        (t
         (error "Malformed with-gensyms spec: ~S" spec))))
