(cl:in-package #:sicl-expression-to-ast)

(defmethod expand ((ast ico:when-ast))
  (let ((origin (ico:origin ast)))
    (abp:with-builder ((make-instance 'builder))
      (abp:node* (:if :source origin)
        (1 :test (ico:test-ast ast))
        (1 :then (abp:node* (:progn :source origin)
                   (* :form (ico:form-asts ast))))
        (1 :else
           (abp:node* (:unparsed :expression 'nil :source origin)))))))
