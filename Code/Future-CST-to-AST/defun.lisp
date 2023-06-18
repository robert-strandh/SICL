(cl:in-package #:sicl-expression-to-ast)

(defmethod expand ((ast ico:defun-ast))
  (let* ((name (ico:name (ico:name-ast ast)))
         (block-name (if (symbolp name) name (second name))))
    (abp:with-builder (make-instance 'builder)
      (abp:node* (:progn)
        (* :form
           (abp:node* (:eval-when)
             (* :situation (:compile-toplevel))
             ;; Add compile-time stuff here.
             )
           (abp:node* (:eval-when)
             (* :situation (:load-toplevel) (:execute))
             (* :form
                (abp:node* (:setf)
                  ;; Add SETF stuff here.
                  (abp:node* (:lambda
                               :lambda-list-ast
                               (ico:lambda-list-ast ast)
                               :declaration-asts
                               (ico:declaration-asts ast)
                               :documentation-ast
                               (ico:documentation-ast ast))
                    (* :form
                       (abp:node* (:block :form-asts (ico:form-asts ast))
                         (1 :block-name
                            (abp:node* (:block-name :name block-name)))))))
                (abp:node* (:literal :literal name)))))))))
