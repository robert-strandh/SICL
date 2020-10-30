(cl:in-package #:sicl-cons)

(defun pushnew-expander
    (client item place env args key key-p test test-p test-not test-not-p)
  (declare (ignorable test test-not))
  (if (and test-p test-not-p)
      (progn (warn 'warn-both-test-and-test-not-given
                   :name 'pushnew)
             `(error 'both-test-and-test-not-given :name 'pushnew))
      (let ((item-var (gensym)))
        (multiple-value-bind (vars vals store-vars writer-form reader-form)
            (sicl-environment:get-setf-expansion
             client env place)
          `(let ((,item-var ,item)
                 ,@(mapcar #'list vars vals)
                 ,@(make-bindings args))
             ,@(if key-p `((declare (ignorable key))) `())
             (let ((,(car store-vars) ,reader-form))
               ,(if key
                    (if test-p
                        `(unless (member (funcall key ,item-var) ,(car store-vars)
                                         :test test :key key)
                           (push ,item-var ,(car store-vars)))
                        (if test-not-p
                            `(unless (member (funcall key ,item-var) ,(car store-vars)
                                             :test-not test-not :key key)
                               (push ,item-var ,(car store-vars)))
                            `(unless (member (funcall key ,item-var) ,(car store-vars)
                                             :key key)
                               (push ,item-var ,(car store-vars)))))
                    (if test-p
                        `(unless (member ,item-var ,(car store-vars)
                                         :test test)
                           (push ,item-var ,(car store-vars)))
                        (if test-not-p
                            `(unless (member ,item-var ,(car store-vars)
                                         :test-not test-not)
                               (push ,item-var ,(car store-vars)))
                            `(unless (member ,item-var ,(car store-vars))
                               (push ,item-var ,(car store-vars))))))
               ,writer-form))))))
