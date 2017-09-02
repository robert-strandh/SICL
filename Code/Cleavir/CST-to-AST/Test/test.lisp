(cl:in-package #:cleavir-cst-to-ast-test)

(defun test1 ()
  (let* ((cst (cst:cst-from-expression '(function (lambda (x) x))))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:function-ast
                 :origin (0)
                 :policy nil
                 :body-ast [cleavir-ast:progn-ast
                             :origin nil
                             :policy nil
                             :form-asts ([cleavir-ast:setq-ast
                                           :origin (0 1 1 0)
                                           :policy nil
                                           :lhs-ast [cleavir-ast:lexical-ast
                                                      :name x
                                                      :origin (0 1 1 0)
                                                      :policy nil]
                                           :value-ast [cleavir-ast:lexical-ast
                                                        :name #:|x|
                                                        :origin (0 1 1 0)
                                                        :policy nil]]
                                         [cleavir-ast:block-ast
                                           :origin nil
                                           :policy nil
                                           :body-ast [cleavir-ast:load-time-value-ast
                                                       :form nil
                                                       :origin nil
                                                       :policy nil
                                                       :read-only-p t]])]
                 :lambda-list ([cleavir-ast:lexical-ast
                                 :name #:|x|
                                 :origin (0 1 1 0)
                                 :policy nil])]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (ast-equal-p ast1 ast2))))

(defun test2 ()
  (let* ((cst (cst:cst-from-expression '*special1*))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:symbol-value-ast
                 :origin (0)
                 :policy nil
                 :symbol-ast [cleavir-ast:load-time-value-ast
                               :origin (0)
                               :policy nil
                               :read-only-p nil
                               :form '*special1*]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (ast-equal-p ast1 ast2))))

(defun test3 ()
  (let* ((cst (cst:cst-from-expression '(car *special1*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:call-ast
                 :origin nil
                 :policy nil
                 :callee-ast
                 [cleavir-ast:fdefinition-ast
                   :origin nil
                   :policy nil
                   :name-ast [cleavir-ast:load-time-value-ast
                               :origin (0 0)
                               :policy nil
                               :read-only-p nil
                               :form 'car]]
                 :argument-asts
                 ([cleavir-ast:symbol-value-ast
                    :origin (0 1)
                    :policy nil
                    :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0)
                                  :policy nil
                                  :read-only-p nil
                                  :form '*special1*]])]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (ast-equal-p ast1 ast2))))
