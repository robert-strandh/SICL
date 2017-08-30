(cl:in-package #:cleavir-cst-to-ast-test)

(defun ff ()
  [cleavir-ast:function-ast
    :body-ast [cleavir-ast:progn-ast
                :form-asts ([cleavir-ast:setq-ast
                              :lhs-ast [cleavir-ast:lexical-ast
                                         :name x
                                         :origin nil
                                         :policy nil]
                              :origin 234
                              :policy nil
                              :value-ast [cleavir-ast:lexical-ast
                                           :name #:|x|
                                           :origin 234234
                                           :policy nil]]
                            [cleavir-ast:block-ast
                              :body-ast [cleavir-ast:load-time-value-ast
                                          :form nil
                                          :origin nil
                                          :policy nil
                                          :read-only-p t]
                              :origin nil
                              :policy nil])
                :origin nil
                :policy nil]
    :lambda-list ([cleavir-ast:lexical-ast
                    :name #:|x|
                    :origin 234234
                    :policy nil])
    :origin nil
    :policy nil])
