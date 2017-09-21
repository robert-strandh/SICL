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
                                         [cleavir-ast:progn-ast
                                           :origin nil
                                           :policy nil
                                           :form-asts
                                           ([cleavir-ast:lexical-ast
                                              :name x
                                              :origin (0 1 1 0)
                                              :policy nil])])]
                 :lambda-list ([cleavir-ast:lexical-ast
                                 :name #:|x|
                                 :origin (0 1 1 0)
                                 :policy nil])]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test2 ()
  (let* ((cst (cst:cst-from-expression '*special1*))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:symbol-value-ast
                 :origin (0)
                 :policy nil
                 :symbol-ast [cleavir-ast:load-time-value-ast
                               :origin (0)
                               :policy nil
                               :read-only-p t
                               :form '*special1*]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test3 ()
  (let* ((cst (cst:cst-from-expression '(car *special1*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:call-ast
                 :origin (0)
                 :policy nil
                 :callee-ast
                 [cleavir-ast:fdefinition-ast
                   :origin nil
                   :policy nil
                   :name-ast [cleavir-ast:load-time-value-ast
                               :origin (0 0)
                               :policy nil
                               :read-only-p t
                               :form 'car]]
                 :argument-asts
                 ([cleavir-ast:symbol-value-ast
                    :origin (0 1)
                    :policy nil
                    :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 1)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]])]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test4 ()
  (let* ((cst (cst:cst-from-expression '((lambda (x) x) 234)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:call-ast
                  :argument-asts
                  ([cleavir-ast:load-time-value-ast
                      :read-only-p t
                      :form '234
                      :policy nil
                      :origin (0 1)])
                  :callee-ast
                  [cleavir-ast:function-ast
                     :body-ast
                     [cleavir-ast:progn-ast
                        :form-asts
                        ([cleavir-ast:setq-ast
                            :value-ast
                            #1=[cleavir-ast:lexical-ast
                                  :name #:|x|
                                  :policy nil
                                  :origin #2=(0 0 1 0)]
                            :lhs-ast
                            #3=[cleavir-ast:lexical-ast
                                  :name cleavir-cst-to-ast-test::x
                                  :policy nil
                                  :origin #2#]
                            :policy nil
                            :origin #2#]
                         [cleavir-ast:progn-ast
                            :form-asts
                            (#3#)
                            :policy nil
                            :origin nil])
                        :policy nil
                        :origin nil]
                     :lambda-list
                     (#1#)
                     :policy nil
                     :origin nil]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test5 ()
  (let* ((cst (cst:cst-from-expression '(let ((x 234)) x)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:call-ast
                  :argument-asts
                  ([cleavir-ast:load-time-value-ast
                      :read-only-p t
                      :form '234
                      :policy nil
                      :origin (0 1 0 1)])
                  :callee-ast
                  [cleavir-ast:function-ast
                     :body-ast
                     [cleavir-ast:progn-ast
                        :form-asts
                        ([cleavir-ast:setq-ast
                            :value-ast
                            #1=[cleavir-ast:lexical-ast
                                  :name #:|x|
                                  :policy nil
                                  :origin #2=(0 1 0 0)]
                            :lhs-ast
                            #3=[cleavir-ast:lexical-ast
                                  :name cleavir-cst-to-ast-test::x
                                  :policy nil
                                  :origin #2#]
                            :policy nil
                            :origin #2#]
                         [cleavir-ast:progn-ast
                            :form-asts
                            (#3#)
                            :policy nil
                            :origin nil])
                        :policy nil
                        :origin nil]
                     :lambda-list
                     (#1#)
                     :policy nil
                     :origin nil]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test6 ()
  (let* ((cst (cst:cst-from-expression '(let* ((x 234)) x)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:call-ast
                  :argument-asts
                  ([cleavir-ast:load-time-value-ast
                      :read-only-p t
                      :form '234
                      :policy nil
                      :origin (0 1 0 1)])
                  :callee-ast
                  [cleavir-ast:function-ast
                     :body-ast
                     [cleavir-ast:progn-ast
                        :form-asts
                        ([cleavir-ast:setq-ast
                            :value-ast
                            #1=[cleavir-ast:lexical-ast
                                  :name #:|x|
                                  :policy nil
                                  :origin #2=(0 1 0 0)]
                            :lhs-ast
                            #3=[cleavir-ast:lexical-ast
                                  :name cleavir-cst-to-ast-test::x
                                  :policy nil
                                  :origin #2#]
                            :policy nil
                            :origin #2#]
                         [cleavir-ast:progn-ast
                            :form-asts
                            (#3#)
                            :policy nil
                            :origin nil])
                        :policy nil
                        :origin nil]
                     :lambda-list
                     (#1#)
                     :policy nil
                     :origin nil]
                  :policy nil
                  :origin nil]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test7 ()
  (let* ((cst (cst:cst-from-expression '(quote a)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:load-time-value-ast
                  :read-only-p t
                  :form 'cleavir-cst-to-ast-test::a
                  :policy nil
                  :origin (0 1)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test8 ()
  (let* ((cst (cst:cst-from-expression '(block a 234)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:block-ast
                  :body-ast
                  [cleavir-ast:progn-ast
                     :form-asts
                     ([cleavir-ast:load-time-value-ast
                         :read-only-p t
                         :form '234
                         :policy nil
                         :origin (0 2)])
                     :policy nil
                     :origin nil]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test9 ()
  (let* ((cst (cst:cst-from-expression '(block a (return-from a 234))))
         (env (make-instance 'environment))
         (ast2 #1=[cleavir-ast:block-ast
                     :body-ast
                     [cleavir-ast:progn-ast
                        :form-asts
                        ([cleavir-ast:return-from-ast
                            :form-ast
                            [cleavir-ast:load-time-value-ast
                               :read-only-p t
                               :form '234
                               :policy nil
                               :origin (0 2 2)]
                            :block-ast #1#
                            :policy nil
                            :origin (0 2)])
                        :policy nil
                        :origin nil]
                     :policy nil
                     :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test10 ()
  (let* ((cst (cst:cst-from-expression '(eval-when (:execute) 234)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:progn-ast
                  :form-asts
                  ([cleavir-ast:load-time-value-ast
                      :read-only-p t
                      :form '234
                      :policy nil
                      :origin (0 2)])
                  :policy nil
                  :origin nil])
         (cleavir-cst-to-ast:*compiler* 'compile))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test11 ()
  (let* ((cst (cst:cst-from-expression '(tagbody a)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:progn-ast
                  :form-asts
                  ([cleavir-ast:tagbody-ast
                      :item-asts
                      ([cleavir-ast:tag-ast
                          :name a
                          :policy nil
                          :origin (0 1)])
                      :policy nil
                      :origin (0)]
                   [cleavir-ast:load-time-value-ast
                      :read-only-p t
                      :form 'nil
                      :policy nil
                      :origin nil])
                  :policy nil
                  :origin nil]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test12 ()
  (let* ((cst (cst:cst-from-expression '(tagbody a (go a))))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:progn-ast
                  :form-asts
                  ([cleavir-ast:tagbody-ast
                      :item-asts
                      (#1=[cleavir-ast:tag-ast
                             :name a
                             :policy nil
                             :origin (0 1)]
                       [cleavir-ast:go-ast
                          :tag-ast #1#
                          :policy nil
                          :origin (0 2)])
                      :policy nil
                      :origin (0)]
                   [cleavir-ast:load-time-value-ast
                      :read-only-p t
                      :form 'nil
                      :policy nil
                      :origin nil])
                  :policy nil
                  :origin nil]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test13 ()
  (let* ((cst (cst:cst-from-expression '(if *special1* 234 345)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:if-ast
                  :else-ast
                  [cleavir-ast:load-time-value-ast
                     :read-only-p t
                     :form '234
                     :policy nil
                     :origin (0 2)]
                  :then-ast
                  [cleavir-ast:load-time-value-ast
                     :read-only-p t
                     :form '345
                     :policy nil
                     :origin (0 3)]
                  :test-ast
                  [cleavir-ast:eq-ast
                     :arg2-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'nil
                        :policy nil
                        :origin nil]
                     :arg1-ast
                     [cleavir-ast:symbol-value-ast
                        :symbol-ast
                        [cleavir-ast:load-time-value-ast
                           :read-only-p t
                           :form '*special1*
                           :policy nil
                           :origin #1=(0 1)]
                        :policy nil
                        :origin #1#]
                     :policy nil
                     :origin nil]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test20 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-add short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-add-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test21 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-sub short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-sub-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test22 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-mul short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-mul-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test23 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-div short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-div-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test24 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-less short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-less-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test25 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-not-greater short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-not-greater-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test26 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-greater short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-greater-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test27 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-not-less short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-not-less-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test28 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-equal short-float *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-equal-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg1-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                  :origin (0 2)
                                  :policy nil
                                  :read-only-p t
                                  :form '*special1*]]
                 :arg2-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 3)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 3)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special2*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test30 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-sin short-float *special1*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-sin-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 2)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special1*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test31 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-cos short-float *special1*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-cos-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 2)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special1*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test32 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:float-sqrt short-float *special1*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:float-sqrt-ast
                 :origin (0)
                 :policy nil
                 :subtype short-float
                 :arg-ast
                 [cleavir-ast:symbol-value-ast
                   :origin (0 2)
                   :policy nil
                   :symbol-ast [cleavir-ast:load-time-value-ast
                                 :origin (0 2)
                                 :policy nil
                                 :read-only-p t
                                 :form '*special1*]]]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test40 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:fixnum-equal *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:fixnum-equal-ast
                  :arg2-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special2*
                        :policy nil
                        :origin #1=(0 2)]
                     :policy nil
                     :origin #1#]
                  :arg1-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special1*
                        :policy nil
                        :origin #2=(0 1)]
                     :policy nil
                     :origin #2#]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test41 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:fixnum-less *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:fixnum-less-ast
                  :arg2-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special2*
                        :policy nil
                        :origin #1=(0 2)]
                     :policy nil
                     :origin #1#]
                  :arg1-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special1*
                        :policy nil
                        :origin #2=(0 1)]
                     :policy nil
                     :origin #2#]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test42 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:fixnum-not-greater *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:fixnum-not-greater-ast
                  :arg2-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special2*
                        :policy nil
                        :origin #1=(0 2)]
                     :policy nil
                     :origin #1#]
                  :arg1-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special1*
                        :policy nil
                        :origin #2=(0 1)]
                     :policy nil
                     :origin #2#]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test43 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:fixnum-greater *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:fixnum-greater-ast
                  :arg2-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special2*
                        :policy nil
                        :origin #1=(0 2)]
                     :policy nil
                     :origin #1#]
                  :arg1-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special1*
                        :policy nil
                        :origin #2=(0 1)]
                     :policy nil
                     :origin #2#]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test44 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:fixnum-not-less *special1* *special2*)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:fixnum-not-less-ast
                  :arg2-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special2*
                        :policy nil
                        :origin #1=(0 2)]
                     :policy nil
                     :origin #1#]
                  :arg1-ast
                  [cleavir-ast:symbol-value-ast
                     :symbol-ast
                     [cleavir-ast:load-time-value-ast
                        :read-only-p t
                        :form 'cleavir-cst-to-ast-test::*special1*
                        :policy nil
                        :origin #2=(0 1)]
                     :policy nil
                     :origin #2#]
                  :policy nil
                  :origin (0)]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test45 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:let-uninitialized (x) x)))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:progn-ast
                  :form-asts
                  ([cleavir-ast:lexical-ast
                      :name cleavir-cst-to-ast-test::x
                      :policy nil
                      :origin (0 1 0)])
                  :policy nil
                  :origin nil]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test46 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:let-uninitialized (x)
                 (cleavir-primop:fixnum-add *special1* *special2* x))))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:progn-ast
                  :form-asts
                  ([cleavir-ast:fixnum-add-ast
                      :variable-ast
                      [cleavir-ast:lexical-ast
                         :name cleavir-cst-to-ast-test::x
                         :policy nil
                         :origin (0 1 0)]
                      :arg2-ast
                      [cleavir-ast:symbol-value-ast
                         :symbol-ast
                         [cleavir-ast:load-time-value-ast
                            :read-only-p t
                            :form 'cleavir-cst-to-ast-test::*special2*
                            :policy nil
                            :origin #1=(0 2 2)]
                         :policy nil
                         :origin #1#]
                      :arg1-ast
                      [cleavir-ast:symbol-value-ast
                         :symbol-ast
                         [cleavir-ast:load-time-value-ast
                            :read-only-p t
                            :form 'cleavir-cst-to-ast-test::*special1*
                            :policy nil
                            :origin #2=(0 2 1)]
                         :policy nil
                         :origin #2#]
                      :policy nil
                      :origin (0 2)])
                  :policy nil
                  :origin nil]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test47 ()
  (let* ((cst (cst:cst-from-expression
               '(cleavir-primop:let-uninitialized (x)
                 (cleavir-primop:fixnum-sub *special1* *special2* x))))
         (env (make-instance 'environment))
         (ast2 [cleavir-ast:progn-ast
                  :form-asts
                  ([cleavir-ast:fixnum-sub-ast
                      :variable-ast
                      [cleavir-ast:lexical-ast
                         :name cleavir-cst-to-ast-test::x
                         :policy nil
                         :origin (0 1 0)]
                      :arg2-ast
                      [cleavir-ast:symbol-value-ast
                         :symbol-ast
                         [cleavir-ast:load-time-value-ast
                            :read-only-p t
                            :form 'cleavir-cst-to-ast-test::*special2*
                            :policy nil
                            :origin #1=(0 2 2)]
                         :policy nil
                         :origin #1#]
                      :arg1-ast
                      [cleavir-ast:symbol-value-ast
                         :symbol-ast
                         [cleavir-ast:load-time-value-ast
                            :read-only-p t
                            :form 'cleavir-cst-to-ast-test::*special1*
                            :policy nil
                            :origin #2=(0 2 1)]
                         :policy nil
                         :origin #2#]
                      :policy nil
                      :origin (0 2)])
                  :policy nil
                  :origin nil]))
    (assign-sources cst)
    (let ((ast1 (cleavir-cst-to-ast:cst-to-ast cst env nil)))
      (assert (ast-equal-p ast1 ast2)))))

(defun test ()
  (test1)
  (test2)
  (test3)
  (test4)
  (test5)
  (test6)
  (test7)
  (test8)
  (test9)
  (test10)
  (test11)
  (test12)
  (test13)
  (test20)
  (test21)
  (test22)
  (test23)
  (test24)
  (test25)
  (test26)
  (test27)
  (test28)
  (test30)
  (test31)
  (test32)
  (test40)
  (test41)
  (test42)
  (test43)
  (test44)
  (test45)
  (test46)
  (test47))
