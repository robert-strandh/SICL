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

(defun test ()
  (test1)
  (test2)
;;  (test3)
  (test20)
  (test21)
  (test22)
  (test23)
  (test24)
  (test25)
  (test26))
