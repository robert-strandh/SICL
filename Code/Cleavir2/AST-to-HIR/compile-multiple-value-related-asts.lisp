(cl:in-package #:cleavir-ast-to-hir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MULTIPLE-VALUE-PROG1-AST.

(defun compile-multiple-value-prog1-body (client form-asts context)
  (loop with result-context = context
        for form-ast in (reverse form-asts)
        for successor = (compile-ast client form-ast result-context)
        do (setf result-context
                 (clone-context
                  context
                  :successor successor))
        finally (return (first (successors result-context)))))

(defmethod compile-ast (client (ast cleavir-ast:multiple-value-prog1-ast) context)
  (compile-ast
   client
   (cleavir-ast:first-form-ast ast)
   (clone-context
    context
    :successor
    (if (eq (results context) :values)
        (let* ((dynamic-environment-location
                 (cleavir-ir:make-lexical-location (gensym "values")))
               (new-context
                 (clone-context
                  context
                  :results '()
                  :dynamic-environment-location dynamic-environment-location)))
          (make-instance 'cleavir-ir:save-values-instruction
            :output dynamic-environment-location
            :successor
            (compile-multiple-value-prog1-body
             client
             (cleavir-ast:form-asts ast)
             (clone-context
              new-context
              :successor
              (make-instance 'cleavir-ir:restore-values-instruction
                :successor (first (successors context))
                :dynamic-environment-location dynamic-environment-location)))))
        (let ((new-context (clone-context context :results '())))
          (compile-multiple-value-prog1-body
           client
           (cleavir-ast:form-asts ast)
           (clone-context
            new-context
            :successor (first (successors context)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MULTIPLE-VALUE-SETQ-AST.

(defmethod compile-ast (client (ast cleavir-ast:multiple-value-setq-ast) context)
  (assert-context ast context nil 1)
  (let ((locations (mapcar #'find-or-create-location
                           (cleavir-ast:lhs-asts ast))))
    (compile-ast
     client
     (cleavir-ast:form-ast ast)
     (clone-context
      context
      :results locations
      :successor (first (successors context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a VALUES-AST.

(defun split-lists (list1 list2)
  ;; Returns maximal prefixes of the same length,
  ;; and then suffixes. E.g.:
  ;; (s-l '(1 2) '(3 4 5)) => (1 2), (3 4), nil, (5)
  ;; (s-l '(1 2 3) '(4 5)) => (1 2), (4 5), (3), nil
  ;; (s-l '(1 2) '(3 4)) => (1 2), (3 4), nil, nil
  (let ((min (min (length list1) (length list2))))
    (values (subseq list1 0 min)
            (subseq list2 0 min)
            (nthcdr min list1)
            (nthcdr min list2))))

(defmethod compile-ast (client (ast cleavir-ast:values-ast) context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (assert-context ast context nil 1)
    (let ((arguments (cleavir-ast:argument-asts ast)))
      (cond ((eq results :values)
             (let ((temps (make-temps arguments)))
               (compile-arguments
                client
                arguments temps
                (make-instance 'cleavir-ir:fixed-to-multiple-instruction
                 :inputs temps :successor (first successors))
                context)))
            (t ;lexical locations
             ;; this is a bit tricky because there may be more or less
             ;; arguments than results, in which case we must compile
             ;; for effect or nil-fill.
             ;; First we collect those that match.
             (multiple-value-bind (args results valueless nils)
                 (split-lists arguments results)
               ;; Now we know which match, so compile those.
               ;; Note also we have to preserve left-to-right evaluation order.
               (compile-arguments
                client
                args
                results
                ;; Obviously at most one of valueless and nils can be non-null.
                (cond (valueless
                       (loop with succ = (first successors)
                             for arg in (reverse valueless)
                             do (setf succ (compile-ast client
                                                        arg
                                                        (clone-context
                                                         context
                                                         :results '()
                                                         :successor succ)))
                             finally (return succ)))
                      (nils (nil-fill nils (first successors)))
                      (t (first successors)))
                context)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compile a MULTIPLE-VALUE-CALL-AST.

(defun compile-two-value-call (client ast context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (let ((quotient-temp (cleavir-ir:new-temporary))
          (remainder-temp (cleavir-ir:new-temporary))
          (function-temp (cleavir-ir:new-temporary)))
      (compile-ast
       client
       (cleavir-ast:function-form-ast ast)
       (clone-context
        context
        :result function-temp
        :successor
        (compile-ast
         client
         (first (cleavir-ast:form-asts ast))
         (clone-context
          context
          :results (list quotient-temp remainder-temp)
          :successor
          (make-instance 'cleavir-ir:funcall-instruction
            :inputs (list function-temp quotient-temp remainder-temp)
            :outputs '()
            :successors
            (if (eq results :values)
                successors
                (list (make-instance 'cleavir-ir:multiple-to-fixed-instruction
                        :outputs results
                        :successor (first successors))))))))))))

(defun compile-multiple-value-call (client ast context)
  (with-accessors ((results results)
                   (successors successors))
      context
    (assert-context ast context nil 1)
    (let* ((function-temp (cleavir-ir:new-temporary))
           (values-temp (cleavir-ir:new-temporary))
           (successor
             (make-instance 'cleavir-ir:multiple-value-call-instruction
               :inputs (list function-temp values-temp)
               :outputs '()
               :successors
               (if (eq results :values)
                   successors
                   (list (make-instance 'cleavir-ir:multiple-to-fixed-instruction
                           :inputs '()
                           :outputs results
                           :successor (first successors)))))))
      (loop for form-ast in (reverse (cleavir-ast:form-asts ast))
            do (setf successor
                     (compile-ast
                      client
                      form-ast
                      (clone-context
                       context
                       :results :values
                       :successor
                       (make-instance 'cleavir-ir:append-values-instruction
                         :output values-temp
                         :successor successor)))))
      (compile-ast
       client
       (cleavir-ast:function-form-ast ast)
       (clone-context
        context
        :result function-temp
        :successor
        (make-instance 'cleavir-ir:initialize-values-instruction
          :output values-temp
          :successor successor))))))

(defmethod compile-ast (client (ast cleavir-ast:multiple-value-call-ast) context)
  (if (and (= (length (cleavir-ast:form-asts ast)) 1)
           (typep (first (cleavir-ast:form-asts ast))
                  'cleavir-ast:two-values-ast-mixin))
      (compile-two-value-call client ast context)
      (compile-multiple-value-call client ast context)))
