(cl:in-package #:cleavir-def-use-chains-test)

(defun nodes-used-by-definition (node var successor-fun in-fun out-fun)
  (let ((result '()))
    (labels ((traverse (node path)
               (if (or (member node path :test #'eq)
                       (member node result :test #'eq))
                   nil
                   (progn (when (member var (funcall in-fun node)
                                        :test #'eq)
                            (push node result))
                          (unless (member var (funcall out-fun node)
                                          :test #'eq)
                            (loop for succ in (funcall successor-fun node)
                                  do (traverse succ (cons node path))))))))
      (loop for succ in (funcall successor-fun node)
            do (traverse succ '())))
    result))

(defun test-def-use-chains-one-graph (start-node succ-fun in-fun out-fun)
  (let ((def-use-chains
          (cleavir-def-use-chains:def-use-chains
              start-node succ-fun in-fun out-fun)))
    ;; First check that each def-use chain is correct, i.e., that the
    ;; use nodes mentioned in it are precisely the nodes that can be
    ;; reached by the definition.
    (loop for (def . uses) in def-use-chains
          do (let* ((nodes (nodes-used-by-definition
                            (car def) (cdr def) succ-fun in-fun out-fun)))
               (assert (and (subsetp nodes uses :test #'eq)
                            (subsetp uses nodes :test #'eq)))))
    ;; Next check that for every definition, there is a def-use-chain
    ;; that containes precisely the nodes that can be reached by that
    ;; definition.
    (cleavir-utilities:map-nodes
     start-node succ-fun
     (lambda (node)
       (loop for var in (funcall out-fun node)
             for nodes = (nodes-used-by-definition
                          node var succ-fun in-fun out-fun)
             for uses = (cdr (assoc (cons node var) def-use-chains
                                    :test #'equal))
             do (assert (and (subsetp nodes uses :test #'eq)
                             (subsetp uses nodes :test #'eq))))))))

(defun test-def-use-chains (&optional (n 10000))
  (loop repeat n
        do (multiple-value-bind (start-node succ-fun in-fun out-fun)
               (cleavir-test-utilities:random-flow-chart)
             (test-def-use-chains-one-graph
              start-node succ-fun in-fun out-fun))))
