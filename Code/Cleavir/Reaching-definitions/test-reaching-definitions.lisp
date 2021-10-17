(in-package #:cleavir-reaching-definitions-test)

;;;; To test the reaching definitions, we essentially program it
;;;; again, in a different way.  This time we take a single
;;;; definition, and we see what nodes it reaches by traversing the
;;;; graph starting at that node.  We call this function for every
;;;; definition in some random graph, and we compare the result to
;;;; what the call to REACHING-DEFINITIONS returns.  We compare it
;;;; both ways, i.e., if some node can be reached the stupid way, then
;;;; its definition had better be in the result of
;;;; REACHING-DEFINITIONS, and if some node is not in the result of
;;;; REACHING-DEFINITIONS, then it must not be possible to reach it
;;;; the stupid way.

;;; Return a hash table in which the keys are the nodes that can be
;;; reached from a definition of VAR in NODE.
(defun nodes-reached-by-definition (node var successor-fun output-fun)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (node path)
               (if (member node path :test #'eq)
                   nil
                   (unless (gethash node table)
                     (setf (gethash node table) t)
                     (unless (member var (funcall output-fun node)
                                     :test #'eq)
                       (loop for succ in (funcall successor-fun node)
                             do (traverse succ (cons node path))))))))
      (loop for succ in (funcall successor-fun node)
            do (traverse succ '())))
    table))
                 
(defun test-reaching-definitions-on-one-graph
    (start-node successor-fun output-fun)
  (let ((reaching-definitions
          (cleavir-reaching-definitions:reaching-definitions
           start-node successor-fun output-fun)))
    (cleavir-utilities:map-nodes
     start-node
     successor-fun
     (lambda (node)
       (loop for var in (funcall output-fun node)
             do (let ((nodes-reached (nodes-reached-by-definition
                                      node var successor-fun output-fun)))
                  (loop for n being each hash-key of nodes-reached
                        do (assert (member (cons node var)
                                           (gethash n reaching-definitions)
                                           :test #'equal)))
                  (loop for n being each hash-key of reaching-definitions
                        do (if (gethash n nodes-reached)
                               (assert (member
                                        (cons node var)
                                        (gethash n reaching-definitions)
                                        :test #'equal))
                               (assert (not (member
                                             (cons node var)
                                             (gethash n reaching-definitions)
                                             :test #'equal)))))))))))

(defun test-reaching-definitions (&optional (n 10000))
  (loop repeat n
        do (multiple-value-bind (start-node successor-fun in-fun out-fun)
               (cleavir-test-utilities:random-flow-chart)
             (declare (ignore in-fun))
             (test-reaching-definitions-on-one-graph
              start-node successor-fun out-fun))))
