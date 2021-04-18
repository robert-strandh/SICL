(cl:in-package #:cleavir-dominance-test)

(defun depth-first-search-preorder (start-node successor-fun)
  (cleavir-utilities:depth-first-search-preorder
   start-node successor-fun))

(defun name (node)
  (cleavir-test-utilities:name node))

(defun successors (node)
  (cleavir-test-utilities:successors node))

;;; This algorithm applies the definition of dominators by examining
;;; all possible paths from the root of the flow chart to a node N,
;;; and removing any node not on that path from the possible
;;; dominators of N.
(defun slow-dominators (start-node successor-fun)
  (let* ((preorder (depth-first-search-preorder start-node successor-fun))
         (dominators (let ((table (make-hash-table :test #'eq)))
                       (loop for node in preorder
                             do (setf (gethash node table) preorder))
                       table)))
    (flet ((successors (node)
             (funcall successor-fun node))
           (dominators (node)
             (gethash node dominators))
           ((setf dominators) (new-dominators node)
             (setf (gethash node dominators) new-dominators)))
      (loop for node in preorder
            do (labels ((traverse (n path)
                          (cond ((eq n node)
                                 (setf (dominators node)
                                       (intersection (dominators node)
                                                     (cons n path)
                                                     :test #'eq)))
                                ((member n path :test #'eq)
                                 nil)
                                (t
                                 (loop for succ in (successors n)
                                       do (traverse succ (cons n path)))))))
                 (traverse start-node '()))))
    dominators))

(defun test-one-chart (start-node)
  (let ((d1 (cleavir-dominance:dominance-tree start-node #'successors))
        (d2 (slow-dominators start-node #'successors))
        (preorder (depth-first-search-preorder start-node #'successors)))
    (loop for node in preorder
          do (unless (and (null (set-difference (gethash node d1)
                                                (gethash node d2)))
                          (null (set-difference (gethash node d2)
                                                (gethash node d1))))
               (return (values node d1 d2))))))

(defun test-dominance (&optional (n 10000))
  (loop repeat n
        do (let ((f (cleavir-test-utilities:random-flow-chart)))
             (when (< (cleavir-utilities:count-nodes
                       f #'cleavir-test-utilities:successors)
                      50)
               (test-one-chart f)))))

(defun draw-dominance-tree (dominance-tree filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (format stream "digraph G {~%   ordering = in;~%")
    ;; Draw all the nodes first.
    (loop for node being each hash-key of dominance-tree
          do (format stream "   ~a [label = \"~a\"];~%"
                     (name node) (name node)))
    ;; Now draw all the arcs.
    (loop for node being each hash-key of dominance-tree
          do (let ((idom (cleavir-dominance:immediate-dominator
                          dominance-tree node)))
               (unless (null idom)
                 (format stream "   ~a -> ~a;~%"
                         (name node) (name idom)))))
    (format stream "}~%")))

(defun draw-dominance-frontiers
    (start-node successor-fun dominance-frontiers filename)
  (flet ((successors (node)
           (funcall successor-fun node)))
    (let ((table (make-hash-table :test #'eq)))
      (with-open-file (stream filename
                              :direction :output
                              :if-exists :supersede)
        (format stream "digraph G {~% ordering = out;~%")
        (labels ((draw-node (node)
                   (unless (gethash node table)
                     (setf (gethash node table) t)
                     (loop for succ in (successors node)
                           do (draw-node succ))
                     (format stream
                             "   ~a [label = \"~a\"];~%"
                             (name node) (name node))
                     (loop for succ in (successors node)
                           do (format stream
                                      "   ~a -> ~a;~%"
                                      (name node)
                                      (name succ))))))
          (draw-node start-node))
        (loop for node being each hash-key of dominance-frontiers
              do (let ((f (gethash node dominance-frontiers)))
                   (loop for ff in f
                         do (format stream
                                    "   ~a -> ~a [style = bold, color = red];"
                                    (name node) (name ff)))))
        (format stream "}~%")))))
    
;;; The dominance frontier of a node X is defined to be the set of all
;;; nodes Y such that 1: there exists a predecessor Z of Y such that X
;;; dominates Z, and 2: X does not strictly dominate Y.
(defun slow-dominance-frontiers (start-node successor-fun)
  (let ((predecessor-fun (cleavir-utilities:predecessor-function
                          start-node successor-fun))
        (dominance-tree (cleavir-dominance:dominance-tree
                         start-node successor-fun))
        (preorder (cleavir-utilities:depth-first-search-preorder
                   start-node successor-fun))
        (result (make-hash-table :test #'eq)))
    (flet ((predecessors (node)
             (funcall predecessor-fun node))
           (dominates (n1 n2)
             (member n1 (cleavir-dominance:dominators
                         dominance-tree n2)))
           (strictly-dominates (n1 n2)
             (member n1 (cleavir-dominance:strict-dominators
                         dominance-tree n2))))
      (loop for x in preorder
            do (loop for y in preorder
                     do (when (and (some (lambda (z)
                                           (dominates x z))
                                         (predecessors y))
                                   (not (strictly-dominates x y)))
                          (push y (gethash x result))))))
    result))
    
(defun test-dominance-frontiers-one-flow-chart (start-node successor-fun)
  (flet ((same-set-p (s1 s2)
           (and (null (set-difference s1 s2 :test #'eq))
                (null (set-difference s2 s1 :test #'eq)))))
    (let ((f1 (cleavir-dominance:dominance-frontiers
               start-node successor-fun))
          (f2 (slow-dominance-frontiers
               start-node successor-fun)))
      (loop for node being each hash-key of f1
            do (unless (same-set-p (gethash node f1) (gethash node f2))
                 (throw 'discrepancy (values start-node f1 f2))))
      (loop for node being each hash-key of f2
            do (unless (same-set-p (gethash node f1) (gethash node f2))
                 (throw 'discrepancy (values start-node f1 f2)))))))

(defun test-dominance-frontiers (&optional (n 10000))
  (catch 'discrepancy
    (loop repeat n
          do (test-dominance-frontiers-one-flow-chart
              (cleavir-test-utilities:random-flow-chart)
              #'cleavir-test-utilities:successors))))
