(cl:in-package #:cleavir-loops)

;;;; Loops.
;;;;
;;;; A loop can only be the result of a TAGBODY form.  Whenever there
;;;; is a non-trivial strongly connected component in the flow graph,
;;;; we have a loop.  The only restriction on loops in Common Lisp is
;;;; that the TAGBODY form introduces a unique node that
;;;; dominates every other node in the strongly connected
;;;; component.  Another way of saying the same thing is that it is not
;;;; possible to transfer control from the outside of a TAGBODY form
;;;; into it (we can ignore control arcs from nested procedures).
;;;;
;;;; If we compute a dominance tree of all the nodes in a
;;;; strongly connected component S, then a nested loop shows up as a
;;;; (sub) strongly connected component immediately dominated by some
;;;; other node than the one that dominates the entire component
;;;; S.  Let us say that I0 is the node that dominates every
;;;; node in the strongly connected component.  Let I10, I11,
;;;; ..., I1n be the nodes in S that are immediately dominated
;;;; by I0.  If we remove I0 and I10, I11, ... I1n from S and the
;;;; remaining nodes contain a strongly connected component T,
;;;; this means that there is a nested loop, and since I0 is not the
;;;; node that immediately dominates them, there must be some
;;;; other node I that does.  
;;;;
;;;; Notice that a nested loop detected this way may or may not be the
;;;; result of a nested TAGBODY form.  It could also appear because the
;;;; tags and the GO forms of the top-level loop form such a nested
;;;; loop "by accident".  But we still want to count it as a nested
;;;; loop.  And it is unlikely that such a nested loop would appear by
;;;; accident.  
;;;;
;;;; Finally, it is possible that a nested loop that appears by
;;;; accident, i.e., that is not the result of a nested TAGBODY form,
;;;; can not be detected by the method describe above, because there is
;;;; no restriction preventing a GO into the middle of it.  As a result
;;;; we might have a real nested loop, but since the nodes in it
;;;; are all directly dominated by I0, then we won't find it.

;;; Kosaraju's algoritm is a simple algorithm for finding the strongly
;;; connected components of a flow graph.  The return value is a set
;;; (represented as a list) of strongly connected components, each
;;; represented as a list of nodes.  A trivial component will be
;;; represented as a list of a single element.
(defun kosaraju (start-node successor-fun)
  (let ((pre (cleavir-utilities:depth-first-search-preorder
              start-node successor-fun))
        (table (make-hash-table :test #'eq))
        (result '())
        (temp '()))
    (let ((predecessor-fun (cleavir-utilities:predecessor-function
                            start-node successor-fun)))
      (labels ((traverse (node)
                 (unless (gethash node table)
                   (setf (gethash node table) t)
                   (loop for pred in (funcall predecessor-fun node)
                         do (traverse pred))
                   (push node temp))))
        (loop until (null pre)
              for initial = (pop pre)
              unless (gethash initial table)
                do (setf temp '())
                   (traverse initial)
                   (push temp result))
        result))))
