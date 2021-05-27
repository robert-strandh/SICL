(cl:in-package #:cleavir-def-use-chains)

;;; We return a list of def-use chains.  Each def-use chain is
;;; represented by a list whose CAR is the DEFINITION (i.e, a CONS of
;;; a NODE and a VARIABLE), and whose CDR is a list of nodes where the
;;; definition is used.
(defun def-use-chains (start-node successor-fun in-fun out-fun)
  (let ((reaching-definitions
          (cleavir-reaching-definitions:reaching-definitions
           start-node successor-fun out-fun))
        (def-use-chains (make-hash-table :test #'eq)))
    (cleavir-utilities:map-nodes
     start-node
     successor-fun
     (lambda (node)
       (loop for reaching in (gethash node reaching-definitions)
             do (when (member (cdr reaching) (funcall in-fun node))
                  (push node (gethash reaching def-use-chains))))))
    (let ((result '()))
      (maphash (lambda (definition nodes)
                 (push (cons definition nodes) result))
               def-use-chains)
      result)))
