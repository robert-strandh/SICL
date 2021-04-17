(cl:in-package #:cleavir-ast-transformations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Replacing a node in an AST.
;;;
;;; Given an AST and a FUNCTION, the FUNCTION is applied to each node
;;; of the AST.  If on some node N, the function returns NIL then N is
;;; cloned.  Otherwise, the return value of FUNCTION is a node that
;;; should replace N in the copy.

(defun reinitialize (instance keyword replacement)
  (reinitialize-instance instance keyword replacement))

(defun replace-ast (ast function)
  (let ((dictionary (make-hash-table :test #'eq)))
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (let ((replacement (funcall function node)))
         (unless (null replacement)
           (setf (gethash node dictionary) replacement))))
     ast)
    (cleavir-ast:map-ast-depth-first-preorder
     (lambda (node)
       (loop for (keyword reader) in (cleavir-io:save-info node)
             for value = (funcall reader node)
             for replacement = (gethash value dictionary)
             unless (null replacement)
               do (reinitialize node keyword replacement)))
     ast)))
