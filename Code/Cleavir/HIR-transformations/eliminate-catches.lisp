(in-package #:cleavir-hir-transformations)

;;;; Eliminates CATCH-INSTRUCTIONS with unused continuations.
;;;; Because they have multiple successors, this is not suitable
;;;; for inclusion in the general remove-useless-instructions.

(defun dead-catch-p (instruction)
  (and (typep instruction 'cleavir-ir:catch-instruction)
       (let ((cont (first (cleavir-ir:outputs instruction))))
         (null (cleavir-ir:using-instructions cont)))))

(defun eliminate-catches (initial-instruction)
  (let ((death nil))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       ;; Update instruction dynenvs by zooming up the nesting until a
       ;; live dynenv is reached.
       (do* ((dynenv (cleavir-ir:dynamic-environment-location instruction)
                     (cleavir-ir:dynamic-environment-location dynenv-definer))
             (dynenv-definer (first (cleavir-ir:defining-instructions dynenv))
                             (first (cleavir-ir:defining-instructions dynenv))))
           ((not (dead-catch-p dynenv-definer))
            (setf (cleavir-ir:dynamic-environment-location instruction) dynenv)))
       (when (dead-catch-p instruction)
         (push instruction death)))
     initial-instruction)
    (dolist (catch death)
      ;; Modify the flow graph.
      (cleavir-ir:bypass-instruction (cleavir-ir:first-successor catch) catch))
    (cleavir-ir:reinitialize-data initial-instruction)
    death))
