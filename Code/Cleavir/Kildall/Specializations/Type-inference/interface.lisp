(in-package #:cleavir-kildall-type-inference)

(defun infer-types (initial-instruction environment
                    &key prune draw
                      (liveness
                       (cleavir-liveness:liveness
                        initial-instruction)))
  ;; controlled by policy (see insert-type-checks.lisp)
  (thes->typeqs initial-instruction environment)
  (let* ((s (make-instance 'type-inference :env environment
                                           :liveness liveness))
         (d (cleavir-kildall:kildall s initial-instruction)))
    ;; since drawing is just for debugging, it's probably more
    ;; useful to do before the dangerous pruning step.
    (when draw
      (cleavir-kildall-graphviz:draw-flowchart-with-inputs
       initial-instruction draw s d))
    (when prune
      (prune-typeqs initial-instruction s d))
    (function-type initial-instruction s d)))

(defun function-type (enter specialization dictionary)
  (let* ((return (cleavir-kildall:entry-return
                  (cleavir-kildall:find-entry
                   specialization enter
                   :key #'cleavir-kildall:entry-enter)))
         (return-descriptor
           (cleavir-kildall:find-in-pool specialization
            (first (cleavir-ir:inputs return))
            (cleavir-kildall:instruction-pool return dictionary)))
         (return-type
           ;; This assumes that no unboxed descriptors, which have
           ;; no corresponding type specifiers, are involved.
           (cleavir-type-descriptors:descriptor->specifier
            return-descriptor (environment specialization)))
         (lambda-list
           (loop for thing in (cleavir-ir:lambda-list enter)
                 when (symbolp thing) ; lambda list keyword
                   collect thing
                 else collect t)))
    `(function ,lambda-list ,return-type)))
