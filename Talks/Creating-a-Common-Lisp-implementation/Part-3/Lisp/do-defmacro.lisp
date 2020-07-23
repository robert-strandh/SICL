(cl:in-package #:target-iteration)

(defmacro do (variable-clauses end-test-clause &body body)
  (let ((again-tag (gensym)))
    `(block nil
       (let ,(mapcar
              (lambda (clause)
                (subseq clause 0 2))
              variable-clauses)
         (tagbody
            ,again-tag
            (when ,(first end-test-clause)
              (return ,(second end-test-clause)))
            ,@body
            ,@(mapcar (lambda (clause)
                        `(setq ,(first clause) ,(third clause)))
                      variable-clauses)
            (go ,again-tag))))))
