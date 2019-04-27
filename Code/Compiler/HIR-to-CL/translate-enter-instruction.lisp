(cl:in-package #:sicl-hir-to-cl)

(defun make-let-bindings (lambda-list)
  (loop for item in lambda-list
        unless (member item lambda-list-keywords)
          append (cond ((atom item)
                        (list (cleavir-ir:name item)))
                       ((= (length item) 2)
                        (list (cleavir-ir:name (first item))
                              (cleavir-ir:name (second item))))
                       (t
                        (list (cleavir-ir:name (second item))
                              (cleavir-ir:name (third item)))))))

(defun translate-enter-instruction (enter-instruction) 
  (let* ((lambda-list (cleavir-ir:lambda-list enter-instruction))
         (successor (first (cleavir-ir:successors enter-instruction))))
    `(let ,(make-let-bindings lambda-list)
       (tagbody ,@(translate successor)))))



