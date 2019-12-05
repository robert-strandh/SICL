(cl:in-package #:cleavir-ir)

(defun all (&rest arguments)
  (notany #'null arguments))

(defun none (&rest arguments)
  (every #'null arguments))

(defun all-or-none (&rest arguments)
  (or (apply #'all arguments)
      (apply #'none arguments)))

(defun combine (combination &rest arguments)
  (if (apply #'none arguments)
      combination
      (copy-list arguments)))

(defmacro normalize-arguments
    (instruction-class-name input-names output-names successor-names)
  `(defmethod shared-initialize :around
     ((instruction ,instruction-class-name)
      slot-names
      &rest keys
      &key
      (inputs nil inputs-p) ,@input-names
      (outputs nil outputs-p) ,@output-names
      (successors nil successors-p) ,@successor-names)
     (declare (ignore inputs outputs successors))
     (cond ((or ,@input-names)
            (assert (and ,@input-names))
            (assert (not inputs-p))
            (cond ((or ,@output-names)
                   (assert (and ,@output-names))
                   (assert (not outputs-p))
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :outputs (list ,@output-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :outputs (list ,@output-names)
                                 keys))))
                  (t
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 keys))))))
           (t
            (cond ((or ,@output-names)
                   (assert (and ,@output-names))
                   (assert (not outputs-p))
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :outputs (list ,@output-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 :outputs (list ,@output-names)
                                 keys))))
                  (t
                   (cond ((or ,@successor-names)
                          (assert (and ,@successor-names))
                          (assert (not successors-p))
                          (apply #'call-next-method instruction slot-names
                                 :inputs (list ,@input-names)
                                 :successors (list ,@successor-names)
                                 keys))
                         (t
                          (apply #'call-next-method instruction slot-names
                                 keys)))))))))
