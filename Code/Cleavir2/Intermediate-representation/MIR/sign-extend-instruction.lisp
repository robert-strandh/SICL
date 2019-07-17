(cl:in-package #:cleavir-ir)

(defclass sign-extend-instruction (instruction one-successor-mixin)
  ())

(defmethod shared-initialize :around
    ((instruction sign-extend-instruction) slot-names
     &key
       inputs input
       outputs output
       successors successor)
  (let ((inputs (if (null input) inputs (list input)))
        (outputs (if (null output) outputs (list output)))
        (successors (if (null successor) successors (list successor))))
    (call-next-method instruction slot-names
                      :inputs inputs :outputs outputs :successors successors)))
