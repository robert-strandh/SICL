(cl:in-package #:cleavir-partial-inlining)

(defmethod inline-one-instruction :around
    (enclose-instruction
     call-instruction
     enter-instruction
     successor-instruction
     mapping)
  (let ((copy (find-in-mapping mapping successor-instruction)))
    (if (null copy)
        '()
        (call-next-method))))
