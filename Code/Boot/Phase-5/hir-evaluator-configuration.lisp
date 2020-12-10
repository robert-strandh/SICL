(cl:in-package sicl-boot-phase-5)

(defmethod sicl-hir-evaluator:instruction-thunk
    ((client client)
     (instruction cleavir-ir:standard-object-class-of-instruction)
     lexical-environment)
  (sicl-hir-evaluator:make-thunk
      (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (sicl-hir-evaluator:output 0)
          (let ((object (sicl-hir-evaluator:input 0)))
            (cond ((typep object 'sicl-boot::header)
                   (slot-value object 'sicl-boot::%class))
                  ((null object)
                   (env:find-class client (e5 client) 'null))
                  ((symbolp object)
                   (env:find-class client (e5 client) 'symbol))
                  ((stringp object)
                   (env:find-class client (e5 client) 'string))
                  (t
                   (error "Class of ~s asked for in E5" object)))))
    (sicl-hir-evaluator:successor 0)))
