(cl:in-package #:sicl-hir-evaluator)

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:return-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :successors 0)
    (throw 'return
      (values-list *global-values-location*))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:funcall-instruction)
     lexical-environment)
  (let* ((input-indices
           (loop for input in (cleavir-ir:inputs instruction)
                 collect (value-index input lexical-environment))))
    (macrolet ((fixed-arity-call (arity)
                 `(make-thunk (client instruction lexical-environment :inputs ,arity)
                    (let ((sicl-run-time:*dynamic-environment* dynamic-environment)
                          (call-stack-entry
                            (make-instance 'call-stack-entry
                              :origin (cleavir-ast-to-hir:origin instruction)
                              :arguments
                              (loop for input-index in input-indices
                                   collect (lref input-index)))))
                      (setf *global-values-location*
                            (multiple-value-list
                             (let* ((*call-stack* (cons call-stack-entry *call-stack*)))
                               (funcall
                                ,@(loop for index below arity collect `(input ,index))))))
                      (successor 0)))))
      (case (length (cleavir-ir:inputs instruction))
        (0 (error "Funcall instruction with zero inputs."))
        (1 (fixed-arity-call 1))
        (2 (fixed-arity-call 2))
        (3 (fixed-arity-call 3))
        (4 (fixed-arity-call 4))
        (5 (fixed-arity-call 5))
        (6 (fixed-arity-call 6))
        (7 (fixed-arity-call 7))
        (8 (fixed-arity-call 8))
        (otherwise
         (make-thunk (client instruction lexical-environment)
           (let ((sicl-run-time:*dynamic-environment* dynamic-environment)
                 (call-stack-entry
                   (make-instance 'call-stack-entry
                     :origin (cleavir-ast-to-hir:origin instruction)
                     :arguments
                     (loop for input-index in input-indices
                                   collect (lref input-index)))))
             (setf *global-values-location*
                   (multiple-value-list
                    (let ((*call-stack* (cons call-stack-entry *call-stack*)))
                      (apply #'funcall
                             (loop for input-index in input-indices
                                   collect (lref input-index))))))
             (successor 0))))))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:assignment-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :outputs 1)
    (setf (output 0) (input 0))
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:eq-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 2 :successors 2)
    (if (eq (input 0) (input 1))
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:consp-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :successors 2)
    (if (consp (input 0))
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:fixnump-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :successors 2)
    ;; FIXME: This test is wrong if the host and the target have a
    ;; different fixnum range.
    (if (typep (input 0) 'fixnum)
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:characterp-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :successors 2)
    (if (characterp (input 0))
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:single-float-p-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :successors 2)
    (if (typep (input 0) 'single-float)
        (successor 0)
        (successor 1))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:standard-object-p-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :inputs 1 :successors 2)
    (if (or (characterp (input 0))
            (typep (input 0) 'fixnum) ; Same problem as with the fixnump instruction above.
            (typep (input 0) 'single-float)
            (consp (input 0)))
        (successor 1)
        (successor 0))))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:nop-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment)
    (successor 0)))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:unreachable-instruction)
     lexical-environment)
  (make-thunk (client instruction lexical-environment :successors 0)
    (error "Reached an unreachable instruction.")))

(defmethod instruction-thunk
    (client
     (instruction cleavir-ir:load-constant-instruction)
     lexical-environment)
  (let ((constant (cdr (cleavir-ir:location-info instruction))))
    (make-thunk (client instruction lexical-environment :outputs 1)
      (setf (output 0) constant)
      (successor 0))))
