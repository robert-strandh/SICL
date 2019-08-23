(cl:in-package #:sicl-hir-transformations)

(defclass top-level-enter-instruction (cleavir-ir:top-level-enter-instruction)
  ((%constants :initform '() :accessor constants)))

(defgeneric trivial-constant-p (constant))

(defmethod trivial-constant-p (constant)
  (declare (ignore constant))
  nil)

(defmethod trivial-constant-p ((constant integer))
  (<= #.(- (expt 2 31)) #. (1- (expt 2 31))))

(defmethod trivial-constant-p ((constant character))
  (< (char-code constant) #.(expt 2 29)))

(defun hoist-constant-inputs (initial-instruction)
  (with-accessors ((constants constants))
      initial-instruction
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for input in (cleavir-ir:inputs instruction)
             do (when (and (typep input 'cleavir-ir:constant-input)
                           (not (trivial-constant-p (cleavir-ir:value input))))
                  (let* ((value (cleavir-ir:value input))
                         (pos (position value constants)))
                    (if (null pos)
                        (setf constants (append constants (list value)))
                        nil)))))
     initial-instruction)))
