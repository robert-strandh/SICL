(cl:in-package #:sicl-hir-transformations)

(defclass top-level-enter-instruction (cleavir-ir:top-level-enter-instruction)
  ((%constants :initform '() :accessor constants)
   (%function-names :initform '() :accessor function-names)))

(defgeneric trivial-constant-p (constant))

(defmethod trivial-constant-p (constant)
  (declare (ignore constant))
  nil)

(defmethod trivial-constant-p ((constant integer))
  (<= #.(- (expt 2 31)) #. (1- (expt 2 31))))

(defmethod trivial-constant-p ((constant character))
  (< (char-code constant) #.(expt 2 29)))

(defun hoist-constant-inputs (initial-instruction)
  (let ((locations '())
        (static-environment-location
          (cleavir-ir:static-environment initial-instruction)))
    (with-accessors ((constants constants)
                     (function-names function-names))
        initial-instruction
      (cleavir-ir:map-instructions-arbitrary-order
       (lambda (instruction)
         (loop for remaining = (cleavir-ir:inputs instruction) then (rest remaining)
               for input = (first remaining)
               until (null remaining)
               do (when (and (typep input 'cleavir-ir:constant-input)
                             (not (trivial-constant-p (cleavir-ir:value input))))
                    (let* ((value (cleavir-ir:value input))
                           (pos (position value constants)))
                      (when (null pos)
                        (setf pos (length constants))
                        (setf constants (append constants (list value)))
                        (let ((location (make-instance 'cleavir-ir:lexical-location
                                          :name (gensym)))
                              (position (+ pos (length function-names))))
                          (setf locations (append locations (list location)))
                          (cleavir-ir:insert-instruction-after
                           (make-instance 'cleavir-ir:fetch-instruction
                             :inputs (list static-environment-location
                                           (make-instance 'cleavir-ir:constant-input
                                             :value position))
                             :output location)
                           initial-instruction)))
                      (setf (first remaining)
                            (nth pos locations))))))
       initial-instruction)))
  (cleavir-ir:reinitialize-data initial-instruction))
