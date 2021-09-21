(cl:in-package #:sicl-compiler)

(defgeneric trivial-constant-p (constant))

(defmethod trivial-constant-p (constant)
  (declare (ignore constant))
  nil)

(defmethod trivial-constant-p ((constant integer))
  (<= #.(- (expt 2 31)) constant #.(1- (expt 2 31))))

(defmethod trivial-constant-p ((constant character))
  (< (char-code constant) #.(expt 2 29)))

;;; Constants will be allocated in a vector on the global heap.  Since
;;; the global heap does not move objects, the distance between a
;;; particular constant and the instruction that needs it does not
;;; change.  So we can use PC-relative addressing to load the
;;; constant.  This way is now reflected in HIR by the use of the
;;; LOAD-CONSTANT-INSTRUCTION.
;;;
;;; We make the LOCATION-INFO of the LOAD-CONSTANT-INSTRUCTION the
;;; index of the vector of constants where the constant is to be
;;; loaded from.  We accumulate the constants we see into a list so
;;; that we can determine whether some constant has been seen before,
;;; and then we reuse it and its index in the vector.  Later, the
;;; vector of constants is stored in the LOAD-CONSTANT-INSTRUCTION, so
;;; that the HIR evaluator ca access it from there at the given index.
;;;
;;; The OFFSET parameter is the number of LOAD-TIME-VALUEs that have
;;; been hoisted.  The value of each LOAD-TIME-VALUE-FORM is stored in
;;; the first part of the vector of constants, so the constants
;;; created by the reader are stored in the second part, beginning at
;;; index OFFSET.
;;;
;;; We store the list of constants we have seen in the code object.
;;; Later, this list will be stored in the second half of the
;;; constants vector and the entire vector will be passed to the
;;; top-level function as an argument, when the code object is tied
;;; to some environment.

(defun process-constant-inputs (code-object)
  (let ((initial-instruction (ir code-object)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for remaining = (cleavir-ir:inputs instruction) then (rest remaining)
             for input = (first remaining)
             until (null remaining)
             do (when (and (typep input 'cleavir-ir:constant-input)
                           (not (trivial-constant-p (cleavir-ir:value input))))
                  (let ((value (cleavir-ir:value input))
                        (temp (make-instance 'cleavir-ir:lexical-location
                                :name (gensym))))
                    (ensure-literal (constants code-object) value)
                    (cleavir-ir:insert-instruction-before
                     (make-instance 'cleavir-ir:load-literal-instruction
                       :location-info (list value)
                       :output temp)
                     instruction)
                    (setf (first remaining) temp)))))
     initial-instruction)
    (cleavir-ir:reinitialize-data initial-instruction)))


