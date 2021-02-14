(cl:in-package #:sicl-compiler)

(defgeneric trivial-constant-p (constant))

(defmethod trivial-constant-p (constant)
  (declare (ignore constant))
  nil)

(defmethod trivial-constant-p ((constant integer))
  (<= #.(- (expt 2 31)) #. (1- (expt 2 31))))

(defmethod trivial-constant-p ((constant character))
  (< (char-code constant) #.(expt 2 29)))

;;; Constants will be allocated in a vector on the global heap.  Since
;;; the global heap does not move objects, the distance between a
;;; particular constant and the instruction that needs it does not
;;; change.  So we can use PC-relative addressing to load the
;;; constant.  This way is now reflected in HIR by the use of the
;;; LOAD-CONSTANT-INSTRUCTION.
;;;
;;; We make the LOCATION-INFO of the LOAD-CONSTANT-INSTRUCTION a CONS
;;; of two things.  The first thing is the position of the constant in
;;; what will ultimately become the vector, but during HIR
;;; transformations, it is a list contained in the
;;; TOP-LEVEL-ENTER-INSTRUCTION.  This position information is used
;;; when HIR is transformed into MIR, LIR, etc.  The second thing is
;;; the constant itself, and it is used only by the HIR evaluator.
;;;
;;; The idea is to handle LOAD-TIME-VALUE in a similar way, but we
;;; haven't worked out the details yet.  Again a
;;; LOAD-CONSTANT-INSTRUCTION would be used, but constant to load
;;; would be computed by the top-level function and inserted into the
;;; vector.  An alternative would be to treat all non-trivial
;;; constants as LOAD-TIME-VALUEs.  The difference then would be that
;;; true constants would then be created at load time, rather than
;;; when the AST is read by the reader.
(defun process-constant-inputs (code-object offset)
  (with-accessors ((constants constants)
                   (initial-instruction ir))
      code-object
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
                      (setf constants (append constants (list value))))
                    (let ((temp (make-instance 'cleavir-ir:lexical-location
                                  :name (gensym))))
                      (cleavir-ir:insert-instruction-before
                       (make-instance 'sicl-ir:load-constant-instruction
                         :location-info (+ pos offset)
                         :output temp)
                       instruction)
                      (setf (first remaining) temp))))))
     initial-instruction)
    (cleavir-ir:reinitialize-data initial-instruction)))
