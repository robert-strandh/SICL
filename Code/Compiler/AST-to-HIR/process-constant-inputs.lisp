(cl:in-package #:sicl-ast-to-hir)

(defgeneric trivial-constant-p (constant))

(defmethod trivial-constant-p (constant)
  (declare (ignore constant))
  nil)

(defmethod trivial-constant-p ((constant integer))
  (<= #.(- (expt 2 31)) constant #.(1- (expt 2 31))))

(defmethod trivial-constant-p ((constant character))
  (< (char-code constant) #.(expt 2 29)))

;;; We allocate literals in the global heap.  Since objects there do
;;; not move, a literal is represented as an immediate (full-word)
;;; value in the instruction stream of native code.  But in order to
;;; make sure that the garbage collector can find those literals, we
;;; also keep them around in a vector in the code object.

(defun process-constant-inputs (initial-instruction constants)
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
                  (sicl-compiler:ensure-literal constants value)
                  (cleavir-ir:insert-instruction-before
                   (make-instance 'cleavir-ir:load-literal-instruction
                     :location-info (list value)
                     :output temp)
                   instruction)
                  (setf (first remaining) temp)))))
   initial-instruction)
  (cleavir-ir:reinitialize-data initial-instruction))
