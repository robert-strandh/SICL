(cl:in-package #:cleavir-partial-inlining)

(defun translate-input-for-copy (input external-map internal-map stack)
  (declare (ignore stack))
  (or (find-in-mapping internal-map input)
      (find-in-mapping external-map input)
      input))
(defun translate-inputs-for-copy (inputs external-map internal-map stack)
  (loop for input in inputs
        collect (translate-input-for-copy input external-map internal-map stack)))

(defun translate-output-for-copy (output external-map internal-map stack)
  ;; We assume that if a variable is closed over it will be in the external map
  ;; during this copy. I think this is true with normally generated HIR.
  (or (find-in-mapping internal-map output)
      (find-in-mapping external-map output)
      (if (member (gethash output *location-ownerships*) stack)
          (let ((new (if (typep output 'cleavir-ir:values-location)
                         (cleavir-ir:make-values-location)
                         (cleavir-ir:make-lexical-location
                          (cleavir-ir:name output)))))
            (add-to-mapping external-map output new)
            new)
          output)))
(defun translate-outputs-for-copy (outputs external-map internal-map stack)
  (loop for output in outputs
        collect (translate-output-for-copy output external-map internal-map stack)))

;;; EXTERNAL-MAP is the mapping from inline, and is only read.
;;; INTERNAL-MAP is a fresh mapping which is used only for internal
;;; locations, and is written to.
;;; STACK is a list of ENTER-INSTRUCTIONS that we're in the middle
;;; of copying. If an output is owned by something in the stack, it
;;; ought to be copied, but if not, it's something closed over and
;;; must not be copied.
(defun copy-function-recur (enter external-map internal-map stack)
  (let ((copies nil)
        (stack (cons enter stack)))
    ;; First loop: Copy all instructions in the function, but leave
    ;; predecessors and successors disconnected.
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (let* ((inputs (cleavir-ir:inputs instruction))
              (outputs (cleavir-ir:outputs instruction))
              (new-inputs (translate-inputs-for-copy inputs external-map internal-map stack))
              (new-outputs (translate-outputs-for-copy outputs external-map internal-map stack))
              (copy (cleavir-ir:clone-instruction instruction
                :inputs new-inputs :outputs new-outputs)))
         (push copy copies)
         (add-to-mapping *instruction-mapping* instruction copy)))
     enter)
    ;; Second loop: Loop over the copies doing hookups.
    (flet ((maybe-replace (instruction)
             (let ((copy (find-in-mapping *instruction-mapping* instruction)))
               (or copy instruction))))
      (loop for copy in copies
            do (setf (cleavir-ir:predecessors copy)
                     (mapcar #'maybe-replace (cleavir-ir:predecessors copy))
                     (cleavir-ir:successors copy)
                     (mapcar #'maybe-replace (cleavir-ir:successors copy)))
            do (typecase copy
                 (cleavir-ir:enclose-instruction
                  ;; We have to do this in the second loop so that any
                  ;; UNWINDS in the inner function can be hooked up to copies.
                  (setf (cleavir-ir:code copy)
                        (copy-function-recur
                         (cleavir-ir:code copy) external-map internal-map stack)))
                 (cleavir-ir:unwind-instruction
                  (setf (cleavir-ir:destination copy)
                        (maybe-replace (cleavir-ir:destination copy)))))))
    (find-in-mapping *instruction-mapping* enter)))

;;; Returns a copy of a HIR function.
;;; This is required because an internal function could have things it's mapped
;;; over refer to inputs that were copied by inlining.
;;; ENTER is an enter instruction representing the function being copied.
;;; MAPPING is the lexical variable mapping used during this inline.
;;; The copy of ENTER is returned.
(defun copy-function (enter mapping)
  (copy-function-recur enter mapping (make-hash-table :test #'eq) nil))
