(cl:in-package #:sicl-code-generation)

(defvar *labels*)

(defun find-instruction-label (instruction)
  (gethash instruction *labels*))

(defun translate-instruction (instruction next)
  (let ((successors (cleavir-ir:successors instruction)))
    (if (< (length successors) 2)
        (let* ((temp (translate-simple-instruction instruction))
               (result (if (listp temp) temp (list temp))))
          (if (or (null successors)
                  (eq (first successors) next))
              result
              (append result
                      (list (make-instance 'cluster:code-command
                              :mnemonic "JMP"
                              :operands
                              (list (find-instruction-label
                                     (first successors))))))))
        (translate-branch-instruction instruction next))))

(defun generate-code (lir)
  (let* ((instructions (linearize-lir lir))
         (*labels* (create-instruction-labels instructions)))
    (loop for (instruction next) on instructions
          for label = (find-instruction-label instruction)
          for translation = (translate-instruction instruction next)
          unless (null label)
            collect label
          append translation)))
