(cl:in-package #:sicl-code-generation)

(defvar *labels*)

(defun find-instruction-label (instruction)
  (gethash instruction *labels*))

(defun generate-code (lir)
  (let* ((instructions (linearize-lir lir))
         (*labels* (create-instruction-labels instructions)))
    (loop for instruction in instructions
          for translation = (translate-instruction instruction)
          if (listp translation)
            append translation
          else
            collect translation)))
