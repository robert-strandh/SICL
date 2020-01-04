(cl:in-package #:sicl-code-generation)

(defvar *labels*)

(defun find-instruction-label (instruction)
  (gethash instruction *labels*))

(defun generate-code (lir)
  (let* ((instructions (linearize-lir lir))
         (*labels* (create-instruction-labels instructions)))
    (loop for instruction in instructions
          for label = (find-instruction-label instruction)
          for translation = (translate-instruction instruction)
          unless (null label)
            collect label
          if (listp translation)
            append translation
          else
            collect translation)))
