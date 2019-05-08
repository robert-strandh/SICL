(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate-last-instruction (client instruction context))

(defmethod translate-last-instruction (client instruction context)
  (append (translate client instruction context)
          '()))

(defun translate-basic-block (client basic-block context)
  (let* ((instructions (instructions basic-block))
         (last-instruction (first (last instructions))))
    (append (loop for instruction in (butlast instructions)
                  append (translate client instruction context))
            (translate-last-instruction client last-instruction context))))
