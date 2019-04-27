(cl:in-package #:sicl-hir-to-cl)

(defvar *visited*)

(defgeneric translate (instruction))

(defmethod translate :around (instruction)
  (let ((tag (gethash instruction *visited*)))
    (if (null tag)
        (progn (setf (gethash instruction *visited*) (gensym))
               (call-next-method))
        `((go ,tag)))))
