(cl:in-package #:sicl-hir-to-cl)

(defvar *visited*)

(defgeneric translate (instruction))

;;; FIXME: Remove this method later.
(defmethod translate (instruction)
  (cons `(invalid ,instruction)
        (reduce #'append
                (mapcar #'translate (cleavir-ir:successors instruction)))))

(defmethod translate :around (instruction)
  (let ((tag (gethash instruction *visited*)))
    (if (null tag)
        (progn (setf (gethash instruction *visited*) (gensym))
               (call-next-method))
        `((go ,tag)))))
