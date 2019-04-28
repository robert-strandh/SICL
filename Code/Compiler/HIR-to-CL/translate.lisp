(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate (instruction context))

;;; FIXME: Remove this method later.
(defmethod translate (instruction context)
  (declare (ignore context))
  (cons `(invalid ,instruction)
        (reduce #'append
                (mapcar #'translate (cleavir-ir:successors instruction)))))

(defmethod translate ((instruction cleavir-ir:assignment-instruction) context)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (output (first (cleavir-ir:outputs instruction)))
         (successor (first (cleavir-ir:successors instruction))))
    (cons `(setq ,(cleavir-ir:name output)
                 ,(if (typep input 'cleavir-ir:constant-input)
                      `',(cleavir-ir:value input)
                      (cleavir-ir:name input)))
          (translate successor context))))

(defmethod translate ((instruction cleavir-ir:funcall-instruction) context)
  (let ((inputs (cleavir-ir:inputs instruction))
        (output (first (cleavir-ir:outputs instruction)))
        (successor (first (cleavir-ir:successors instruction))))
    (cons `(setq ,(cleavir-ir:name output)
                 (multiple-value-list
                  (apply #'funcall ,(mapcar #'cleavir-ir:name inputs))))
          (translate successor context))))

(defmethod translate :around (instruction context)
  (let* ((visited (visited context))
         (tag (gethash instruction visited)))
    (if (null tag)
        (progn (setf (gethash instruction visited) (gensym))
               (cons (gethash instruction visited)
                     (call-next-method)))
        `((go ,tag)))))
