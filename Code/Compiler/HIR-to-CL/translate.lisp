(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate (instruction context))

;;; FIXME: Remove this method later.
(defmethod translate (instruction context)
  (cons `(invalid ,instruction)
        (loop for successor in (cleavir-ir:successors instruction)
              append (translate successor context))))

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
        (successor (first (cleavir-ir:successors instruction))))
    (cons `(setq ,(values-location context)
                 (multiple-value-list
                  (apply #'funcall ,(mapcar #'cleavir-ir:name inputs))))
          (translate successor context))))

(defmethod translate ((instruction cleavir-ir:return-instruction) context)
  `((return-from ,(block-name context)
      (apply #'values ,(values-location context)))))

(defmethod translate ((instruction cleavir-ir:enclose-instruction) context)
  (let ((name (cleavir-ir:name (first (cleavir-ir:outputs instruction))))
        (enter (cleavir-ir:code instruction))
        (successor (first (cleavir-ir:successors instruction))))
    `((setq ,name (funcall (aref ,*static-environment-variable* 1)
                           ,(gethash enter (function-names context))
                           ,@(mapcar #'cleavir-ir:name
                                     (cleavir-ir:inputs instruction))))
      (closer-mop:set-funcallable-instance-function
       ,name
       (lambda (&rest args)
         (funcall ,(gethash enter (function-names context))
                  args
                  (funcall ,(static-env-function-var context) ,name)
                  *dynamic-environment*)))
      ,@(translate successor context))))

(defmethod translate ((instruction cleavir-ir:fetch-instruction) context)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (static-environment-input (first inputs))
         (static-environment-input-name (cleavir-ir:name static-environment-input))
         (index-input (second inputs))
         (index (cleavir-ir:value index-input))
         (output (first (cleavir-ir:outputs instruction)))
         (output-name (cleavir-ir:name output))
         (successor (first (cleavir-ir:successors instruction))))
    `((setq ,output-name
            (aref ,static-environment-input-name ,(+ index 2)))
      ,@(translate successor context))))

(defmethod translate ((instruction cleavir-ir:read-cell-instruction) context)
  (let* ((input (first (cleavir-ir:inputs instruction)))
         (input-name (cleavir-ir:name input))
         (output (first (cleavir-ir:outputs instruction)))
         (output-name (cleavir-ir:name output))
         (successor (first (cleavir-ir:successors instruction))))
    `((setq ,output-name
            (car ,input-name))
      ,@(translate successor context))))

(defmethod translate ((instruction cleavir-ir:write-cell-instruction) context)
  (let* ((inputs (cleavir-ir:inputs instruction))
         (cons-input (first inputs))
         (object-input (second inputs))
         (cons-name (cleavir-ir:name cons-input))
         (object-name (cleavir-ir:name object-input))
         (successor (first (cleavir-ir:successors instruction))))
    `((rplaca ,cons-name ,object-name)
      ,@(translate successor context))))

(defmethod translate :around (instruction context)
  (let* ((visited (visited context))
         (tag (gethash instruction visited)))
    (if (null tag)
        (progn (setf (gethash instruction visited) (gensym))
               (cons (gethash instruction visited)
                     (call-next-method)))
        `((go ,tag)))))
