(cl:in-package #:sicl-hir-to-cl)

;;; Return 4 values:
;;;
;;;   * A list of reuqired parameters, each one represented as a
;;;     single lexical location
;;;
;;;   * A list of &OPTIONAL parameters, each one represented as a
;;;     list of two lexical locations, one for the parameter and
;;;     one for the supplied-p parameter.
;;;
;;;   * A &REST parameter represented as a single lexical location,
;;;     or NIL of there is no &REST parameter.
;;;
;;;   * A list of &KEY parameters, each one represented as a list
;;;     of three elements, a symbol and two lexical locations.
(defun split-lambda-list (lambda-list)
  (let ((pos (position-if (lambda (x) (member x lambda-list-keywords))
                          lambda-list)))
    (when (null pos)
      (return-from split-lambda-list
        (values lambda-list '() nil '())))
    (let ((required-parameters (subseq lambda-list 0 pos))
          (remaining (subseq lambda-list pos))
          (optional-parameters '())
          (rest-parameter nil)
          (key-parameters '()))
      (loop for item in remaining
            do (cond ((member item lambda-list-keywords)
                      nil)
                     ((atom item)
                      (setq rest-parameter item))
                     ((= (length item) 2)
                      (push item optional-parameters))
                     (t
                      (push item key-parameters))))
      (values required-parameters
              (reverse optional-parameters)
              rest-parameter
              (reverse key-parameters)))))

(defun translate-enter-instruction (client enter-instruction context)
  (let* ((static-environment-output
           (cleavir-ir:static-environment enter-instruction))
         (dynamic-environment-output
           (cleavir-ir:dynamic-environment-output enter-instruction))
                  (successor (first (cleavir-ir:successors enter-instruction)))
         (*arguments-variable* (gensym "arguments"))
         (static-environment-variable (cleavir-ir:name static-environment-output))
         (*static-environment-variable* static-environment-variable)
         (dynamic-environment-variable (cleavir-ir:name dynamic-environment-output))
         (lexical-locations (find-valid-lexical-locations
                             enter-instruction
                             static-environment-output
                             dynamic-environment-output))
         (lexical-location-names (mapcar #'cleavir-ir:name lexical-locations))
         (basic-blocks (compute-basic-blocks enter-instruction))
         (*basic-blocks-in-dynamic-environment* (make-hash-table :test #'eq))
         (*basic-block-of-leader* (make-hash-table :test #'eq))
         (*tag-of-basic-block* (make-hash-table :test #'eq)))
    (process-basic-blocks basic-blocks)
    `(lambda (,*arguments-variable*
              ,static-environment-variable
              ,dynamic-environment-variable)
       (declare (ignorable ,static-environment-variable
                           ,dynamic-environment-variable))
       (block ,(block-name context)
         (let (,@lexical-location-names
               (source nil))
           (declare (ignorable ,@lexical-location-names))
           (tagbody (go ,(tag-of-basic-block (basic-block-of-leader successor)))
              ,@(loop with dynamic-environment-location
                        = (cleavir-ir:dynamic-environment-location successor)
                      with basic-blocks
                        = (basic-blocks-in-dynamic-environment
                           dynamic-environment-location)
                      for basic-block in basic-blocks
                      collect (tag-of-basic-block basic-block)
                      append (translate-basic-block
                              client
                              basic-block
                              context))))))))

(defmethod translate (client (instruction cleavir-ir:enclose-instruction) context)
  (let ((name (cleavir-ir:name (first (cleavir-ir:outputs instruction))))
        (enter (cleavir-ir:code instruction)))
    `((setq ,name
            (enclose ,(gethash enter (function-names context))
                     (aref ,*static-environment-variable* 0)
                     ,@(mapcar #'cleavir-ir:name
                               (cleavir-ir:inputs instruction))))
      (closer-mop:set-funcallable-instance-function
       ,name
       (lambda (&rest args)
         (funcall ,(gethash enter (function-names context))
                  args
                  (funcall ,(static-env-function-var context) ,name)
                  *dynamic-environment*))))))
