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

(defun find-valid-lexical-locations (enter-instruction exclude)
  (set-difference (find-lexical-locations enter-instruction)
                  exclude))

(defun translate-enter-instruction (client enter-instruction context)
  (let* ((lambda-list (cleavir-ir:lambda-list enter-instruction))
         (outputs (cleavir-ir:outputs enter-instruction))
         (static-environment-output (first outputs))
         (dynamic-environment-output (second outputs))
         (successor (first (cleavir-ir:successors enter-instruction)))
         (arguments-variable (gensym "arguments"))
         (static-environment-variable (cleavir-ir:name static-environment-output))
         (*static-environment-variable* static-environment-variable)
         (dynamic-environment-variable (cleavir-ir:name dynamic-environment-output))
         (remaining-variable (gensym))
         (lexical-locations (find-valid-lexical-locations
                             enter-instruction (list static-environment-output
                                                     dynamic-environment-output)))
         (basic-blocks (compute-basic-blocks enter-instruction))
         (*dynamic-environment-of-basic-block* (make-hash-table :test #'eq))
         (*basic-blocks-in-dynamic-environment* (make-hash-table :test #'eq))
         (*basic-block-of-leader* (make-hash-table :test #'eq))
         (*tag-of-basic-block* (make-hash-table :test #'eq)))
    (loop for basic-block in basic-blocks
          for leader = (first (instructions basic-block))
          for dynamic-environment-location
            = (cleavir-ir:dynamic-environment-location leader)
          do (setf (gethash basic-block *dynamic-environment-of-basic-block*)
                   dynamic-environment-location)
             (push basic-block
                   (gethash dynamic-environment-location
                            *basic-blocks-in-dynamic-environment*))
             (setf (gethash leader *basic-block-of-leader*)
                   basic-block)
             (setf (gethash basic-block *tag-of-basic-block*)
                   (gensym)))
    (multiple-value-bind (required-parameters
                          optional-parameters
                          rest-parameter
                          key-parameters)
        (split-lambda-list lambda-list)
      `(lambda (,arguments-variable
                ,static-environment-variable
                ,dynamic-environment-variable)
         (declare (ignorable ,static-environment-variable
                             ,dynamic-environment-variable))
         (block ,(block-name context)
           (let (,@(mapcar #'cleavir-ir:name lexical-locations)
                 (,remaining-variable ,arguments-variable))
             ;; Check that enough arguments were passed.
             ,@(if (null required-parameters)
                   '()
                   `((when (< (length ,arguments-variable)
                              ,(length required-parameters))
                       (error "Not enough arguments"))))
             ;; Check that not too many arguments were passed
             ,@(if (and (null rest-parameter) (null key-parameters))
                   '()
                   `((when (> (length ,arguments-variable)
                              ,(+ (length required-parameters)
                                  (length optional-parameters)))
                       (error "Too many arguments"))))
             ,@(loop for required-parameter in required-parameters
                     collect `(setq ,(cleavir-ir:name required-parameter)
                                    (pop ,remaining-variable)))
             ,@(loop for optional-parameter in optional-parameters
                     collect `(setq ,(cleavir-ir:name (first optional-parameter))
                                    (pop ,remaining-variable))
                     collect `(setq ,(cleavir-ir:name (first optional-parameter))
                                    t))
             ,@(if (null rest-parameter)
                   '()
                   `((setq ,(cleavir-ir:name rest-parameter) ,remaining-variable)))
             ,@(loop for key-parameter in key-parameters
                     collect `(multiple-value-bind (indicator value tail)
                                  (get-properties ,remaining-variable
                                                  '(,(first key-parameter)))
                                (unless (null tail)
                                  (setf ,(cleavir-ir:name (second key-parameter))
                                        value)
                                  (setf ,(cleavir-ir:name (third key-parameter))
                                        t))))
             (tagbody (go ,(tag-of-basic-block (basic-block-of-leader successor)))
                ,@(loop with dynamic-environment-location
                          = (cleavir-ir:dynamic-environment-location successor)
                        with basic-blocks
                          = (basic-blocks-in-dynamic-environment
                             dynamic-environment-location)
                        for basic-block in basic-blocks
                        collect (tag-of-basic-block basic-block)
                        append (let ((*dynamic-environment-stack*
                                       (list dynamic-environment-location)))
                                 (translate-basic-block
                                  client
                                  basic-block
                                  context))))))))))

(defmethod translate (client (instruction cleavir-ir:enclose-instruction) context)
  (let ((name (cleavir-ir:name (first (cleavir-ir:outputs instruction))))
        (enter (cleavir-ir:code instruction)))
    `((setq ,name
            (enclose ,(gethash enter (function-names context))
                     ,@(mapcar #'cleavir-ir:name
                               (cleavir-ir:inputs instruction))))
      (closer-mop:set-funcallable-instance-function
       ,name
       (lambda (&rest args)
         (funcall ,(gethash enter (function-names context))
                  args
                  (funcall ,(static-env-function-var context) ,name)
                  *dynamic-environment*))))))
