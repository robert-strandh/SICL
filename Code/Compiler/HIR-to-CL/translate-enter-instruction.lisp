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

(defun translate-enter-instruction (enter-instruction context)
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
                                                     dynamic-environment-output))))
    (multiple-value-bind (required-parameters
                          optional-parameters
                          rest-parameter
                          key-parameters)
        (split-lambda-list lambda-list)
      `(lambda (,arguments-variable
                ,static-environment-variable
                ,dynamic-environment-variable)
         (declare (ignorable ,dynamic-environment-variable))
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
             (tagbody ,@(translate successor context))))))))
