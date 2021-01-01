(cl:in-package #:cleavir-cst-to-ast)

;;;; The general method for processing the lambda list is as follows:
;;;; We use recursion to process the remaining lambda list.  Before
;;;; the recursive call, we add the current parameters to the
;;;; environment that we pass to the recursive call.  The call returns
;;;; two values: the AST that was built and a modified lambda list,
;;;; containing the lambda list keywords, and the lexical variables
;;;; that were introduced.  The exception is that processing &AUX
;;;; entries does not return any lambda list, because it will always
;;;; be empty.
;;;;
;;;; The reason we do it this way is that, if a parameter turns out to
;;;; be a special variable, the entire rest of the lambda list and
;;;; function body must be executed with this variable bound.  The AST
;;;; configuration for expressing that situation is that the AST for
;;;; computing the rest of the lambda list and the body must be a
;;;; child of a BIND-AST that indicates that the special variable
;;;; should be bound.  This recursive method makes sure that the child
;;;; exists before the BIND-AST is created.
;;;;
;;;; The parameter DSPECS that is used in several functions is a list
;;;; of canonicalized declaration specifiers.  This list is used to
;;;; determine whether a variable is declared special.

;;;; Several functions in this system create a LEXICAL LAMBDA LIST.
;;;; Such a lambda list is similar to an ordinary lambda list.  It can
;;;; have required parameter, &OPTIONAL parameters, a &REST parameter,
;;;; and &KEY parameters.  It can not have any &AUX parameters,
;;;; though.  It can also have implementation-specific parameters.
;;;; The parameters are different from those of an ordinary lambda
;;;; list.  A required parameter is represented as a LEXICAL-AST
;;;; corresponding to the variable of the parameter in the original
;;;; lambda list.  The same thing is true for a &REST parameter.  An
;;;; &OPTIONAL parameter is represented as a list of two LEXICAL-ASTs.
;;;; The first AST of the list corresponds to the variable of the
;;;; parameter in the original lambda list, and the second AST
;;;; corresponds to a SUPPLIED-P parameter, whether the original
;;;; lambda list had such a parameter or not.  We generate ASTs that
;;;; test the SUPPLIED-P parameter and, if it is NIL, compute the
;;;; value of the initialization form if the corresponding parameter.
;;;; A &KEY parameter is represented as a list of three elements.  The
;;;; first element is the keyword to be used to determine whether this
;;;; parameter was given.  The remaining two elements play the same
;;;; role as those of an &OPTIONAL parameter.  We are not concerned
;;;; here with they way in which it is determined whether a particular
;;;; &KEY parameter was given or not.  This logic is determined for
;;;; each implementation.

;;; Process a single parameter.  This function first computes a new
;;; environment by augmenting ENVIRONMENT with information from
;;; PARAMETER.  Then it recursively processes the parameters in
;;; REMAINING-PARAMETERS-IN-GROUP and in REMAINING-PARAMETER-GROUPS in
;;; the augmented environment.  Finally, it returns two values.  The
;;; first return value is the AST resulting from the recursive
;;; processing, from the processing of PARAMETER and from BODY.  The
;;; second value is a lexical lambda list resulting from the
;;; processing of the parameters in PARAMETER, in
;;; REMAINING-PARAMETERS-IN-GROUP, and in REMAINING-PARAMETER-GROUPS.
(defgeneric process-parameter
    (client
     parameter
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs-of-parameter
     remaining-idspecs-in-group
     remaining-idspecs
     entry-of-parameter
     remaining-entries-in-group
     remaining-entries
     body
     environment))

;;; Process all the parameters in the list PARAMETERS-IN-GROUP.  This
;;; function first computes a new environment by augmenting
;;; ENVIRONMENT with information from the parameters in the list
;;; PARAMETERS-IN-GROUP.  Then it recursively processes the parameters
;;; in REMAINING-PARAMETER-GROUPS in the augmented environment.
;;; Finally, it returns two values.  The first return value is the AST
;;; resulting from the recursive processing and from the processing of
;;; the parameters in PARAMETERS-IN-GROUP and of BODY.  The second
;;; return value is a lexical lambda list corresponding to the
;;; parameters in PARAMETERS-IN-GROUP and in
;;; REMAINING-PARAMETER-GROUPS.
(defgeneric process-parameters-in-group
    (client
     parameters-in-group
     remaining-parameter-groups
     idspecs-in-group
     remaining-idspecs
     entries-in-group
     remaining-entries
     body
     environment))

;;; This function first computes a new environment by augmenting
;;; ENVIRONMENT with information from the parameters in
;;; PARAMETER-GROUP.  Then it recursively processes the parameters in
;;; REMAINING-PARAMETER-GROUPS in the augmented environment.  Finally,
;;; it returns two values.  The first return value is the AST
;;; resulting from the recursive processing and from the processing of
;;; the parameters in PARAMETER-GROUP and of BODY.  The second value
;;; is a lexical lambda list that corresponds to the parameters in
;;; PARAMETER-GROUP and REMAINING-PARAMETER-GROUPS.
(defgeneric process-parameter-group
    (client
     parameter-group
     remaining-parameter-groups
     idspecs-in-group
     remaining-idspecs
     entries-in-group
     remaining-entries
     body
     environment))

;;; Process all the parameters in the list of parameter groups
;;; PARAMETER-GROUPS.  This function returns two values.  The first
;;; return value is a list if parameter gruops that mirror the
;;; parameter groups in PARMETER-GROUPS.  The second return value is
;;; the AST resulting from the processing of those parameters and of
;;; BODY.
(defgeneric process-parameter-groups
    (client parameter-groups idspecs entries body environment))

(defmethod process-parameters-in-group
    (client
     (parameters-in-group null)
     remaining-parameter-groups
     idspecs-in-group
     remaining-idspecs
     entries-in-group
     remaining-entries
     body
     environment)
  (declare (ignore idspecs-in-group entries-in-group))
  (process-parameter-groups
   client
   remaining-parameter-groups
   remaining-idspecs
   remaining-entries
   body
   environment))

(defmethod process-parameters-in-group
    (client
     (parameters-in-group cons)
     remaining-parameter-groups
     idspecs-in-group
     remaining-idspecs
     entries-in-group
     remaining-entries
     body
     environment)
  (process-parameter
   client
   (car parameters-in-group)
   (cdr parameters-in-group)
   remaining-parameter-groups
   (car idspecs-in-group)
   (cdr idspecs-in-group)
   remaining-idspecs
   (car entries-in-group)
   (cdr entries-in-group)
   remaining-entries
   body
   environment))

(defgeneric new-environment-from-parameter
    (client parameter idspecs environment))

;;; This class is used to describe the body of a function.  It
;;; contains the declaration specifiers that apply to the body as a
;;; whole, and the body as a CST.
(defclass body ()
  ((%dspecs :initarg :dspecs :accessor dspecs)
   (%cst :initarg :cst :accessor body-cst)))

(defun make-body (dspecs cst)
  (make-instance 'body
    :dspecs dspecs
    :cst cst))

;;; Convert the body of a function.
(defun convert-body (client body environment)
  (let ((new-env (augment-environment-with-declarations
                  client environment (dspecs body))))
    (convert client (body-cst body) new-env)))

(defmethod process-parameter-groups
    (client (parameter-groups null) idspecs entries body environment)
  (values (convert-body client body environment) '()))

(defmethod process-parameter-groups
    (client (parameter-groups cons) idspecs entries body environment)
  (process-parameter-group
   client
   (car parameter-groups)
   (cdr parameter-groups)
   (car idspecs)
   (cdr idspecs)
   (car entries)
   (cdr entries)
   body
   environment))

(defmethod process-parameter-group
    (client
     (parameter-group cst:multi-parameter-group-mixin)
     remaining-parameter-groups
     idspecs-in-group
     remaining-idspecs
     entries-in-group
     remaining-entries
     body
     environment)
  (process-parameters-in-group
   client
   (cst:parameters parameter-group)
   remaining-parameter-groups
   idspecs-in-group
   remaining-idspecs
   entries-in-group
   remaining-entries
   body
   environment))

(defmethod process-parameter-group
    (client
     (parameter-group cst:ordinary-rest-parameter-group)
     remaining-parameter-groups
     idspecs-in-group
     remaining-idspecs
     entries-in-group
     remaining-entries
     body
     environment)
  (process-parameter
   client
   (cst:parameter parameter-group)
   '()
   remaining-parameter-groups
   (car idspecs-in-group)
   '()
   remaining-idspecs
   (car entries-in-group)
   '()
   remaining-entries
   body
   environment))

(defmethod new-environment-from-parameter
    (client (parameter cst:simple-variable) idspecs environment)
  (augment-environment-with-variable
   client (cst:name parameter) idspecs environment environment))

(defmethod new-environment-from-parameter
    (client (parameter cst:ordinary-key-parameter) idspecs environment)
  (augment-environment-with-parameter
   client (cst:name parameter) (cst:supplied-p parameter) idspecs environment))

(defmethod new-environment-from-parameter
    (client (parameter cst:ordinary-optional-parameter) idspecs environment)
  (augment-environment-with-parameter
   client (cst:name parameter) (cst:supplied-p parameter) idspecs environment))

(defmethod new-environment-from-parameter
    (client (parameter cst:aux-parameter) idspecs environment)
  (augment-environment-with-variable
   client (cst:name parameter) idspecs environment environment))

(defmethod process-parameter
    (client
     (parameter cst:simple-variable)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     remaining-idspecs-in-group
     remaining-idspecs
     lexical-ast
     remaining-entries-in-group
     remaining-entries
     body
     environment)
  (let ((new-env (new-environment-from-parameter
                  client
                  parameter
                  idspecs
                  environment)))
    (set-or-bind-variable
     client
     (cst:name parameter) lexical-ast
     (lambda ()
       (process-parameters-in-group
        client
        remaining-parameters-in-group
        remaining-parameter-groups
        remaining-idspecs-in-group
        remaining-idspecs
        remaining-entries-in-group
        remaining-entries
        body
        new-env))
     new-env)))

(defmethod process-parameter
    (client
     (parameter cst:ordinary-optional-parameter)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     remaining-idspecs-in-group
     remaining-idspecs
     entry
     remaining-entries-in-group
     remaining-entries
     body
     environment)
  (let* ((var-cst (cst:name parameter))
         (init-form-cst (if (null (cst:form parameter))
                            (make-atom-cst nil (cst:source var-cst))
                            (cst:form parameter)))
         (supplied-p-cst (cst:supplied-p parameter))
         (new-env (new-environment-from-parameter
                   client
                   parameter
                   idspecs
                   environment))
         (init-ast (convert client
                            init-form-cst
                            environment)))
    (process-init-parameter
     client
     var-cst (first entry)
     supplied-p-cst (second entry)
     init-ast new-env
     (lambda ()
       (process-parameters-in-group
        client
        remaining-parameters-in-group
        remaining-parameter-groups
        remaining-idspecs-in-group
        remaining-idspecs
        remaining-entries-in-group
        remaining-entries
        body
        new-env)))))

(defmethod process-parameter
    (client
     (parameter cst:ordinary-key-parameter)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     remaining-idspecs-in-group
     remaining-idspecs
     entry
     remaining-entries-in-group
     remaining-entries
     body
     environment)
  (let* ((var-cst (cst:name parameter))
         (init-form-cst (if (null (cst:form parameter))
                            (make-atom-cst nil (cst:source var-cst))
                            (cst:form parameter)))
         (supplied-p-cst (cst:supplied-p parameter))
         (new-env (new-environment-from-parameter
                   client
                   parameter
                   idspecs
                   environment))
         (init-ast (convert client
                            init-form-cst
                            environment)))
    (process-init-parameter
     client
     var-cst (first entry)
     supplied-p-cst (second entry)
     init-ast new-env
     (lambda ()
       (process-parameters-in-group
        client
        remaining-parameters-in-group
        remaining-parameter-groups
        remaining-idspecs-in-group
        remaining-idspecs
        remaining-entries-in-group
        remaining-entries
        body
        new-env)))))

(defmethod process-parameter
    (client
     (parameter cst:aux-parameter)
     remaining-parameters-in-group
     remaining-parameter-groups
     idspecs
     remaining-idspecs-in-group
     remaining-idspecs
     entry
     remaining-entries-in-group
     remaining-entries
     body
     environment)
  (declare (ignore entry))
  (let* ((var-cst (cst:name parameter))
         (init-form-cst (cst:form parameter))
         (new-env (new-environment-from-parameter
                   client
                   parameter
                   idspecs
                   environment))
         (init-ast (convert
                    client
                    init-form-cst
                    environment)))
    (set-or-bind-variable
     client
     var-cst init-ast
     (lambda ()
       (process-parameters-in-group
        client
        remaining-parameters-in-group
        remaining-parameter-groups
        remaining-idspecs-in-group
        remaining-idspecs
        remaining-entries-in-group
        remaining-entries
        body
        new-env))
     new-env)))

(defun itemize-declaration-specifiers-by-parameter-group
    (items-by-parameter-group canonical-dspecs)
  (if (null items-by-parameter-group)
      (values '() canonical-dspecs)
      (multiple-value-bind (itemized-dspecs remaining-dspecs)
           (itemize-declaration-specifiers (first items-by-parameter-group)
                                           canonical-dspecs)
        (multiple-value-bind (more-itemized-dspecs more-remaining-dspecs)
            (itemize-declaration-specifiers-by-parameter-group
             (rest items-by-parameter-group)
             remaining-dspecs)
          (values (cons itemized-dspecs more-itemized-dspecs)
                  more-remaining-dspecs)))))

(defun cst-for-body (forms-cst block-name-cst)
  (if (null block-name-cst)
      (cst:cons (make-atom-cst 'progn *origin*) forms-cst
                :source *origin*)
      (cst:cons (make-atom-cst 'block *origin*)
                (cst:cons block-name-cst forms-cst)
                :source *origin*)))

(defmethod convert-code
    (client lambda-list-cst body-cst environment &key (block-name-cst nil))
  (let ((parsed-lambda-list
          (cst:parse-ordinary-lambda-list client lambda-list-cst :error-p nil)))
    (when (null parsed-lambda-list)
      (error 'malformed-lambda-list :cst lambda-list-cst))
    (multiple-value-bind (declaration-csts documentation forms-cst)
        (cst:separate-function-body body-cst)
      ;; FIXME: Handle documentation
      (declare (ignore documentation))
      (let* ((declaration-specifiers
               (loop for declaration-cst in declaration-csts
                     append (cdr (cst:listify declaration-cst))))
             (canonicalized-dspecs
               (cst:canonicalize-declaration-specifiers
                client
                '()
                declaration-specifiers)))
        (multiple-value-bind (idspecs rdspecs)
            (itemize-declaration-specifiers-by-parameter-group
             (itemize-lambda-list parsed-lambda-list)
             canonicalized-dspecs)
          (multiple-value-bind (lexical-lambda-list entries)
              (lambda-list-from-parameter-groups (cst:children parsed-lambda-list))
            (let ((ast
                    (process-parameter-groups
                     client
                     (cst:children parsed-lambda-list)
                     idspecs
                     entries
                     (make-body rdspecs (cst-for-body forms-cst block-name-cst))
                     environment)))
              (cleavir-ast:make-ast 'cleavir-ast:function-ast
                :body-ast ast
                :lambda-list lexical-lambda-list))))))))
