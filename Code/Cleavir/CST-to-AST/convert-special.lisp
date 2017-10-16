(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special :around (operator cst environment system)
  (declare (ignore system))
  (check-special-form-syntax operator cst)
  (when (and *compile-time-too*
             *current-form-is-top-level-p*
             (not (member operator
                          '(progn locally macrolet symbol-macrolet eval-when))))
    (cleavir-env:eval (cst:raw cst) environment environment))
  (call-next-method))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    ((symbol (eql 'quote)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db s (quote-cst const-cst) cst
    (declare (ignore quote-cst))
    (convert-constant const-cst env system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    ((symbol (eql 'block)) cst env system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (block name-cst . body-cst) cst
          (declare (ignore block))
    (let ((name (cst:raw name-cst)))
      (unless (symbolp name)
        (error 'block-name-must-be-a-symbol
               :expr name
               :origin (cst:source name-cst)))
      (let* ((ast (cleavir-ast:make-block-ast nil :origin origin))
             (new-env (cleavir-env:add-block env name ast)))
        (setf (cleavir-ast:body-ast ast)
              (process-progn (convert-sequence body-cst new-env system)))
        ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special
    ((symbol (eql 'return-from)) cst env system)
  (cst:db origin (return-from-cst block-name-cst . rest-csts) cst
    (declare (ignore return-from-cst))
    (flet ((find-info (block-name)
             (cleavir-env:block-info env block-name)))
      (let ((info (find-info (cst:raw block-name-cst)))
            (value-cst (if (cst:null rest-csts)
                           (make-instance 'cst:atom-cst :raw nil)
                           (cst:first rest-csts))))
        (loop while (null info)
              do (restart-case (error 'cleavir-env:no-block-info
                                      :name (cst:raw block-name-cst)
                                      :origin (cst:source block-name-cst))
                   (substitute (new-block-name)
                     :report (lambda (stream)
                               (format stream "Substitute a different name."))
                     :interactive (lambda ()
                                    (format *query-io* "Enter new name: ")
                                    (list (read *query-io*)))
                     (setq info (find-info new-block-name)))
                   (recover ()
                     ;; In order to recover from the error, we ignore
                     ;; the RETURN-FROM form and only compile the return
                     ;; value form (or NIL if no return value form was
                     ;; present).
                     (return-from convert-special
                       (convert value-cst env system)))))
        (cleavir-ast:make-return-from-ast
         (cleavir-env:identity info)
         (convert value-cst env system)
         :origin origin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defmethod convert-special
    ((symbol (eql 'eval-when)) cst environment system)
  (with-preserved-toplevel-ness
    (cst:db s (eval-when-cst situations-cst . body-cst) cst
      (declare (ignore eval-when-cst))
      (let ((situations (cst:raw situations-cst)))
        (if (or (eq cleavir-generate-ast:*compiler* 'cl:compile)
                (eq cleavir-generate-ast:*compiler* 'cl:eval)
                (not *current-form-is-top-level-p*))
            (if (or (member :execute situations)
                    (member 'cl:eval situations))
                (process-progn
                 (convert-sequence body-cst environment system))
                (convert (cst:cst-from-expression nil) environment system))
            (cond ((or
                    ;; CT   LT   E    Mode
                    ;; Yes  Yes  ---  ---
                    (and (or (member :compile-toplevel situations)
                             (member 'cl:compile situations))
                         (or (member :load-toplevel situations)
                             (member 'cl:load situations)))
                    ;; CT   LT   E    Mode
                    ;; No   Yes  Yes  CTT
                    (and (not (or (member :compile-toplevel situations)
                                  (member 'compile situations)))
                         (or (member :load-toplevel situations)
                             (member 'load situations))
                         (or (member :execute situations)
                             (member 'eval situations))
                         *compile-time-too*))
                   (let ((*compile-time-too* t))
                     (process-progn
                      (convert-sequence body-cst environment system))))
                  ((or
                    ;; CT   LT   E    Mode
                    ;; No   Yes  Yes  NCT
                    (and (not (or (member :compile-toplevel situations)
                                  (member 'compile situations)))
                         (or (member :load-toplevel situations)
                             (member 'load situations))
                         (or (member :execute situations)
                             (member 'eval situations))
                         (not *compile-time-too*))
                    ;; CT   LT   E    Mode
                    ;; No   Yes  No   ---
                    (and (not (or (member :compile-toplevel situations)
                                  (member 'compile situations)))
                         (or (member :load-toplevel situations)
                             (member 'load situations))
                         (not (or (member :execute situations)
                                  (member 'eval situations)))))
                   (let ((*compile-time-too* nil))
                     (process-progn
                      (convert-sequence body-cst environment system))))
                  ((or
                    ;; CT   LT   E    Mode
                    ;; Yes  No   ---  ---
                    (and (or (member :compile-toplevel situations)
                             (member 'compile situations))
                         (not (or (member :load-toplevel situations)
                                  (member 'load situations))))
                    ;; CT   LT   E    Mode
                    ;; No   No   Yes  CTT
                    (and (not (or (member :compile-toplevel situations)
                                  (member 'compile situations)))
                         (not (or (member :load-toplevel situations)
                                  (member 'load situations)))
                         (or (member :execute situations)
                             (member 'eval situations))
                         *compile-time-too*))
                   (cleavir-env:eval `(progn ,@(cst:raw body-cst))
                                     environment environment)
                   (convert (cst:cst-from-expression nil)
                            environment system))
                  (t
                   (convert (cst:cst-from-expression nil)
                            environment system))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FLET.

;;; Given a function name represented as a CST, return the name of a
;;; block (also as a CST) that should be associated with the function
;;; with that name.
(defun block-name-from-function-name (function-name)
  (if (cst:atom function-name)
      function-name
      (cst:second function-name)))

;;; Take an environment and a CST representing a single local function
;;; definition.  Return a new environment which is like the one passed
;;; as an argument, except the it has been augmented by the name of
;;; the local function.
(defun augment-environment-from-fdef (environment definition-cst)
  (let ((name-cst (cst:first definition-cst)))
    (augment-environment-with-local-function-name name-cst environment)))

;;; Take an environment, a CST representing a list of function
;;; definitions, and return a new environment which is like the one
;;; passed as an argument, except that is has been augmented by the
;;; local function names in the list.
(defun augment-environment-from-fdefs (environment definitions-cst)
  (loop with result = environment
        for remaining = definitions-cst then (cst:rest remaining)
        until (cst:null remaining)
        do (let ((definition-cst (cst:first remaining)))
             (setf result
                   (augment-environment-from-fdef result definition-cst)))
        finally (return result)))

;;; Given an environment and the name of a function, return the
;;; LEXICAL-AST that will have the function with that name as a value.
;;; It is known that the environment contains an entry corresponding
;;; to the name given as an argument.
(defun function-lexical (environment name)
  (cleavir-env:identity (cleavir-env:function-info environment name)))

;;; Convert a local function definition.
(defun convert-local-function (definition-cst environment system)
  (check-cst-proper-list definition-cst
                         'local-function-definition-must-be-proper-list)
  (check-argument-count definition-cst 1 nil)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (unless (proper-function-name-p name-cst)
      (error 'function-name-must-be-proper-function-name
             :expr (cst:raw name-cst)
             :origin (cst:source name-cst)))
    (let ((block-name-cst (block-name-from-function-name name-cst)))
      (convert-code lambda-list-cst
                    body-cst
                    environment
                    system
                    block-name-cst))))

;;; Convert a CST representing a list of local function definitions.
(defun convert-local-functions (definitions-cst environment system)
  (check-cst-proper-list definitions-cst 'flet-functions-must-be-proper-list)
  (loop for remaining = definitions-cst
          then (cst:rest remaining)
        until (cst:null remaining)
        collect (let* ((def-cst (cst:first remaining))
                       (name-cst (cst:first def-cst))
                       (name (cst:raw name-cst))
                       (fun (convert-local-function
                             def-cst environment system)))
                  (cons name fun))))

;;; Compute and return a list of SETQ-ASTs that will assign the AST of
;;; each function in a list of function ASTs to its associated
;;; LEXICAL-AST.  FUNCTIONS is a list of CONS cells.  Each CONS cell
;;; has a function name in its CAR and an AST in its CDR.
(defun compute-function-init-asts (functions env)
  (loop for (name . fun-ast) in functions
        collect (cleavir-ast:make-setq-ast
                 (function-lexical env name)
                 fun-ast)))

;;; FIXME: add the processing of DYNAMIC-EXTENT declarations.
(defmethod convert-special ((symbol (eql 'flet)) cst env system)
  (cst:db origin (flet-cst definitions-cst . body-cst) cst
    (declare (ignore flet-cst))
    (multiple-value-bind (declaration-csts body-csts)
        (cst:separate-ordinary-body body-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations system declaration-csts))
             (defs (convert-local-functions definitions-cst env system))
             (new-env (augment-environment-from-fdefs env definitions-cst))
             (init-asts
               (compute-function-init-asts defs new-env))
             (final-env (augment-environment-with-declarations
                         new-env canonical-declaration-specifiers)))
        (process-progn
         (append init-asts
                 ;; So that flet with empty body works.
                 (list
                  (process-progn
                   (convert-sequence (cst:cstify body-csts)
                                     final-env
                                     system)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

(defmethod convert-special ((symbol (eql 'labels)) cst env system)
  (cst:db origin (labels-cst definitions-cst . body-cst) cst
    (declare (ignore labels-cst))
    (multiple-value-bind (declaration-csts body-csts)
        (cst:separate-ordinary-body body-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations system declaration-csts))
             (new-env (augment-environment-from-fdefs env definitions-cst))
             (defs (convert-local-functions definitions-cst new-env system))
             (init-asts
               (compute-function-init-asts defs new-env))
             (final-env (augment-environment-with-declarations
                         new-env canonical-declaration-specifiers)))
        (process-progn
         (append init-asts
                 ;; So that flet with empty body works.
                 (list
                  (process-progn
                   (convert-sequence (cst:cstify body-csts)
                                     final-env
                                     system)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.
;;;
;;; The TAGBODY special form always returns NIL.  We generate a PROGN
;;; with the TAGBODY-AST and a CONSTANT-AST in it, because the
;;; TAGBODY-AST (unlike hte TAGBODY special form) does not generate a
;;; value.

(defun tagp (item)
  (let ((raw (cst:raw item)))
    ;; go tags are symbols or integers, per CLHS glossary.
    (or (symbolp raw)
        (integerp raw))))

(defmethod convert-special
    ((symbol (eql 'tagbody)) cst env system)
  (cst:db origin (tagbody-cst . body-cst) cst
    (declare (ignore tagbody-cst))

    (let ((tag-asts
            (loop for rest = body-cst then (cst:rest rest)
                  until (cst:null rest)
                  when (tagp (cst:first rest))
                    collect (let ((tag-cst (cst:first rest)))
                              (cleavir-ast:make-tag-ast
                               (cst:raw tag-cst)
                               :origin (cst:source tag-cst)))))
          (new-env env))
      (loop for ast in tag-asts
            do (setf new-env (cleavir-env:add-tag
                              new-env (cleavir-ast:name ast) ast)))
      (let ((item-asts (loop for rest = body-cst then (cst:rest rest)
                             until (cst:null rest)
                             collect (let ((item-cst (cst:first rest)))
                                       (if (tagp item-cst)
                                           (pop tag-asts)
                                           (convert item-cst new-env system))))))
        (process-progn
         (list (cleavir-ast:make-tagbody-ast item-asts
                                             :origin origin)
               (convert-constant (cst:cst-from-expression nil) env system)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special
    ((symbol (eql 'go)) cst env system)
  (declare (ignore system))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db origin (go-cst tag-cst) cst
    (declare (ignore go-cst))
    (let ((info (tag-info env (cst:raw tag-cst))))
      (cleavir-ast:make-go-ast
       (cleavir-env:identity info)
       :origin origin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special ((symbol (eql 'if)) cst env system)
  (cst:db origin (if-cst test-cst then-cst . tail-cst) cst
    (declare (ignore if-cst))
    (let ((test-ast (convert test-cst env system))
          (true-ast (convert then-cst env system))
          (false-ast (if (cst:null tail-cst)
                         (convert-constant (cst:cst-from-expression nil)
                                           env system)
                         (cst:db s (else-cst) tail-cst
                           (convert else-cst env system)))))
      (if (typep test-ast 'cleavir-ast:boolean-ast-mixin)
          (cleavir-ast:make-if-ast
           test-ast
           true-ast
           false-ast
           :origin origin)
          (cleavir-ast:make-if-ast
           (cleavir-ast:make-eq-ast
            test-ast
            (convert-constant (cst:cst-from-expression nil) env system))
           false-ast
           true-ast
           :origin origin)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defmethod convert-special ((symbol (eql 'load-time-value)) cst env system)
  (declare (ignore system))
  (cst:db origin (load-time-value-cst form-cst . remaining-cst) cst
    (declare (ignore load-time-value-cst))
    (cleavir-ast:make-load-time-value-ast 
     (cst:raw form-cst)
     (if (cst:null remaining-cst)
         nil
         (cst:raw (cst:first remaining-cst)))
     :origin origin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, PROGN processes
;;; its subforms the same way as the form itself.

(defmethod convert-special ((symbol (eql 'progn)) cst env system)
  (with-preserved-toplevel-ness
    (cst:db origin (progn-cst . form-csts) cst
      (declare (ignore progn-cst))
      (process-progn (convert-sequence form-csts env system) origin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;;; its subforms the same way as the form itself.

;;; Given the CST for a MACROLET definition and an environment, return
;;; a macro expander (or macro function) for the definition.
(defun expander (definition-cst environment system)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (let ((lambda-expression (cst:parse-macro system
                                              name-cst
                                              lambda-list-cst
                                              `(progn ,@(cst:raw body-cst))
                                              environment)))
      (cleavir-env:eval lambda-expression
                        (cleavir-env:compile-time environment)
                        environment))))

(defmethod convert-special
    ((symbol (eql 'macrolet)) cst env system)
  (cst:db origin (macrolet-cst definition-csts . body-cst) cst
    (declare (ignore macrolet-cst))
    (let ((new-env env))
      (loop for remaining = definition-csts then (cst:rest remaining)
            until (cst:null remaining)
            do (let* ((definition-cst (cst:first remaining))
                      (name-cst (cst:first definition-cst))
                      (name (cst:raw name-cst))
                      (expander (expander definition-cst env system)))
                 (setf new-env
                       (cleavir-env:add-local-macro new-env name expander))))
      (with-preserved-toplevel-ness
        (convert (cst:cons (make-instance 'cst:atom-cst
                             :raw 'locally
                             :source origin)
                           body-cst)
                 new-env
                 system)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.
;;;

(defun convert-named-function (name-cst environment system)
  (let ((info (function-info environment (cst:raw name-cst))))
    (convert-function-reference name-cst info environment system)))

(defun convert-lambda-function (lambda-form-cst env system)
  (convert-code (cst:second lambda-form-cst)
                (cst:rest (cst:rest lambda-form-cst)) env system))

(defmethod convert-special ((symbol (eql 'function)) cst env system)
  (cst:db origin (function-cst name-cst) cst
    (declare (ignore function-cst))
    (let ((result (if (proper-function-name-p name-cst)
                      (convert-named-function name-cst env system)
                      (convert-lambda-function name-cst env system))))
      (reinitialize-instance result :origin origin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized to operators for which we do not provide a
;;; conversion method.

;;; Implementations should probably convert this in terms of
;;; CLEAVIR-PRIMOP:MULTIPLE-VALUE-CALL.
(defmethod convert-special
    ((symbol (eql 'multiple-value-call)) cst environment system)
  (declare (ignore environment system))
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    ((symbol (eql 'unwind-protect)) cst environment system)
  (declare (ignore environment system))
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    ((symbol (eql 'catch)) cst environment system)
  (declare (ignore environment system))
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    ((symbol (eql 'throw)) cst environment system)
  (declare (ignore environment system))
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    ((symbol (eql 'progv)) cst environment system)
  (declare (ignore environment system))
  (error 'no-default-method :operator symbol :expr cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;

(defmethod convert-special
    ((symbol (eql 'setq)) cst environment system)
  (let* ((csts (cst:listify (cst:rest cst)))
         (form-asts (loop for (var-cst form-cst) on csts by #'cddr
                          collect (convert-elementary-setq
                                   var-cst form-cst environment system))))
    (process-progn form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET.
;;;

(defmethod convert-special
    ((symbol (eql 'let)) cst environment system)
  (convert-let cst environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET*.
;;;

(defmethod convert-special
    ((symbol (eql 'let*)) cst environment system)
  (convert-let* cst environment system))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.
;;;

(defmethod convert-special
    ((symbol (eql 'locally)) cst environment system)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (locally-cst . body-forms-cst) cst
    (declare (ignore locally-cst))
    (multiple-value-bind (declaration-csts body-csts)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations system declaration-csts))
             (new-env (augment-environment-with-declarations
                       environment canonical-declaration-specifiers))
             (body-cst (cst:cstify body-csts)))
        (with-preserved-toplevel-ness
          (process-progn (convert-sequence body-cst new-env system)))))))
