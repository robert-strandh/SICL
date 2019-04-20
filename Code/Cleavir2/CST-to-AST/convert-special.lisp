(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special :around (client operator cst lexical-environment dynamic-environment-ast)
  (declare (ignore client))
  (when (and *compile-time-too*
             *current-form-is-top-level-p*
             (not (member operator
                          '(progn locally macrolet symbol-macrolet eval-when))))
    (cleavir-env:eval (cst:raw cst) lexical-environment lexical-environment))
  (restart-case (call-next-method)
    (recover ()
      :report "Recover by replacing form by a call to ERROR."
      (cst:cst-from-expression
       '(error 'run-time-program-error
         :expr (cst:raw cst)
         :origin (cst:source cst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special
    (client (symbol (eql 'quote)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db s (quote-cst const-cst) cst
    (declare (ignore quote-cst))
    (convert-constant client const-cst lexical-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special
    (client (symbol (eql 'block)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (block name-cst . body-cst) cst
    (declare (ignore block))
    (let ((name (cst:raw name-cst)))
      (unless (symbolp name)
        (error 'block-name-must-be-a-symbol
               :expr name
               :origin (cst:source name-cst)))
      (let* ((new-dynenv (make-instance 'cleavir-ast:lexical-ast
                           :name '#:block-dynamic-environment))
             (ast (make-instance 'cleavir-ast:block-ast
                    :dynamic-environment-out new-dynenv))
             (new-env (cleavir-env:add-block lexical-environment name ast))
             (cleavir-ast:*dynamic-environment* new-dynenv))
        (setf (cleavir-ast:body-ast ast)
              (process-progn (convert-sequence client body-cst new-env)))
        ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special
    (client (symbol (eql 'return-from)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (cst:db origin (return-from-cst block-name-cst . rest-csts) cst
    (declare (ignore return-from-cst))
    (let ((block-name (cst:raw block-name-cst)))
      (unless (symbolp block-name)
        (error 'block-name-must-be-a-symbol
               :expr block-name
               :origin (cst:source block-name-cst)))
      (flet ((find-info (block-name)
               (cleavir-env:block-info lexical-environment block-name)))
        (let ((info (find-info block-name))
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
                         (convert client value-cst lexical-environment dynamic-environment-ast)))))
          (make-instance 'cleavir-ast:return-from-ast
           :block-ast (cleavir-env:identity info)
           :form-ast (convert client value-cst lexical-environment dynamic-environment-ast)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defun check-eval-when-syntax (cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let ((situations-cst (cst:second cst)))
    (unless (cst:proper-list-p situations-cst)
      (error 'situations-must-be-proper-list
             :expr (cst:raw situations-cst)
             :origin (cst:source situations-cst)))
    ;; Check each situation
    (loop for remaining = situations-cst then (cst:rest remaining)
          until (cst:null remaining)
          do (let* ((situation-cst (cst:first remaining))
                    (situation (cst:raw situation-cst)))
               (unless (and (symbolp situation)
                            (member situation
                                    '(:compile-toplevel :load-toplevel :execute
                                      compile load eval)))
                 (error 'invalid-eval-when-situation
                        :expr situation
                        :origin (cst:source situation-cst)))))))

(defparameter *use-file-compilation-sematics-p* nil)

(defmethod convert-special
    (client (symbol (eql 'eval-when)) cst lexical-environment dynamic-environment-ast)
  (check-eval-when-syntax cst)
  (with-preserved-toplevel-ness
    (cst:db s (eval-when-cst situations-cst . body-cst) cst
      (declare (ignore eval-when-cst))
      (let ((situations (cst:raw situations-cst)))
        (if (or (not *use-file-compilation-sematics-p*)
                (not *current-form-is-top-level-p*))
            (if (or (member :execute situations)
                    (member 'cl:eval situations))
                (process-progn
                 (convert-sequence client body-cst lexical-environment))
                (convert client (cst:cst-from-expression nil) lexical-environment dynamic-environment-ast))
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
                      (convert-sequence client body-cst lexical-environment))))
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
                      (convert-sequence client body-cst lexical-environment))))
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
                                     lexical-environment lexical-environment)
                   (convert client
                            (cst:cst-from-expression nil)
                            lexical-environment
                            dynamic-environment-ast))
                  (t
                   (convert client
                            (cst:cst-from-expression nil)
                            lexical-environment
                             dynamic-environment-ast))))))))

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
(defun augment-environment-from-fdef (lexical-environment definition-cst)
  (let ((name-cst (cst:first definition-cst)))
    (augment-environment-with-local-function-name name-cst lexical-environment)))

;;; Take an environment, a CST representing a list of function
;;; definitions, and return a new environment which is like the one
;;; passed as an argument, except that is has been augmented by the
;;; local function names in the list.
(defun augment-environment-from-fdefs (lexical-environment definitions-cst)
  (loop with result = lexical-environment
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
(defun function-lexical (lexical-environment name)
  (cleavir-env:identity (cleavir-env:function-info lexical-environment name)))

;;; Convert a local function definition.
(defun convert-local-function (client definition-cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list definition-cst
                         'local-function-definition-must-be-proper-list)
  (check-argument-count definition-cst 1 nil)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (unless (proper-function-name-p name-cst)
      (error 'function-name-must-be-proper-function-name
             :expr (cst:raw name-cst)
             :origin (cst:source name-cst)))
    (let ((block-name-cst (block-name-from-function-name name-cst)))
      (convert-code client lambda-list-cst
                    body-cst
                    lexical-environment
                    dynamic-environment-ast
                    :block-name-cst block-name-cst))))

;;; Convert a CST representing a list of local function definitions.
(defun convert-local-functions (client definitions-cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list definitions-cst 'flet-functions-must-be-proper-list)
  (loop for remaining = definitions-cst
          then (cst:rest remaining)
        until (cst:null remaining)
        collect (let* ((def-cst (cst:first remaining))
                       (name-cst (cst:first def-cst))
                       (name (cst:raw name-cst))
                       (fun (convert-local-function
                             client def-cst lexical-environment dynamic-environment-ast)))
                  (cons name fun))))

;;; Compute and return a list of SETQ-ASTs that will assign the AST of
;;; each function in a list of function ASTs to its associated
;;; LEXICAL-AST.  FUNCTIONS is a list of CONS cells.  Each CONS cell
;;; has a function name in its CAR and an AST in its CDR.
(defun compute-function-init-asts (functions lexical-environment)
  (loop for (name . fun-ast) in functions
        collect (make-instance 'cleavir-ast:setq-ast
                  :lhs-ast (function-lexical lexical-environment name)
                  :value-ast fun-ast)))

;;; FIXME: add the processing of DYNAMIC-EXTENT declarations.
(defmethod convert-special (client (symbol (eql 'flet)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (flet-cst definitions-cst . body-cst) cst
    (declare (ignore flet-cst))
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations client declaration-csts))
             (defs (convert-local-functions client definitions-cst lexical-environment dynamic-environment-ast))
             (new-env (augment-environment-from-fdefs lexical-environment definitions-cst))
             (init-asts
               (compute-function-init-asts defs new-env))
             (final-env (augment-environment-with-declarations
                         new-env canonical-declaration-specifiers)))
        (process-progn
         (append init-asts
                 ;; So that flet with empty body works.
                 (list
                  (process-progn
                   (convert-sequence client forms-cst final-env)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

(defmethod convert-special (client (symbol (eql 'labels)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (labels-cst definitions-cst . body-cst) cst
    (declare (ignore labels-cst))
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations client declaration-csts))
             (new-env (augment-environment-from-fdefs lexical-environment definitions-cst))
             (defs (convert-local-functions client definitions-cst new-env dynamic-environment-ast))
             (init-asts
               (compute-function-init-asts defs new-env))
             (final-env (augment-environment-with-declarations
                         new-env canonical-declaration-specifiers)))
        (process-progn
         (append init-asts
                 ;; So that flet with empty body works.
                 (list
                  (process-progn
                   (convert-sequence client forms-cst final-env)))))))))

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
    (client (symbol (eql 'tagbody)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (tagbody-cst . body-cst) cst
    (declare (ignore tagbody-cst))
    (let ((tag-asts
            (loop for rest = body-cst then (cst:rest rest)
                  until (cst:null rest)
                  when (tagp (cst:first rest))
                    collect (let ((tag-cst (cst:first rest)))
                              (make-instance 'cleavir-ast:tag-ast
                                :name (cst:raw tag-cst)))))
          (new-dynenv (make-instance 'cleavir-ast:lexical-ast
                           :name '#:tagbody-dynamic-environment))
          (new-env lexical-environment))
      (loop with cleavir-ast:*dynamic-environment* = new-dynenv
            for ast in tag-asts
            do (setf new-env (cleavir-env:add-tag
                              new-env (cleavir-ast:name ast) ast)))
      (let ((item-asts (loop with cleavir-ast:*dynamic-environment* = new-dynenv
                             for rest = body-cst then (cst:rest rest)
                             until (cst:null rest)
                             collect (let ((item-cst (cst:first rest)))
                                       (if (tagp item-cst)
                                           (pop tag-asts)
                                           (convert client item-cst new-env dynamic-environment-ast))))))
        (process-progn
         (list (make-instance 'cleavir-ast:tagbody-ast
                 :item-asts item-asts
                 :dynamic-environment-out new-dynenv)
               (convert-constant client (cst:cst-from-expression nil) lexical-environment)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special
    (client (symbol (eql 'go)) cst lexical-environment dynamic-environment-ast)
  (declare (ignore client))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db origin (go-cst tag-cst) cst
    (declare (ignore go-cst))
    (let ((info (tag-info lexical-environment (cst:raw tag-cst))))
      (make-instance 'cleavir-ast:go-ast
        :tag-ast (cleavir-env:identity info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special (client (symbol (eql 'if)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 3)
  (cst:db origin (if-cst test-cst then-cst . tail-cst) cst
    (declare (ignore if-cst))
    (let ((test-ast (convert client test-cst lexical-environment dynamic-environment-ast))
          (true-ast (convert client then-cst lexical-environment dynamic-environment-ast))
          (false-ast (if (cst:null tail-cst)
                         (convert-constant client
                                           (cst:cst-from-expression nil)
                                            lexical-environment)
                         (cst:db s (else-cst) tail-cst
                           (convert client else-cst lexical-environment dynamic-environment-ast)))))
      (if (typep test-ast 'cleavir-ast:boolean-ast-mixin)
          (make-instance 'cleavir-ast:if-ast
           :test-ast test-ast
           :then-ast true-ast
           :else-ast false-ast)
          (make-instance 'cleavir-ast:if-ast
           :test-ast
           (make-instance 'cleavir-ast:eq-ast
            :arg1-ast test-ast
            :arg2-ast (convert-constant client (cst:cst-from-expression nil) lexical-environment))
           :then-ast false-ast
           :else-ast true-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defmethod convert-special (client (symbol (eql 'load-time-value)) cst lexical-environment dynamic-environment-ast)
  (declare (ignore client))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (cst:db origin (load-time-value-cst form-cst . remaining-cst) cst
    (declare (ignore load-time-value-cst))
    (make-instance 'cleavir-ast:load-time-value-ast
      :form (cst:raw form-cst)
      :read-only-p
      (if (cst:null remaining-cst)
          nil
          (let ((read-only-p (cst:raw (cst:first remaining-cst))))
            (if (member read-only-p '(nil t))
                read-only-p
                ;; The HyperSpec specifically requires a "boolean"
                ;; and not a "generalized boolean".
                (error 'read-only-p-must-be-boolean
                       :expr read-only-p
                       :origin (cst:source (cst:first remaining-cst)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, PROGN processes
;;; its subforms the same way as the form itself.

(defmethod convert-special (client (symbol (eql 'progn)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (with-preserved-toplevel-ness
    (cst:db origin (progn-cst . form-csts) cst
      (declare (ignore progn-cst))
      (process-progn (convert-sequence client form-csts lexical-environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;;; its subforms the same way as the form itself.

;;; Given the CST for a MACROLET definition and an environment, return
;;; a macro expander (or macro function) for the definition.
;;; FIXME: check syntax.
(defun expander (client definition-cst lexical-environment)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (let ((lambda-expression (cst:parse-macro client
                                              name-cst
                                              lambda-list-cst
                                              `(progn ,@(cst:raw body-cst))
                                              lexical-environment)))
      (cleavir-env:eval lambda-expression
                        (cleavir-env:compile-time lexical-environment)
                        lexical-environment))))

(defmethod convert-special
    (client (symbol (eql 'macrolet)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore macrolet-cst))
    (unless (cst:proper-list-p definitions-cst)
      (error 'macrolet-definitions-must-be-proper-list
             :expr (cst:raw definitions-cst)
             :origin (cst:source definitions-cst)))
    (let ((new-env lexical-environment))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (let* ((definition-cst (cst:first remaining))
                      (name-cst (cst:first definition-cst))
                      (name (cst:raw name-cst))
                      (expander (expander client definition-cst lexical-environment)))
                 (setf new-env
                       (cleavir-env:add-local-macro new-env name expander))))
      (with-preserved-toplevel-ness
        (convert client
                 (cst:cons (make-instance 'cst:atom-cst
                             :raw 'locally
                             :source origin)
                           body-cst)
                 new-env
                 dynamic-environment-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-special
    (client (head (eql 'symbol-macrolet)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (symbol-macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore symbol-macrolet-cst))
    (let ((new-env lexical-environment))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (cst:db definition-origin (name-cst expansion-cst)
                   (cst:first remaining)
                 (let ((name (cst:raw name-cst))
                       (expansion (cst:raw expansion-cst)))
                   (setf new-env
                         (cleavir-env:add-local-symbol-macro
                          new-env name expansion)))))
      (with-preserved-toplevel-ness
        (convert client
                 (cst:cons (cst:cst-from-expression 'locally)
                           body-cst
                           :source origin)
                 new-env
                 dynamic-environment-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.
;;;

(defun convert-named-function (client name-cst lexical-environment dynamic-environment-ast)
  (let ((info (function-info lexical-environment (cst:raw name-cst))))
    (convert-function-reference client name-cst info lexical-environment dynamic-environment-ast)))

(defun convert-lambda-function (client lambda-form-cst lexical-environment dynamic-environment-ast)
  (convert-code client
                (cst:second lambda-form-cst)
                (cst:rest (cst:rest lambda-form-cst))
                lexical-environment
                dynamic-environment-ast))

(defun check-function-syntax (cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (let ((function-name-cst (cst:second cst)))
    (cond ((proper-function-name-p function-name-cst)
           nil)
          ((cst:consp function-name-cst)
           (unless (eq (cst:raw (cst:first function-name-cst)) 'lambda)
             (error 'function-argument-must-be-function-name-or-lambda-expression
                    :expr (cst:raw function-name-cst)
                    :origin (cst:source function-name-cst)))
           (unless (cst:proper-list-p function-name-cst)
             (error 'lambda-must-be-proper-list
                    :expr (cst:raw function-name-cst)
                    :origin (cst:source function-name-cst))))
          (t
           (error 'function-argument-must-be-function-name-or-lambda-expression
                  :expr (cst:raw function-name-cst)
                  :origin (cst:source function-name-cst))))))

(defmethod convert-special (client (symbol (eql 'function)) cst lexical-environment dynamic-environment-ast)
  (check-function-syntax cst)
  (cst:db origin (function-cst name-cst) cst
    (declare (ignore function-cst))
    (let ((result (if (proper-function-name-p name-cst)
                      (convert-named-function client name-cst lexical-environment dynamic-environment-ast)
                      (convert-lambda-function client name-cst lexical-environment dynamic-environment-ast))))
      (reinitialize-instance result :origin origin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

;;; Given a type in values position (i.e. argument to THE or return
;;; value of a function type), return three values: a list of REQUIRED
;;; types, a list of OPTIONAL types, and a REST type, and whether a
;;; REST was present.  e.g. (values integer &optional cons &rest
;;; symbol) => (INTEGER), (CONS), SYMBOL, T

(defun parse-values-type (values-type)
  (let ((original-values-type values-type))
    (cond
      ((and (consp values-type) (eq (car values-type) 'values))
       (setf values-type (rest values-type))
       (values
        (loop while (and (consp values-type)
                         (not (find (car values-type)
                                    '(&optional &rest))))
              collect (pop values-type))
        (when (eq (car values-type) '&optional)
          (pop values-type)
          (loop while (and (consp values-type)
                           (not (eq (car values-type) '&rest)))
                collect (pop values-type)))
        (when (eq (car values-type) '&rest)
          (unless (null (cddr values-type))
            (error 'values-&rest-syntax :expr original-values-type))
          (second values-type))
        (eq (car values-type) '&rest)))
      (t (values (list values-type) nil nil nil)))))

;;; Given results from parse-values-type, insert "fudginess" for
;;; CL:THE semantics. The fudginess is in the number of values: the
;;; form in a THE can return a different number of values than are
;;; specified in the type, but we would like to represent types more
;;; exactly for analysis.
(defun fudge-values-type (req opt rest restp)
  ;; to allow too many values, just force a &rest iff unspecified
  (unless restp (setf rest 't))
  ;; too few values is more difficult
  ;; any values not returned by the THE form are considered NIL,
  ;; so if a "required" type includes NIL it could also be no-value
  ;; and on the flipside, if a type does NOT include NIL the form
  ;; must actually return a value for it.
  ;; Therefore, we can just make all types on the end of REQ that
  ;; do not include NIL optional.
  ;; A further complication is that, since this is compile-time,
  ;; some types may not be defined enough for TYPEP to work
  ;; (e.g. SATISFIES with an undefined function) as mentioned in
  ;; CLHS deftype. Therefore we use SUBTYPEP instead of TYPEP.
  ;; We have, also according to that page, the opportunity to
  ;; signal a warning and ignore the declaration instead, but
  ;; that requires more intimacy with the implementation type
  ;; system than we presently have.
  (let* ((lastpos (position-if-not
                   (lambda (type)
                     (multiple-value-bind (subtype-p valid-p)
                         (subtypep 'null type)
                       (or subtype-p (not valid-p))))
                   req
                   :from-end t))
         ;; if we found something, we need the next position
         ;; for the next bit. if we didn't, zero
         ;; E.g. (values integer list) => lastpos = 1
         (lastpos (if lastpos (1+ lastpos) 0))
         ;; and new-opt = (list)
         (new-opt (nthcdr lastpos req))
         ;; and new-req = (integer)
         (new-req (ldiff req new-opt)))
    (setf req new-req
          opt (append new-opt opt)))
  (values req opt rest))

;;; the-values-components: compose the above two functions.
(defun the-values-components (values-type)
  (multiple-value-call #'fudge-values-type
    (parse-values-type values-type)))

(defmethod convert-special
    (client (symbol (eql 'the)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (the-cst value-type-cst form-cst) cst
    (declare (ignore the-cst))
    (multiple-value-bind (req opt rest)
        (the-values-components (cst:raw value-type-cst))
      ;; We don't bother collapsing THE forms for user code.
      (make-instance 'cleavir-ast:the-ast
        :form-ast (convert client form-cst lexical-environment dynamic-environment-ast)
        :required req
        :optional opt
        :rest rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1.

(defmethod convert-special
    (client (symbol (eql 'multiple-value-prog1)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (multiple-value-prog1-cst first-cst . rest-cst) cst
    (declare (ignore multiple-value-prog1-cst))
    (make-instance 'cleavir-ast:multiple-value-prog1-ast
     :first-form-ast (convert client first-cst lexical-environment dynamic-environment-ast)
     :form-asts (convert-sequence client rest-cst lexical-environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized to operators for which we do not provide a
;;; conversion method.

;;; Implementations should probably convert this in terms of
;;; CLEAVIR-PRIMOP:MULTIPLE-VALUE-CALL.
(defmethod convert-special
    (client (symbol (eql 'multiple-value-call)) cst lexical-environment dynamic-environment-ast)
  (declare (ignore client lexical-environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    (client (symbol (eql 'unwind-protect)) cst lexical-environment dynamic-environment-ast)
  (declare (ignore client lexical-environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    (client (symbol (eql 'catch)) cst lexical-environment dynamic-environment-ast)
  (declare (ignore client lexical-environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    (client (symbol (eql 'throw)) cst lexical-environment dynamic-environment-ast)
  (declare (ignore client lexical-environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (error 'no-default-method :operator symbol :expr cst))

(defmethod convert-special
    (client (symbol (eql 'progv)) cst lexical-environment dynamic-environment-ast)
  (declare (ignore client lexical-environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 nil)
  (error 'no-default-method :operator symbol :expr cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;

(defmethod convert-special
    (client (symbol (eql 'setq)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (unless (oddp (length (cst:raw cst)))
    (error 'setq-must-have-even-number-of-arguments
           :expr cst
           :origin (cst:source cst)))
  (let* ((csts (cst:listify (cst:rest cst)))
         (form-asts (loop for (variable-cst form-cst) on csts by #'cddr
                          for variable = (cst:raw variable-cst)
                          unless (symbolp variable)
                            do (error 'setq-var-must-be-symbol
                                      :expr variable
                                      :origin (cst:source variable-cst))
                          collect (convert-elementary-setq
                                   client variable-cst form-cst lexical-environment))))
    (process-progn form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET.
;;;

(defmethod convert-special
    (client (symbol (eql 'let)) cst lexical-environment dynamic-environment-ast)
  (convert-let client cst lexical-environment dynamic-environment-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET*.
;;;

(defmethod convert-special
    (client (symbol (eql 'let*)) cst lexical-environment dynamic-environment-ast)
  (convert-let* client cst lexical-environment dynamic-environment-ast))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.
;;;

(defmethod convert-special
    (client (symbol (eql 'locally)) cst lexical-environment dynamic-environment-ast)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (locally-cst . body-forms-cst) cst
    (declare (ignore locally-cst))
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
               (cst:canonicalize-declarations client declaration-csts))
             (new-env (augment-environment-with-declarations
                       lexical-environment canonical-declaration-specifiers)))
        (with-preserved-toplevel-ness
          (process-progn (convert-sequence client forms-cst new-env)))))))
