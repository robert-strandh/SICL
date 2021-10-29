(cl:in-package #:cleavir-cst-to-ast)

(defmethod convert-special :before (client operator cst environment)
  (when (and *compile-time-too*
             *current-form-is-top-level-p*
             (not (member operator
                          '(progn locally macrolet symbol-macrolet eval-when))))
    (cst-eval client cst environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting QUOTE.

(defmethod convert-special (client (symbol (eql 'quote)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db s (quote-cst const-cst) cst
    (declare (ignore quote-cst))
    ;; Frequently, the constant itself appears unquoted in source
    ;; code, such as when it is the argument to a macro that then
    ;; quotes it.  In that case, the entire form will not have any
    ;; source information, but the constant itself will.  In this
    ;; situation, we take the source information from the constant
    ;; itself so that we have some idea of the origin it.
    (let ((*origin* (if (null (cst:source cst))
                        (cst:source const-cst)
                        (cst:source cst))))
      (convert-constant client
                        const-cst
                        environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting BLOCK.

(defmethod convert-special (client (symbol (eql 'block)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (block name-cst . body-cst) cst
    (declare (ignore block))
    (let ((name (cst:raw name-cst)))
      (unless (symbolp name)
        (error 'block-name-must-be-a-symbol :cst name-cst))
      (let* ((ast (cleavir-ast:make-ast 'cleavir-ast:block-ast))
             (new-environment
               (trucler:add-block client environment name ast)))
        (setf (cleavir-ast:body-ast ast)
              (process-progn (convert-sequence client
                                               body-cst
                                               new-environment)))
        ast))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting RETURN-FROM.

(defmethod convert-special (client (symbol (eql 'return-from)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (cst:db origin (return-from-cst block-name-cst . rest-csts) cst
    (declare (ignore return-from-cst))
    (let ((block-name (cst:raw block-name-cst)))
      (unless (symbolp block-name)
        (error 'block-name-must-be-a-symbol :cst block-name-cst))
      (flet ((find-description (block-name)
               (trucler:describe-block client environment block-name)))
        (let ((info (find-description block-name))
              (value-cst (if (cst:null rest-csts)
                             (make-atom-cst nil origin)
                             (cst:first rest-csts))))
          (loop while (null info)
                do (restart-case (error 'trucler:no-block-description :cst cst)
                     (substitute (new-block-name)
                       :report (lambda (stream)
                                 (format stream "Substitute a different name."))
                       :interactive (lambda ()
                                      (format *query-io* "Enter new name: ")
                                      (list (read *query-io*)))
                       (setq info (find-description new-block-name)))
                     (continue ()
                       ;; In order to recover from the error, we ignore
                       ;; the RETURN-FROM form and only compile the return
                       ;; value form (or NIL if no return value form was
                       ;; present).
                       (return-from convert-special
                         (convert client
                                  value-cst
                                  environment)))))
          (cleavir-ast:make-ast 'cleavir-ast:return-from-ast
           :block-ast (trucler:identity info)
           :form-ast (convert client
                      value-cst
                      environment)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting EVAL-WHEN.

(defun check-eval-when-syntax (cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let ((situations-cst (cst:second cst)))
    (unless (cst:proper-list-p situations-cst)
      (error 'situations-must-be-proper-list :cst situations-cst))
    ;; Check each situation.
    (loop for remaining = situations-cst then (cst:rest remaining)
          until (cst:null remaining)
          do (let* ((situation-cst (cst:first remaining))
                    (situation (cst:raw situation-cst)))
               (unless (and (symbolp situation)
                            (member situation
                                    '(:compile-toplevel :load-toplevel :execute
                                      compile load eval)))
                 (error 'invalid-eval-when-situation :cst situation-cst))))))

(defmethod convert-special (client (symbol (eql 'eval-when)) cst environment)
  (check-eval-when-syntax cst)
  (with-preserved-toplevel-ness
    (cst:db s (eval-when-cst situations-cst . body-cst) cst
      (declare (ignore eval-when-cst))
      (let ((situations (cst:raw situations-cst)))
        (if (or (not *use-file-compilation-semantics-p*)
                (not *current-form-is-top-level-p*))
            (if (or (member :execute situations)
                    (member 'cl:eval situations))
                (process-progn
                 (convert-sequence client
                                   body-cst
                                   environment))
                (convert client
                         (make-atom-cst nil s)
                         environment))
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
                      (convert-sequence client
                                        body-cst
                                        environment))))
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
                      (convert-sequence client
                                        body-cst
                                        environment))))
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
                   (cst-eval client
                             (cst:cons (make-atom-cst 'progn s)
                                       body-cst)
                             environment)
                   (convert client
                            (make-atom-cst nil s)
                            environment))
                  (t
                   (convert client
                            (make-atom-cst nil s)
                            environment))))))))

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

;;; Given an environment and the name of a function, return the
;;; LEXICAL-AST that will have the function with that name as a value.
;;; It is known that the environment contains an entry corresponding
;;; to the name given as an argument.
(defun function-lexical (client environment name)
  (trucler:identity (trucler:describe-function client environment name)))

;;; Convert a local function definition.
(defun convert-local-function (client definition-cst environment)
  ;; FIXME: The error message if this check fails needs improvement.
  (check-argument-count definition-cst 1 nil)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (unless (proper-function-name-p name-cst)
      (error 'function-name-must-be-proper-function-name :cst name-cst))
    (let ((block-name-cst (block-name-from-function-name name-cst)))
      (convert-code client lambda-list-cst
                    body-cst
                    environment
                    :block-name-cst block-name-cst))))

;;; Convert a CST representing a list of local function definitions.
(defun convert-local-functions (client definitions-cst environment)
  (loop for remaining = definitions-cst
          then (cst:rest remaining)
        until (cst:null remaining)
        collect (let* ((def-cst (cst:first remaining))
                       (*origin* (cst:source def-cst))
                       (fun (convert-local-function
                             client
                             def-cst
                             environment))
                       ;; Compute these after calling
                       ;; CONVERT-LOCAL-FUNCTION so that we know that
                       ;; DEF-CST is actually a list.
                       (name-cst (cst:first def-cst))
                       (name (cst:raw name-cst)))
                  (cons name fun))))

;;; Compute and return a list of SETQ-ASTs that will assign the AST of
;;; each function in a list of function ASTs to its associated
;;; LEXICAL-AST.  FUNCTIONS is a list of CONS cells.  Each CONS cell
;;; has a function name in its CAR and an AST in its CDR.
(defun compute-function-init-asts (client functions environment)
  (loop for (name . fun-ast) in functions
        collect (cleavir-ast:make-ast 'cleavir-ast:setq-ast
                  :lhs-ast (function-lexical client environment name)
                  :value-ast fun-ast)))

(defun check-function-bindings (bindings operator)
  (check-cst-proper-list bindings 'bindings-must-be-proper-list
                         :operator operator)
  (loop for remaining = bindings then (cst:rest remaining)
        until (cst:null remaining)
        do (check-cst-proper-list
            (cst:first remaining)
            'local-function-definition-must-be-proper-list)))

;;; FIXME: add the processing of DYNAMIC-EXTENT declarations.
(defmethod convert-special (client (symbol (eql 'flet)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (flet-cst definitions-cst . body-cst) cst
    (declare (ignore flet-cst))
    (check-function-bindings definitions-cst 'flet)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-cst)
      (let* ((global-environment
               (trucler:global-environment client environment))
             (declaration-proclamations
               (declaration-proclamations client global-environment))
             (canonical-declaration-specifiers
               (cst:canonicalize-declarations
                client declaration-proclamations declaration-csts))
             (defs (convert-local-functions client
                                            definitions-cst
                                            environment))
             (new-environment
               (augment-environment-from-fdefs client
                                               environment
                                               definitions-cst))
             (init-asts
               (compute-function-init-asts client
                                           defs
                                           new-environment))
             (final-environment
               (augment-environment-with-declarations
                client new-environment canonical-declaration-specifiers)))
        (process-progn
         (append init-asts
                 ;; So that flet with empty body works.
                 (list
                  (process-progn
                   (convert-sequence client
                                     forms-cst
                                     final-environment)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LABELS.

(defmethod convert-special (client (symbol (eql 'labels)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (labels-cst definitions-cst . body-cst) cst
    (declare (ignore labels-cst))
    (check-function-bindings definitions-cst 'labels)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-cst)
      (let* ((global-environment
               (trucler:global-environment client environment))
             (declaration-proclamations
               (declaration-proclamations client global-environment))
             (canonical-declaration-specifiers
               (cst:canonicalize-declarations
                client declaration-proclamations declaration-csts))
             (new-environment
               (augment-environment-from-fdefs client
                                               environment
                                               definitions-cst))
             (defs (convert-local-functions client
                                            definitions-cst
                                            new-environment))
             (init-asts
               (compute-function-init-asts client defs new-environment))
             (final-environment
               (augment-environment-with-declarations
                client new-environment canonical-declaration-specifiers)))
        (process-progn
         (append init-asts
                 ;; So that flet with empty body works.
                 (list
                  (process-progn
                   (convert-sequence client
                                     forms-cst
                                     final-environment)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting TAGBODY.
;;;
;;; The TAGBODY special form always returns NIL.  We generate a PROGN
;;; with the TAGBODY-AST and a CONSTANT-AST in it, because the
;;; TAGBODY-AST (unlike the TAGBODY special form) does not generate a
;;; value.

(defun tagp (item)
  (let ((raw (cst:raw item)))
    ;; GO tags are symbols or integers, per CLHS glossary.
    (or (symbolp raw)
        (integerp raw))))

(defmethod convert-special (client (symbol (eql 'tagbody)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (tagbody-cst . body-cst) cst
    (declare (ignore tagbody-cst))
    (let ((tag-asts
            (loop for rest = body-cst then (cst:rest rest)
                  until (cst:null rest)
                  when (tagp (cst:first rest))
                    collect (let ((tag-cst (cst:first rest)))
                              (cleavir-ast:make-ast 'cleavir-ast:tag-ast
                                :name (cst:raw tag-cst)))))
          (new-environment environment))
      (loop for ast in tag-asts
            do (setf new-environment
                     (trucler:add-tag
                      client new-environment (cleavir-ast:name ast) ast)))
      (let ((item-asts (loop for rest = body-cst then (cst:rest rest)
                             until (cst:null rest)
                             collect (let ((item-cst (cst:first rest)))
                                       (if (tagp item-cst)
                                           (pop tag-asts)
                                           (convert client
                                                    item-cst
                                                    new-environment))))))
        (process-progn
         (list (cleavir-ast:make-ast 'cleavir-ast:tagbody-ast
                 :item-asts item-asts)
               (let ((*origin* origin))
                 (convert-constant client
                                   (make-atom-cst nil origin)
                                   environment))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting GO.

(defmethod convert-special (client (symbol (eql 'go)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (cst:db origin (go-cst tag-cst) cst
    (declare (ignore go-cst))
    (let ((info (describe-tag client environment (cst:raw tag-cst))))
      (cleavir-ast:make-ast 'cleavir-ast:go-ast
        :tag-ast (trucler:identity info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting CATCH.
;;;
;;; The conversion of the CATCH special form works as follows:
;;;
;;; (catch tag form*)
;;;
;;; turns into
;;;
;;; (block <name>
;;;   (catch-ast
;;;     tag
;;;     (lambda (values) (return-from <name> (values-list values)))
;;;     form*))
;;;
;;; where CATCH-AST is this AST.

;;; FIXME: Transmit source information to various ASTs.
(defmethod convert-special (client (symbol (eql 'catch)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (catch-cst tag-cst . body-csts) cst
    (declare (ignore catch-cst))
    (let* ((block-ast
             (cleavir-ast:make-ast 'cleavir-ast:block-ast))
           (argument-ast
             (cleavir-ast:make-ast 'cleavir-ast:lexical-ast
               :name 'values))
           (call-ast
             (cleavir-ast:make-ast 'cleavir-ast:named-call-ast
               :callee-name 'values-list
               :argument-asts (list argument-ast)))
           (return-from-ast
             (cleavir-ast:make-ast 'cleavir-ast:return-from-ast
               :block-ast block-ast
               :form-ast call-ast))
           (function-ast
             (cleavir-ast:make-ast 'cleavir-ast:function-ast
               :lambda-list (list argument-ast)
               :body-ast return-from-ast))
           (tag-ast
             (convert client tag-cst environment))
           (body-ast
             (process-progn
              (convert-sequence client body-csts environment)))
           (catch-ast
             (cleavir-ast:make-ast 'cleavir-ast:catch-ast
               :tag-ast tag-ast
               :throw-function-ast function-ast
               :body-ast body-ast)))
      (setf (cleavir-ast:body-ast block-ast)
            catch-ast)
      block-ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting IF.

(defmethod convert-special (client (symbol (eql 'if)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 3)
  (cst:db origin (if-cst test-cst then-cst . tail-cst) cst
    (declare (ignore if-cst))
    (let ((test-ast (convert client
                             test-cst
                             environment))
          (true-ast (convert client
                             then-cst
                             environment))
          (false-ast (if (cst:null tail-cst)
                         (convert-constant client
                                           (make-atom-cst nil origin)
                                           environment)
                         (cst:db s (else-cst) tail-cst
                           (convert client
                                    else-cst
                                    environment)))))
      (if (typep test-ast 'cleavir-ast:boolean-ast-mixin)
          (cleavir-ast:make-ast 'cleavir-ast:if-ast
           :test-ast test-ast
           :then-ast true-ast
           :else-ast false-ast)
          (cleavir-ast:make-ast 'cleavir-ast:if-ast
           :test-ast
           (cleavir-ast:make-ast 'cleavir-ast:eq-ast
            :arg1-ast test-ast
            :arg2-ast (convert-constant client
                       (make-atom-cst nil origin)
                       environment))
           :then-ast false-ast
           :else-ast true-ast)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOAD-TIME-VALUE.

(defmethod convert-special (client (symbol (eql 'load-time-value)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (cst:db origin (load-time-value-cst form-cst . remaining-cst) cst
    (declare (ignore load-time-value-cst))
    ;; FIXME: We only check whether the READ-ONLY-P flag is well
    ;; formed, but we don't use it for anything.
    (unless (cst:null remaining-cst)
      (let ((read-only-p (cst:raw (cst:first remaining-cst))))
        (if (member read-only-p '(nil t))
            read-only-p
            ;; The HyperSpec specifically requires a "Boolean"
            ;; and not a "generalized Boolean".
            (error 'read-only-p-must-be-boolean
                   :cst (cst:first remaining-cst)))))
    ;; LOAD-TIME-VALUE forms are treated differently, depending on whether
    ;; we are performing file compilation, or whether we are using
    ;; CL:COMPILE.  In the former case, we must arrange that the execution
    ;; of the form occurs at load time in a null lexical environment.  In
    ;; the latter case, the form is evaluated in a null lexical environment
    ;; at compile time, and the result is used instead as a literal object.
    (if *use-file-compilation-semantics-p*
        (let ((lexical-ast (cleavir-ast:make-ast 'cleavir-ast:lexical-ast
                             :name (gensym))))
          (push (cleavir-ast:make-ast 'cleavir-ast:setq-ast
                  :lhs-ast lexical-ast
                  :value-ast
                  (convert
                   client
                   form-cst
                   (trucler:global-environment client environment)))
                *prologue*)
          lexical-ast)
        (convert-constant
         client
         (cst:cst-from-expression
          (cst-eval client form-cst environment))
         environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting PROGN.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, PROGN processes
;;; its subforms the same way as the form itself.

(defmethod convert-special (client (symbol (eql 'progn)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (with-preserved-toplevel-ness
    (cst:db origin (progn-cst . form-csts) cst
      (declare (ignore progn-cst))
      (process-progn (convert-sequence client
                                       form-csts
                                       environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MACROLET.
;;;
;;; According to section 3.2.3.1 of the HyperSpec, MACROLET processes
;;; its subforms the same way as the form itself.

;;; Given the CST for a MACROLET definition and an environment, return
;;; a macro expander (or macro function) for the definition.
;;; FIXME: check syntax.

;;; FIXME: Once we have a CST version of PARSE-MACRO we should use it
;;; instead, and then call CST-EVAL on the resulting lambda expression.
(defun expander (client definition-cst environment)
  (cst:db origin (name-cst lambda-list-cst . body-cst) definition-cst
    (let ((lambda-expression (cst:parse-macro client
                                              name-cst
                                              lambda-list-cst
                                              (cst:raw body-cst)
                                              environment)))
      (eval client
            lambda-expression
            (trucler:restrict-for-macrolet-expander client environment)))))

(defmethod convert-special (client (symbol (eql 'macrolet)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore macrolet-cst))
    (unless (cst:proper-list-p definitions-cst)
      (error 'macrolet-definitions-must-be-proper-list :cst definitions-cst))
    (let ((new-environment environment))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (let* ((definition-cst (cst:first remaining))
                      (name-cst (cst:first definition-cst))
                      (name (cst:raw name-cst))
                      (expander (expander client definition-cst environment)))
                 (setf new-environment
                       (trucler:add-local-macro client new-environment name expander))))
      (with-preserved-toplevel-ness
        (convert client
                 (cst:cons (make-atom-cst 'locally origin)
                           body-cst)
                 new-environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SYMBOL-MACROLET.

(defmethod convert-special (client (head (eql 'symbol-macrolet)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (symbol-macrolet-cst definitions-cst . body-cst) cst
    (declare (ignore symbol-macrolet-cst))
    (let ((new-environment environment))
      (loop for remaining = definitions-cst then (cst:rest remaining)
            until (cst:null remaining)
            do (cst:db definition-origin (name-cst expansion-cst)
                   (cst:first remaining)
                 (let ((name (cst:raw name-cst))
                       (expansion (cst:raw expansion-cst)))
                   (setf new-environment
                         (trucler:add-local-symbol-macro
                          client new-environment name expansion)))))
      (with-preserved-toplevel-ness
        (convert client
                 (cst:cons (make-atom-cst 'locally origin)
                           body-cst
                           :source origin)
                 new-environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting FUNCTION.

(defun convert-named-function (client name-cst environment)
  (let ((info (describe-function client environment (cst:raw name-cst))))
    (convert-function-reference client name-cst info environment)))

(defun convert-lambda-function (client lambda-form-cst environment)
  (let ((*origin*
          (if (null (cst:source lambda-form-cst))
              (cst:source (cst:second lambda-form-cst))
              (cst:source lambda-form-cst))))
    (convert-code
     client
     (cst:second lambda-form-cst)
     (cst:rest (cst:rest lambda-form-cst))
     environment)))

(defun check-function-syntax (cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1)
  (let ((function-name-cst (cst:second cst)))
    (cond ((proper-function-name-p function-name-cst)
           nil)
          ((cst:consp function-name-cst)
           (unless (eq (cst:raw (cst:first function-name-cst)) 'lambda)
             (error 'function-argument-must-be-function-name-or-lambda-expression
                    :cst function-name-cst))
           (unless (cst:proper-list-p function-name-cst)
             (error 'lambda-must-be-proper-list :cst function-name-cst)))
          (t
           (error 'function-argument-must-be-function-name-or-lambda-expression
                  :cst function-name-cst)))))

(defmethod convert-special (client (symbol (eql 'function)) cst environment)
  (check-function-syntax cst)
  (cst:db origin (function-cst name-cst) cst
    (declare (ignore function-cst))
    (let ((result (if (proper-function-name-p name-cst)
                      (convert-named-function client
                                              name-cst
                                              environment)
                      (convert-lambda-function client
                                               name-cst
                                               environment))))
      (reinitialize-instance result :origin origin))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting THE.

;;; Given a type in values position (i.e., an argument to THE or
;;; return value of a function type), return three values: a list of
;;; REQUIRED types, a list of OPTIONAL types, and a REST type, and
;;; whether a REST was present.  e.g., (values integer &optional cons
;;; &rest symbol) => (INTEGER), (CONS), SYMBOL, T.

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
  ;; To allow too many values, just force a &rest if and only of
  ;; unspecified.
  (unless restp (setf rest 't))
  ;; Too few values is more difficult.
  ;; Any values not returned by the THE form are considered NIL,
  ;; so if a "required" type includes NIL, it could also be no-value.
  ;; And on the flipside, if a type does NOT include NIL, the form
  ;; must actually return a value for it.
  ;; Therefore, we can just make all types on the end of REQ that
  ;; do not include NIL optional.
  ;; A further complication is that, since this is compile-time,
  ;; some types may not be defined enough for TYPEP to work
  ;; (e.g., SATISFIES with an undefined function) as mentioned in
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
         ;; If we found something, we need the next position
         ;; for the next bit. If we didn't, zero.
         ;; E.g., (values integer list) => lastpos = 1
         (lastpos (if lastpos (1+ lastpos) 0))
         ;; And new-opt = (list).
         (new-opt (nthcdr lastpos req))
         ;; And new-req = (integer).
         (new-req (ldiff req new-opt)))
    (setf req new-req
          opt (append new-opt opt)))
  (values req opt rest))

;;; The-values-components: compose the above two functions.
(defun the-values-components (values-type)
  (multiple-value-call #'fudge-values-type
    (parse-values-type values-type)))

(defmethod convert-special (client (symbol (eql 'the)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (cst:db origin (the-cst value-type-cst form-cst) cst
    (declare (ignore the-cst))
    (multiple-value-bind (req opt rest)
        (the-values-components (cst:raw value-type-cst))
      ;; We don't bother collapsing THE forms for user code.
      (cleavir-ast:make-ast 'cleavir-ast:the-ast
        :form-ast (convert client form-cst environment)
        :required req
        :optional opt
        :rest rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting MULTIPLE-VALUE-PROG1.

(defmethod convert-special
    (client (symbol (eql 'multiple-value-prog1)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (multiple-value-prog1-cst first-cst . rest-cst) cst
    (declare (ignore multiple-value-prog1-cst))
    (cleavir-ast:make-ast 'cleavir-ast:multiple-value-prog1-ast
      :first-form-ast (convert client
                       first-cst
                       environment)
      :form-asts (convert-sequence client
                  rest-cst
                  environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Methods specialized to operators for which we do not provide a
;;; conversion method.

;;; Implementations should probably convert this in terms of
;;; CLEAVIR-PRIMOP:MULTIPLE-VALUE-CALL.
(defmethod convert-special
    (client (symbol (eql 'multiple-value-call)) cst environment)
  (declare (ignore client environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special (client (symbol (eql 'unwind-protect)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (cst:db origin (unwind-protect-cst protected-form-cst . cleanup-form-csts) cst
    (declare (ignore unwind-protect-cst))
    (let ((cleanup-thunk-cst
            (cst:cons (cst:cst-from-expression 'lambda)
                      (cst:cons (cst:cst-from-expression '())
                            cleanup-form-csts))))
      (cleavir-ast:make-ast 'cleavir-ast:unwind-protect-ast
        :protected-form-ast (convert client
                             protected-form-cst
                             environment)
        :cleanup-thunk-ast (convert client
                            cleanup-thunk-cst
                            environment)))))

(defmethod convert-special (client (symbol (eql 'throw)) cst environment)
  (declare (ignore client environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 2)
  (error 'no-default-method :operator symbol :cst cst))

(defmethod convert-special (client (symbol (eql 'progv)) cst environment)
  (declare (ignore client environment))
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 nil)
  (error 'no-default-method :operator symbol :cst cst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting SETQ.
;;;

(defmethod convert-special (client (symbol (eql 'setq)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (unless (oddp (length (cst:raw cst)))
    (error 'setq-must-have-even-number-of-arguments :cst cst))
  (let* ((csts (cst:listify (cst:rest cst)))
         (form-asts (loop for (variable-cst form-cst) on csts by #'cddr
                          for variable = (cst:raw variable-cst)
                          unless (symbolp variable)
                            do (error 'setq-var-must-be-symbol
                                      :cst variable-cst)
                          collect (convert-elementary-setq
                                   client
                                   variable-cst
                                   form-cst
                                   environment))))
    (process-progn form-asts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET.
;;;

(defmethod convert-special (client (symbol (eql 'let)) cst environment)
  (convert-let client cst environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LET*.
;;;

(defmethod convert-special (client (symbol (eql 'let*)) cst environment)
  (convert-let* client cst environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Converting LOCALLY.
;;;

(defmethod convert-special (client (symbol (eql 'locally)) cst environment)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (cst:db origin (locally-cst . body-forms-cst) cst
    (declare (ignore locally-cst))
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((global-environment
               (trucler:global-environment client environment))
             (declaration-proclamations
               (declaration-proclamations client global-environment))
             (canonical-declaration-specifiers
               (cst:canonicalize-declarations
                client declaration-proclamations declaration-csts))
             (new-environment
               (augment-environment-with-declarations
                client environment canonical-declaration-specifiers)))
        (with-preserved-toplevel-ness
          (process-progn (convert-sequence client
                                           forms-cst
                                           new-environment)))))))
