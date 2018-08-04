(cl:in-package #:cleavir-cst-to-ast)

;;; Check the syntax of a single LET or LET* binding.  If the syntax
;;; is incorrect, signal an error.
(defun check-binding (cst)
  (cond ((or (and (cst:atom cst)
                  (symbolp (cst:raw cst)))
             (and (cst:consp cst)
                  (cst:atom (cst:first cst))
                  (symbolp (cst:raw (cst:first cst)))
                  (or (cst:null (cst:rest cst))
                      (and (cst:consp (cst:rest cst))
                           (cst:null (cst:rest (cst:rest cst)))))))
         nil)
        ((cst:atom cst)
         (error 'binding-must-be-symbol-or-list
                :expr (cst:raw cst)
                :origin (cst:source cst)))
        ((or (and (cst:atom (cst:rest cst))
                  (not (cst:null (cst:rest cst))))
             (not (cst:null (cst:rest (cst:rest cst)))))
         (error 'binding-must-have-length-one-or-two
                :expr (cst:raw cst)
                :origin (cst:source cst)))
        (t
         (error 'variable-must-be-a-symbol
                :expr (cst:raw (cst:first cst))
                :origin (cst:source (cst:first cst))))))

;;; Check the syntax of the bindings of a LET or a LET* form.  If the
;;; syntax is incorrect, signal an error and propose a restart for
;;; fixing it up.
(defun check-bindings (cst)
  (check-cst-proper-list cst 'bindings-must-be-proper-list)
  (loop for remaining = cst then (cst:rest remaining)
        until (cst:null remaining)
        do (check-binding (cst:first remaining))))

(defun make-let-init-asts (initform-csts temp-asts idspecs environment system)
  (loop for initform-cst in initform-csts
        for converted = (convert initform-cst environment system)
        for temp-ast in temp-asts
        collect (cleavir-ast:make-setq-ast temp-ast converted)))

;;; We convert a LET form CST by assignment.
(defun process-remaining-let-bindings (bindings idspecs rdspecs body-forms-cst environment system)
  (if (null bindings)
      ;; We ran out of bindings.  We must build an AST for the body of
      ;; the function.
      (let ((new-env (augment-environment-with-declarations environment rdspecs)))
	(process-progn (convert-sequence body-forms-cst new-env system)))
      (destructuring-bind (variable-cst . lexical-ast) (first bindings)
	(let* (;; We enter the new variable into the environment and
	       ;; then we process remaining parameters and ultimately
	       ;; the body of the function.
	       (new-env (augment-environment-with-variable
			 variable-cst (first idspecs) environment environment))
	       ;; We compute the AST of the remaining computation by
	       ;; recursively calling this same function with the
	       ;; remaining bindings (if any) and the environment that
	       ;; we obtained by augmenting the original one with the
	       ;; parameter variable.
	       (next-ast (process-remaining-let-bindings (rest bindings)
							 (rest idspecs)
							 rdspecs
							 body-forms-cst
							 new-env
							 system)))
	  ;; All that is left to do now, is to construct the AST to
	  ;; return by using the new variable and the AST of the
	  ;; remaining computation as components.
	  (set-or-bind-variable variable-cst lexical-ast next-ast new-env system)))))

(defmethod convert-let (cst environment system)
  (when (cst:null (cst:rest cst))
    (error 'let-or-let*-must-have-at-least-one-argument
           :expr (cst:raw cst)
           :origin (cst:source cst)))
  (cst:db origin (let-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let-cst))
    (check-bindings bindings-cst)
    (multiple-value-bind (declarations-cst body-forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-dspecs
               (cst:canonicalize-declarations system declarations-cst))
             (binding-csts (cst:listify bindings-cst))
             (variable-csts (loop for binding-cst in binding-csts
                                  collect (if (cst:atom binding-cst)
                                              binding-cst
                                              (cst:first binding-cst))))
             (initform-csts (loop for binding-cst in binding-csts
                                  collect (if (or (cst:atom binding-cst)
                                                  (cst:null (cst:rest binding-cst)))
                                              (cst:cst-from-expression 'nil)
                                              (cst:second binding-cst))))
             (temp-asts (loop repeat (length binding-csts)
                              collect (cleavir-ast:make-lexical-ast (gensym)))))
        (multiple-value-bind (idspecs rdspecs)
            (itemize-declaration-specifiers (mapcar #'list variable-csts)
                                            canonical-dspecs)
          (process-progn
           (append (make-let-init-asts initform-csts temp-asts
                                       idspecs environment system)
                   (list (process-remaining-let-bindings
                          (mapcar #'cons variable-csts temp-asts)
                          idspecs
                          rdspecs
                          body-forms-cst
                          environment
                          system)))))))))

;;; We convert a LET* form CST by transforming it into nested LET form
;;; CSTs and then converting those instead.  This is not trivial,
;;; because we need to associate the right declarations with the
;;; corresponding LET form CST.

(defmethod convert-let* (cst environment system)
  (when (cst:null (cst:rest cst))
    (error 'let-or-let*-must-have-at-least-one-argument
           :expr (cst:raw cst)
           :origin (cst:source cst)))
  (cst:db origin (let*-cst bindings-cst . body-forms-cst) cst
    (declare (ignore let*-cst))
    (check-bindings bindings-cst)
    (multiple-value-bind (declaration-csts forms-cst)
        (cst:separate-ordinary-body body-forms-cst)
      (let* ((canonical-declaration-specifiers
              (cst:canonicalize-declarations system declaration-csts))
             (binding-csts (cst:listify bindings-cst))
             (variable-csts
               (loop for binding-cst in binding-csts
                     collect (if (cst:atom binding-cst)
                                 binding-cst
                                 (cst:first binding-cst)))))
        (multiple-value-bind (item-specific-dspecs remaining-dspecs)
            (itemize-declaration-specifiers (mapcar #'list variable-csts)
                                            canonical-declaration-specifiers)
          (loop with remaining-dspecs-cst = (cst:cstify remaining-dspecs)
                with result = (cst:cstify
                               (cons (cst:cst-from-expression 'locally)
                                     (if (null remaining-dspecs)
                                         (cst:listify forms-cst)
                                         (cons
                                          (cst:cons (cst:cst-from-expression 'declare)
                                                    remaining-dspecs-cst
                                                    :source (cst:source remaining-dspecs-cst))
                                          (cst:listify forms-cst)))))
                for binding-cst in (reverse binding-csts)
                for declaration-cst in (reverse item-specific-dspecs)
                do (setf result
                         (cst:cstify
                          (cons (cst:cst-from-expression 'let)
                                (cons (cst:list binding-cst)
                                      (if (null declaration-cst)
                                          (list result)
                                          (list (cst:cstify
                                                 (cons
                                                  (cst:cst-from-expression 'declare)
                                                  declaration-cst))
                                                result))))))
                finally (return (convert result environment system))))))))
