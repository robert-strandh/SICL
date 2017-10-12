(cl:in-package #:cleavir-cst-to-ast)

;;; Check that the syntax of a special form is correct.  The special
;;; form is represented as a CST.  OPERATOR is the name of the special
;;; operator of the special form.  Primary methods on this generic
;;; function signal an error when an incorrect syntax is detected.  An
;;; :AROUND method proposes restarts that replace the CST by one that
;;; signals an error at run time.  The replacement CST is returned by
;;; the :AROUND method.
(defgeneric check-special-form-syntax (operator cst))

(defmethod check-special-form-syntax :around (operator cst)
  (restart-case (progn (call-next-method) cst)
    (recover ()
      :report "Recover by replacing form by a call to ERROR."
      (cst:cst-from-expression
       '(error 'run-time-program-error
         :expr (cst:raw cst)
         :origin (cst:source cst))))))

;;; Take a CST, check whether it represents a proper list.  If it does
;;; not represent ERROR-TYPE is a symbol that is passed to ERROR.
(defun check-cst-proper-list (cst error-type)
  (unless  (cst:proper-list-p cst)
    (error error-type
           :expr (cst:raw cst)
           :origin (cst:source cst))))

;;; Check that the number of arguments greater than or equal to MIN
;;; and less than or equal to MAX.  When MAX is NIL, then there is no
;;; upper bound on the number of arguments.  If the argument count is
;;; wrong, then signal an error.  It is assumed that CST represents a
;;; proper list, so this must be checked first by the caller.
(defun check-argument-count (cst min max)
  (let ((count (1- (length (cst:raw cst)))))
    (unless (and (>= count min)
                 (or (null max)
                     (<= count max)))
      (error 'incorrect-number-of-arguments
             :expr (cst:raw cst)
             :expected-min min
             :expected-max max
             :observed count
             :origin (cst:source cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking QUOTE.

(defmethod check-special-form-syntax ((operator (eql 'quote)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking BLOCK.

(defmethod check-special-form-syntax ((operator (eql 'block)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let* ((name-cst (cst:second cst))
         (name (cst:raw name-cst)))
    (unless (symbolp name)
      (error 'block-name-must-be-a-symbol
             :expr name
             :origin (cst:source name-cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking EVAL-WHEN.

(defmethod check-special-form-syntax ((operator (eql 'eval-when)) cst)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FLET.

(defmethod check-special-form-syntax ((operator (eql 'flet)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let ((definitions (cst:second cst)))
    (unless (cst:proper-list-p definitions)
      (error 'flet-functions-must-be-proper-list
             :expr (cst:raw definitions)
             :origin (cst:source definitions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LABELS.

(defmethod check-special-form-syntax ((operator (eql 'labels)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let ((definitions (cst:second cst)))
    (unless (cst:proper-list-p definitions)
      (error 'labels-functions-must-be-proper-list
             :expr (cst:raw definitions)
             :origin (cst:source definitions)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking FUNCTION.

(defun proper-function-name-p (name-cst)
  (let ((name (cst:raw name-cst)))
    (or (symbolp name)
        (and (cleavir-code-utilities:proper-list-p name)
             (= (length name) 2)
             (eq (car name) 'setf)
             (symbolp (cadr name))))))

(defmethod check-special-form-syntax ((operator (eql 'function)) cst)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking GO.

(defmethod check-special-cst-syntax ((operator (eql 'go)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking IF.

(defmethod check-special-cst-syntax ((operator (eql 'if)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LET and LET*

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
        until (cst:null cst)
        do (check-binding (cst:first remaining))))

(defmethod check-special-cst-syntax ((operator (eql 'let)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (check-bindings (cst:second cst)))

(defmethod check-special-cst-syntax ((operator (eql 'let*)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (check-bindings (cst:second cst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LOAD-TIME-VALUE.

(defmethod check-special-form-syntax ((operator (eql 'load-time-value)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (let ((tail-cst (cst:rest (cst:rest cst))))
    (unless (cst:null tail-cst)
      ;; The HyperSpec specifically requires a "boolean"
      ;; and not a "generalized boolean".
      (unless (member (cst:raw (cst:first tail-cst)) '(nil t))
        (error 'read-only-p-must-be-boolean
               :expr (cst:first tail-cst)
               :origin (cst:source (cst:first tail-cst)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking LOCALLY.

(defmethod check-special-form-syntax ((operator (eql 'locally)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MACROLET.

(defmethod check-special-form-syntax ((operator (eql 'macrolet)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil)
  (let ((definitions-cst (cst:second cst)))
    (unless (cst:proper-list-p definitions-cst)
      (error 'macrolet-definitions-must-be-proper-list
             :expr (cst:raw definitions-cst)
             :origin (cst:source definitions-cst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MULTIPLE-VALUE-CALL.

(defmethod check-special-form-syntax ((operator (eql 'multiple-value-call)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking MULTIPLE-VALUE-PROG1.

(defmethod check-special-form-syntax ((operator (eql 'multiple-value-prog1)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking PROGN.

(defmethod check-special-form-syntax ((operator (eql 'progn)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking PROGV.

(defmethod check-special-form-syntax ((operator (eql 'progv)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 2 nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking RETURN-FROM.

(defmethod check-special-form-syntax ((operator (eql 'return-from)) cst)
  (check-cst-proper-list cst 'form-must-be-proper-list)
  (check-argument-count cst 1 2)
  (let* ((block-name-cst (cst:second cst))
         (block-name (cst:raw block-name-cst)))
    (unless (symbolp block-name)
      (error 'block-name-must-be-a-symbol
             :expr block-name
             :origin (cst:source block-name-cst)))))
