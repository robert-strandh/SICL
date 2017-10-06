(cl:in-package #:cleavir-cst-to-ast)

(defgeneric check-special-form-syntax (head-symbol cst))

(defun check-form-proper-list (cst)
  (unless (cst:proper-list-p cst)
    (error 'form-must-be-proper-list
           :expr (cst:raw cst)
           :origin (cst:source cst))))

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
