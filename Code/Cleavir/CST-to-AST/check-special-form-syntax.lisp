(cl:in-package #:cleavir-cst-to-ast)

(defgeneric check-special-form-syntax (head-symbol cst))

;;; The argument of this function is a CONS-CST, but it either
;;; terminates with an ATOM-CST that is not NULL, or else, it is a
;;; circular CST.  It makes a copy of CST except that the copy
;;; is terminated by a NULL CST.
(defun make-cst-proper (cst)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((aux (cst)
               (if (or (cst:atom cst)
                       (gethash cst table))
                   (cst:cst-from-expression nil)
                   (progn (setf (gethash cst table) t)
                          (let ((next (aux (cst:rest cst))))
                            (make-instance 'cst:cons-cst
                              :source (cst:source cst)
                              :rest next
                              :first (cst:first cst)
                              :raw (cons (car (cst:raw cst))
                                         (cst:raw next))))))))
      (aux cst))))

;;; Take a CST, check whether it represents a proper list.  If it
;;; does, then return it.  If not signal an error that allows a
;;; restart that creates a patched version of CST that represents a
;;; proper list.
(defun check-form-proper-list (cst)
  (if (cst:proper-list-p cst)
      (restart-case (error 'form-must-be-proper-list
                           :expr (cst:raw cst)
                           :origin (cst:source cst))
        (recover ()
          :report (lambda (stream)
                    (format stream "Turn it into a proper list."))
          (return-from check-form-proper-list (make-cst-proper cst))))
      cst))

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
