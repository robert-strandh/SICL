(cl:in-package #:cleavir-cst-to-ast)

(defun make-atom-cst (object &optional origin)
  (make-instance 'cst:atom-cst :raw object :source origin))

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
;;; Checking FUNCTION.

(defun proper-function-name-p (name-cst)
  (let ((name (cst:raw name-cst)))
    (or (symbolp name)
        (and (cleavir-code-utilities:proper-list-p name)
             (= (length name) 2)
             (eq (car name) 'setf)
             (symbolp (cadr name))))))
