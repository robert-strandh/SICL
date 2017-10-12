(cl:in-package #:cleavir-cst-to-ast)

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
                          (cst:cons (cst:first cst)
                                    (aux (cst:rest cst))
                                    :source (cst:source cst))))))
      (aux cst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking QUOTE.

(defmethod check-special-form-syntax ((head (eql 'quote)) cst)
  (let ((cst (check-cst-proper-list cst 'form-must-be-proper-list)))
    (if (check-argument-count cst 1 1)
        cst
        (let ((count (1- (length (cst:raw cst)))))
          (restart-case (error 'incorrect-number-of-arguments
                               :expr (cst:raw cst)
                               :expected-min 1
                               :expected-max 1
                               :observed count
                               :origin (cst:source cst))
            (recover ()
              :report (lambda (stream)
                        (format stream "Correct the argument count."))
              (return-from check-special-form-syntax
                (if (zerop count)
                    (extend-cst cst nil)
                    (shorten-cst cst 1)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking BLOCK.

(defmethod check-special-form-syntax ((head (eql 'block)) cst)
  (let ((cst (check-cst-proper-list cst 'form-must-be-proper-list)))
    (if (check-argument-count cst 1 nil)
        cst
        (restart-case (error 'incorrect-number-of-arguments
                             :expr (cst:raw cst)
                             :expected-min 1
                             :expected-max nil
                             :observed 0
                             :origin (cst:source cst))
            (recover ()
              :report (lambda (stream)
                        (format stream "Correct the argument count."))
              (return-from check-special-form-syntax
                (extend-cst nil)))))))

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
