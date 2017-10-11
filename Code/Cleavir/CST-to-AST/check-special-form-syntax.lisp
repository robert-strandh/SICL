(cl:in-package #:cleavir-cst-to-ast)

;;; Return true if and only if the number of arguments greater than or
;;; equal to MIN and less than or equal to MAX.  When MAX is NIL, then
;;; there is no upper bound on the number of arguments.  It is assumed
;;; that CST represents a proper list, so this must be checked first
;;; by the caller.
(defun check-argument-count (cst min max)
  (let ((count (1- (length (cst:raw cst)))))
    (and (>= count min)
         (or (null max)
             (<= count max)))))

;;; CST is known to represent a proper list, and it is known that the
;;; length of that proper list is strictly greater than LENGTH.
;;; Return a CST that has the same first LENGTH elements as CST,
;;; preserving the origin of component CSTs as much as possible.
(defun shorten-cst (cst length)
  (labels ((aux (cst length raw)
             (if (zerop length)
                 (cst:cst-from-expression nil)
                 (make-instance 'cst:cons-cst
                   :raw raw
                   :source (cst:source cst)
                   :first (cst:first cst)
                   :rest (aux (cst:rest cst) (1- length) (rest raw))))))
    (aux cst length (subseq (cst:raw cst) 0 length))))

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
;;; Checking QUOTE.

(defmethod check-special-form-syntax ((head (eql 'quote)) cst)
  (let ((cst (check-form-proper-list cst)))
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
                    (let ((raw '(quote nil)))
                      (make-instance 'cst:cons-cst
                        :raw raw
                        :source (cst:source cst)
                        :first (cst:first cst)
                        :rest (make-instance 'cons-cst
                                :raw (cdr raw)
                                :source nil
                                :first (make-instance 'cst:atom-cst
                                         :raw nil
                                         :source nil)
                                :rest (cst:rest cst))))
                    (let ((raw (subseq (cst:raw cst) 0 2)))
                      (make-instance 'cst:cons-cst
                        :raw raw
                        :source (cst:source cst)
                        :first (cst:first cst)
                        :rest (make-instance 'cons-cst
                                :raw (cdr raw)
                                :source nil
                                :first (cst:second cst)
                                :rest (make-instance 'cst:atom-cst
                                        :raw nil
                                        :source nil))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking BLOCK.

(defmethod check-special-form-syntax ((head (eql 'block)) cst)
  (let ((cst (check-form-proper-list cst)))
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
                (let ((raw '(block nil)))
                  (make-instance 'cst:cons-cst
                    :raw raw
                    :source (cst:source cst)
                    :first (cst:first cst)
                    :rest (make-instance 'cons-cst
                            :raw (cdr raw)
                            :source nil
                            :first (cst:second cst)
                            :rest (make-instance 'cst:atom-cst
                                    :raw nil
                                    :source nil))))))))))

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
