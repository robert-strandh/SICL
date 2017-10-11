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

;;; Extend a CST, i.e. create a new CST that has the elements of CST
;;; as its first elements, but that has CST-versions of
;;; ADDITIONAL-ELEMENTS as its remaining elements.
(defun extend-cst (cst &rest additional-elements)
  (labels ((aux (cst raw)
             (if (cst:null cst)
                 (cst:cst-from-expression raw)
                 (make-instance 'cst:cons-cst
                   :raw raw
                   :source (cst:source cst)
                   :first (cst:first cst)
                   :rest (aux (cst:rest cst) (rest raw))))))
    (aux cst (append (cst:raw cst) additional-elements))))

;;; Check the syntax of a single LET or LET* binding.  If the syntax
;;; is incorrect, signal an error and propose a restart for fixing it
;;; up.
(defun check-binding (cst)
  (cond ((or (and (cst:atom cst)
                  (symbolp (cst:raw cst)))
             (and (cst:consp cst)
                  (cst:atom (cst:first cst))
                  (symbolp (cst:raw (cst:first cst)))
                  (or (cst:null (cst:rest cst))
                      (and (cst:consp (cst:rest cst))
                           (cst:null (cst:rest (cst:rest cst)))))))
         cst)
        ((cst:atom cst)
         (restart-case (error 'binding-must-be-symbol-or-list
                              :expr (cst:raw cst)
                              :origin (cst:source cst))
           (recover ()
             :report (lambda (stream)
                       (format stream "Replace with a symbol."))
             (cst:cst-from-expression (gensym)))))
        ((or (and (cst:atom (cst:rest cst))
                  (not (cst:null (cst:rest cst))))
             (not (cst:null (cst:rest (cst:rest cst)))))
         (restart-case (error 'binding-must-have-length-one-or-two
                              :expr (cst:raw cst)
                              :origin (cst:source cst))
           (recover ()
             :report (lambda (stream)
                       (format stream "Replace with a correct binding."))
             (if (and (cst:atom (cst:rest cst))
                      (not (cst:null (cst:rest cst))))
                 (if (symbolp (cst:raw (cst:first cst)))
                     ;; Replace the binding with the variable.
                     (cst:first cst)
                     ;; Otherwise, generate a symbol
                     (cst:cst-from-expression (gensym)))
                 (let ((first (cst:first cst))
                       (second (cst:second cst)))
                   (cst:cons first
                             (cst:cons second
                                       (cst:list)
                                       :source (cst:source second))
                             :source (cst:source first)))))))
        (t
         (restart-case (error 'variable-must-be-a-symbol
                              :expr (cst:raw (cst:first cst))
                              :origin (cst:source (cst:first cst)))
           (recover ()
             :report (lambda (stream)
                       (format stream "Replace with a symbol."))
             (cst:cst-from-expression (gensym)))))))

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

;;; Take a CST, check whether it represents a proper list.  If it
;;; does, then return it.  If not signal an error that allows a
;;; restart that creates a patched version of CST that represents a
;;; proper list.  Error type is a symbol that is passed to ERROR.
(defun check-cst-proper-list (cst error-type)
  (if (cst:proper-list-p cst)
      (restart-case (error error-type
                           :expr (cst:raw cst)
                           :origin (cst:source cst))
        (recover ()
          :report (lambda (stream)
                    (format stream "Turn it into a proper list."))
          (return-from check-cst-proper-list (make-cst-proper cst))))
      cst))

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
