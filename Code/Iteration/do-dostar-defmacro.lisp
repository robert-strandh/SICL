(cl:in-package #:sicl-iteration)

;;; This code is in the public domain.
;;;
;;; The preliminary name for this project is SICL, which doesn't stand
;;; for anything in particular.  Pronounce it like "sickle".
;;;
;;; The purpose of this code is to provide a totally portable
;;; implementation of some high-level functionality of the Common Lisp
;;; language, so that implementors of Common Lisp systems can
;;; integrate it as it is into their systems, without having to
;;; implement and maintain a specific version of it. 
;;;
;;; Author: Robert Strandh (robert.strandh@gmail.com)
;;; Date: 2008
;;;
;;; A portable implementation of the Common Lisp
;;; iteration macros.  
;;; 
;;; This implementation does not use any iteration construct, nor any
;;; operations on sequences (other than the ones we define ourself
;;; here).  Implementations can therefore load this file very early on
;;; in the bootstrap process.  It allows for operations on sequences
;;; and the loop macro to be defined in terms of the macros defined
;;; here.

(macrolet ((define-do (name assignment-type)
             (multiple-value-bind (let-type setq-type)
                 (ecase assignment-type
                   (:sequential (values 'let* 'setq))
                   (:parallel (values 'let 'psetq)))
               `(defmacro ,name (variable-clauses end-test
                                 &body body)
                  ;; do some syntax checking
                  (check-variable-clauses ',name variable-clauses)
                  (body-must-be-proper-list ',name body)
                  (unless (and (cleavir-code-utilities:proper-list-p end-test)
                               (not (null end-test)))
                    (error 'malformed-end-test
                           :name ',name
                           :found end-test))
                  (multiple-value-bind (declarations forms)
                      (cleavir-code-utilities:separate-ordinary-body body)
                    (let ((start-tag (gensym)))
                      `(block nil
                         (,',let-type ,(extract-bindings
                                        variable-clauses)
                           ,@declarations
                           (tagbody
                              ,start-tag
                              (when ,(car end-test)
                                (return
                                  (progn ,@(cdr end-test))))
                              ,@forms
                              (,',setq-type ,@(extract-updates
                                               variable-clauses))
                              (go ,start-tag))))))))))
  (define-do do :parallel)
  (define-do do* :sequential))
