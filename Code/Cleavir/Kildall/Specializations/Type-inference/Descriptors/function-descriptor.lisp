(cl:in-package #:cleavir-type-descriptors)

;;;; Function descriptors represent type information about
;;;; functions. These can be used to check arguments to non-literal
;;;; function calls, insert more checks, remove control paths, etc
;;;; But right now function descriptors will just be the return
;;;; values; this is the basic level needed for inference.

;;; imprecise but good enough
(deftype function-descriptor ()
  '(cons (eql function)))

(defun make-function-descriptor (lambda-list return-values)
  (declare (ignore lambda-list)) ; TODO
  `(function * ,return-values))

(defun function-lambda-list (function-descriptor)
  (second function-descriptor))
(defun function-return-values (function-descriptor)
  (third function-descriptor))

;; convenience
(defun return-values (descriptor)
  (if (function-descriptor-p descriptor)
      (function-return-values descriptor)
      (values-top)))

(defun function-bottom ()
  (make-function-descriptor '* (values-bottom)))
(defun function-top ()
  (make-function-descriptor '* (values-top)))

(defun function-descriptor->type (function-descriptor)
  `(function ,(function-lambda-list function-descriptor)
             ,(values-descriptor->type
               (function-return-values function-descriptor))))

(defun function-binary-meet (f1 f2 env)
  (let ((meet (values-binary-meet (function-return-values f1)
                                  (function-return-values f2)
                                  env)))
    (make-function-descriptor '* meet)))

(defun function-binary-join (f1 f2 env)
  (let ((join (values-binary-join (function-return-values f1)
                                  (function-return-values f2)
                                  env)))
    (make-function-descriptor '* join)))

(defun sub-function-p (f1 f2 env)
  (sub-values-p (function-return-values f1)
                (function-return-values f2)
                env))

(defun function-descriptor-p (descriptor)
  (and (consp descriptor) (eq (car descriptor) 'function)))
