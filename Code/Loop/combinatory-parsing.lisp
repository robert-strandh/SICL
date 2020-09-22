(cl:in-package #:sicl-loop)

;;; A parser is a function that takes a list of tokens to parse, and
;;; that returns three values:
;;;
;;;   * A generalized Boolean indicating whether the parse succeeded.
;;;
;;;   * The result of the parse.  If the parse does not succeed, then
;;;     this value is unspecified.
;;;
;;;   * A list of the tokens that remain after the parse.  If the
;;;     parse does not succeed, then this list contains the original
;;;     list of tokens passed as an argument.

;;; Functions that take one or more parsers as arguments can take
;;; either a function or the name of a function.

(defparameter *indent-level* 0)

(defparameter *parse-trace-p* nil)

(defun parse-trace-output (format-control &rest arguments)
  (when *parse-trace-p*
    (format *trace-output*
            (make-string (* 2 *indent-level*) :initial-element #\Space))
    (apply #'format *trace-output* format-control arguments)))

(defun trace-parser (name parser tokens)
  (let ((*indent-level* (1+ *indent-level*)))
    (parse-trace-output "trying ~s on ~s~%" name tokens)
    (multiple-value-bind (successp result rest)
        (funcall parser tokens)
      (parse-trace-output "~asuccess~%" (if successp "" "no "))
      (values successp result rest))))

(defmacro define-parser (name &body body)
  `(progn
     ;; At compile time, we define a parser that generates an error
     ;; message if invoked.  The reason for doing that is to avoid
     ;; warnings at compile time saying the function does not exist. 
     (eval-when (:compile-toplevel)
       (setf (fdefinition ',name)
             (lambda (tokens)
               (declare (ignore tokens))
               (error "Undefined parser: ~s" ',name))))
     ;; At load time, we set the FDEFINITION of the name to the
     ;; result of executing BODY.
     (eval-when (:load-toplevel :execute)
       (setf (fdefinition ',name)
             (lambda (tokens)
               (trace-parser ',name (progn ,@body) tokens))))))

;;; Take a function designator (called the TRANSFORMER) and a
;;; predicate P and return a parser Q that invokes the predicate on
;;; the first token.  If P returns true then Q succeeds and returns
;;; the result of invoking TRANSFORMER on the token together with the
;;; remaining tokens (all tokens except the first one).  If P returns
;;; false, then Q fails.  If there are no tokens, then Q also fails.
(defun singleton (transformer predicate)
  (lambda (tokens)
    (if (and (not (null tokens))
             (funcall predicate (car tokens)))
        (values t (funcall transformer (car tokens)) (cdr tokens))
        (values nil nil tokens))))

;;; Take a list of parsers P1, P2, ..., Pn and return a parser Q that
;;; invokes Pi in order until one of them succeeds.  If some Pi
;;; succeeds. then Q also succeeds with the same result as Pi.  If
;;; every Pi fails, then Q also fails.
(defun alternative (&rest parsers)
  (lambda (tokens)
    ;; We promised not to use the LOOP macro, so we do this with
    ;; TAGBODY instead. 
    (block nil
      (let ((remaining-parsers parsers))
        (tagbody
         again
           (if (null remaining-parsers)
               (return (values nil nil tokens))
               (multiple-value-bind (successp result rest)
                   (funcall (car remaining-parsers) tokens)
                 (pop remaining-parsers)
                 (if successp
                     (return (values t result rest))
                     (go again)))))))))

;;; Take a function designator (called the COMBINER) and a list of
;;; parsers P1, P2, ..., Pn and return a parser Q that invokes every
;;; Pi in order.  If any Pi fails, then Q fails as well.  If every Pi
;;; succeeds, then Q also succeeds and returns the result of calling
;;; APPLY on COMBINER and the list of results of the invocation of
;;; each Pi.
(defun consecutive (combiner &rest parsers)
  (lambda (tokens)
    ;; We promised not to use the LOOP macro, so we do this with
    ;; TAGBODY instead. 
    (block nil
      (let ((remaining-tokens tokens)
            (remaining-parsers parsers)
            (results '()))
        (tagbody
         again
           (if (null remaining-parsers)
               (return (values t
                               (apply combiner (reverse results))
                               remaining-tokens))
               (multiple-value-bind (successp result rest)
                   (funcall (car remaining-parsers) remaining-tokens)
                 (pop remaining-parsers)
                 (if successp
                     (progn (push result results)
                            (setf remaining-tokens rest)
                            (go again))
                     (return (values nil nil tokens))))))))))

;;; Take a function designator (called the COMBINER) and a parser P
;;; and return a parser Q that invokes P repeatedly until it fails,
;;; each time with the tokens remaining from the previous invocation.
;;; The result of the invocation of Q is the result of calling APPLY
;;; on COMBINER and the list of the results of each invocation of P.
;;; Q always succeeds.  If the first invocation of P fails, then Q
;;; succeeds returning the result of calling APPLY on COMBINER and the
;;; empty list of results, and the original list of tokens as usual.
(defun repeat* (combiner parser)
  (lambda (tokens)
    (let ((remaining-tokens tokens)
          (results '()))
      (block nil
        (tagbody
         again
           (multiple-value-bind (successp result rest)
               (funcall parser remaining-tokens)
             (if successp
                 (progn (push result results)
                        (setf remaining-tokens rest)
                        (go again))
                 (return (values t
                                 (apply combiner (reverse results))
                                 remaining-tokens)))))))))

;;; Take a function designator (called the COMBINER) and a parser P
;;; and return a parser Q that invokes P repeatedly until it fails,
;;; each time with the tokens remaining from the previous invocation.
;;; The result of the invocation of Q is the result of calling APPLY
;;; on COMBINER and the list of the results of each invocation of P.
;;; Q succeeds if and only if at least one invocation of P succeeds.
(defun repeat+ (combiner parser)
  (lambda (tokens)
    (let ((results '()))
      (multiple-value-bind (successp result rest)
          (funcall parser tokens)
        (if (not successp)
            (values nil nil tokens)
            (let ((remaining-tokens rest))
              (push result results)
              (block nil
                (tagbody
                 again 
                   (multiple-value-bind (successp result rest)
                       (funcall parser remaining-tokens)
                     (if successp
                         (progn (push result results)
                                (setf remaining-tokens rest)
                                (go again))
                         (return (values t
                                         (apply combiner (reverse results))
                                         remaining-tokens))))))))))))

;;; Take a default value and a parser P and return a parser Q that
;;; always succeeds.  Q invokes P once.  If P succeeds, then Q
;;; succeeds with the same result as P and with the same remaining
;;; tokens.  If P fails, then Q succeeds, returning the default value
;;; and the original list of tokens.
(defun optional (default parser)
  (lambda (tokens)
    (multiple-value-bind (successp result rest)
        (funcall parser tokens)
      (if successp
          (values t result rest)
          (values t default tokens)))))

;;;  LocalWords:  parsers
