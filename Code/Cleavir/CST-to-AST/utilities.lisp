(cl:in-package #:cleavir-cst-to-ast)

(defun symbol-macro-expander (expansion)
  (lambda (form env)
    (declare (ignore form env))
    expansion))

(defun expand (expander form env)
  (funcall (coerce *macroexpand-hook* 'function)
           expander form env))

;;; This variable is bound to a list of forms that will be
;;; searched for in the CST so that a source position can
;;; be found.
;;; It's a dynamic variable because a macroexpander may
;;; want to bind it; see WITH-CURRENT-SOURCE-FORM.
(defvar *current-source-forms*)

(defun find-source-cst-1 (cst form)
  (let ((seen (make-hash-table :test #'eq)))
    (labels ((aux (cst)
               (unless (gethash cst seen)
                 (setf (gethash cst seen) t)
                 (when (eq (cst:raw cst) form)
                   (return-from find-source-cst-1 cst))
                 ;; We don't search through atoms.
                 (when (typep cst 'cst:cons-cst)
                   (aux (cst:first cst))
                   (aux (cst:rest cst)))
                 nil)))
      (aux cst))))

;;; Used in the encapsulators in conditions.lisp.
(defun find-source-cst (cst
                        &optional (forms *current-source-forms*))
  (if (null forms)
      cst
      (or (find-source-cst-1 cst (first forms))
          (find-source-cst cst (rest forms)))))

;;; This is a helper operator to get more accurate errors from
;;; macroexpansion functions. Cleavir wraps such errors in
;;; other conditions that include the CST, which has source
;;; information, using with-encapsulated-conditions. This
;;; operator can be used to specify which CST should be
;;; included in that encapsulation condition.
;;; The "current CST" is initially bound to the whole macro form's
;;; CST by Cleavir. In a with-current-source-form, it will be
;;; rebound to the CST corresponding to the first of the FORMS
;;; that can be located in the current CST, or to the current CST
;;; again if none can be found. Then, when an error is signaled
;;; and Cleavir encapsulates it, it uses the current CST.
;;; This is useful for macros like COND and SETF. For example,
;;; (with-current-source-form (place) (get-setf-expansion place))
;;; means that if the place is malformed etc., the error location
;;; is localized to the place, not the entire SETF form.
;;; If WITH-CURRENT-SOURCE-FORM is executed in some context other
;;; than a macroexpander in Cleavir, no special processing is done.
(defmacro with-current-source-form ((&rest forms) &body body)
  ;; This circuitious expansion is to ensure that code using this
  ;; macro can be loaded even if the compiler has not been yet,
  ;; for bootstrapping purposes or otherwise.
  (let ((thunkg (gensym "THUNK")))
    ;; progn to ensure DECLARE doesn't work
    `(flet ((,thunkg () (progn ,@body)))
       (if (boundp '*current-source-forms*)
           (let ((*current-source-forms*
                  (list* ,@forms *current-source-forms*)))
             (,thunkg))
           (,thunkg)))))

(defun expand-macro (expander cst env)
  (with-encapsulated-conditions
      (cst macroexpansion-error
           macroexpansion-warning
           macroexpansion-style-warning)
    (expand expander (cst:raw cst) env)))

(defun expand-compiler-macro (expander cst env)
  (let ((form (cst:raw cst)))
    (restart-case
        (with-encapsulated-conditions
            (cst compiler-macro-expansion-error
                 compiler-macro-expansion-warning
                 compiler-macro-expansion-style-warning)
          (expand expander form env))
      (continue ()
        :report "Ignore compiler macro."
        (return-from expand-compiler-macro form)))))

(defun cst-eval (cst environment system)
  (with-encapsulated-conditions
      (cst eval-error eval-warning eval-style-warning)
    (cleavir-env:cst-eval cst environment environment system)))

(defun make-atom-cst (object &optional origin)
  (make-instance 'cst:atom-cst :raw object :source origin))

;;; Take a CST, check whether it represents a proper list.  If it does
;;; not represent ERROR-TYPE is a symbol that is passed to ERROR.
(defun check-cst-proper-list (cst error-type &rest more-initargs)
  (unless (cst:proper-list-p cst)
    (apply #'error error-type :cst cst more-initargs)))

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
      (error 'incorrect-number-of-arguments-error
             :cst cst
             :expected-min min
             :expected-max max
             :observed count))))

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
