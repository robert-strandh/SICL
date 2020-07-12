(cl:in-package #:portable-condition-system)

(defun restart-bind-transform-binding (binding)
  (destructuring-bind (name function . arguments) binding
    `(make-restart :name ',name :function ,function ,@arguments)))

;;; RESTART-CASE - bindings and cases

(defun restart-case-make-restart-binding (temp-var parsed-case)
  (destructuring-bind (name lambda-list keys body tag) parsed-case
    (declare (ignore lambda-list body))
    (let ((lambda-var (gensym "RESTART-ARGS")))
      `(,name
        (lambda (&rest ,lambda-var) (setq ,temp-var ,lambda-var) (go ,tag))
        ,@keys))))

(defun restart-case-make-restart-case (block-tag temp-var parsed-case)
  (destructuring-bind (name lambda-list keys body tag) parsed-case
    (declare (ignore name keys))
    `(,tag
      (return-from ,block-tag (apply (lambda ,lambda-list ,@body) ,temp-var)))))

(defun restart-case-signaling-form-p (expression env)
  (let ((expansion (macroexpand expression env)))
    (and (consp expansion)
         (member (car expansion) '(signal warn error cerror)))))

(defun restart-case-expand-signaling-form (expression env)
  (let ((expansion (macroexpand expression env)))
    (case (car expansion)
      ((signal warn error) (restart-case-expand-non-cerror expansion))
      (cerror (restart-case-expand-cerror expansion)))))

(defun restart-case-parse-case (case)
  (destructuring-bind (name lambda-list . rest) case
    (multiple-value-bind (body report interactive test)
        (restart-case-pop-keywords-from-case rest)
      (let ((tag (gensym (format nil "RESTART-~S-TAG" name)))
            (keywords `(,@(restart-case-make-report-subform report)
                        ,@(restart-case-make-test-subform test)
                        ,@(restart-case-make-interactive-subform interactive))))
        (list name lambda-list keywords body tag)))))
