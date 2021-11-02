(cl:in-package #:sicl-conditions)

(defun restart-case-make-report-subform (report)
  (typecase report
    (null '())
    (string `(:report-function (lambda (stream) (write-string ,report stream))))
    (t (list :report-function (list 'function report)))))

(defun restart-case-make-interactive-subform (interactive)
  (typecase interactive
    (null '())
    (t (list :interactive-function (list 'function interactive)))))

(defun restart-case-make-test-subform (test)
  (typecase test
    (null '())
    (t (list :test-function (list 'function test)))))

(defun restart-case-pop-keywords-from-case (case-forms)
  (let ((things case-forms) report interactive test)
    (macrolet ((handle-keyword (symbol keyword)
                 (let ((value (gensym "KEYWORD-VALUE")))
                   `(progn
                      (when ,symbol
                        (error "Duplicate ~S in case ~S." ,keyword case-forms))
                      (pop things)
                      (let ((,value (pop things)))
                        (unless ,value
                          (error "~S may not be NIL in HANDLER-CASE." ,keyword))
                        (setf ,symbol ,value))))))
      (loop
        (let ((thing (first things)))
          (cond
            ((null (rest things))
             (return (values things report interactive test)))
            ((eq thing :report) (handle-keyword report :report))
            ((eq thing :interactive) (handle-keyword interactive :interactive))
            ((eq thing :test) (handle-keyword test :test))
            (t (return (values things report interactive test)))))))))

(defun restart-case-expand-non-cerror (expansion)
  (destructuring-bind (function datum . args) expansion
    (let* ((type (case function
                   (signal 'simple-condition)
                   (warn 'simple-warning)
                   (error 'simple-error)))
           (condition (gensym "CONDITION")))
      `(let ((,condition (coerce-to-condition ,datum (list ,@args)
                                              ',type ',function)))
         (with-condition-restarts ,condition (car *restart-clusters*)
           (,function ,condition))))))

(defun restart-case-expand-cerror (expansion)
  (destructuring-bind (function format-control datum . args) expansion
    (let* ((type 'simple-error)
           (condition (gensym "CONDITION")))
      `(let ((,condition (coerce-to-condition ,datum (list ,@args)
                                              ',type ',function)))
         (with-condition-restarts ,condition (car *restart-clusters*)
           (,function ,format-control ,condition))))))

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
      (let ((tag (gensym))
            (keywords `(,@(restart-case-make-report-subform report)
                        ,@(restart-case-make-test-subform test)
                        ,@(restart-case-make-interactive-subform interactive))))
        (list name lambda-list keywords body tag)))))
