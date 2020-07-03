;;;; src/restarts.lisp

(in-package #:portable-condition-system)

;;; Restart definition

(defstruct restart
  "A restart structure, implementing the ANSI CL system class RESTART."
  (name (error "NAME required."))
  (function (error "FUNCTION required."))
  (report-function nil)
  (interactive-function nil)
  (test-function (constantly t))
  (associated-conditions '()))

(defmethod print-object :around ((restart restart) stream)
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t)
        (prin1 (restart-name restart) stream))
      (call-next-method)))

(defmethod print-object ((restart restart) stream)
  (cond ((restart-report-function restart)
         (funcall (restart-report-function restart) stream))
        ((restart-name restart)
         (format stream "Invoke restart ~A." (restart-name restart)))
        (t
         (format stream "Invoke the anonymous restart ~S." restart))))

;;; Restart visibility and association

(defun restart-visible-p (restart condition)
  ;; FIXME: A call to COMPUTE-RESTARTS -- from an error,
  ;; from user code, whatever -- inside the test
  ;; function would cause infinite recursion here, so
  ;; we disable each restart using
  ;; *restart-test-stack* for the duration of the
  ;; test call.
  (and (funcall (restart-test-function restart) condition)
       (or (null condition)
           (let ((associated-conditions (restart-associated-conditions restart)))
             (or (null associated-conditions)
                 (member condition associated-conditions))))))

(defmacro with-condition-restarts (condition (&rest restarts) &body body)
  (let ((condition-var (gensym "CONDITION"))
        (restarts-var (gensym "RESTARTS"))
        (restart (gensym "RESTART")))
    `(let ((,condition-var ,condition)
           (,restarts-var ,restarts))
       (unwind-protect
            (progn
              (dolist (,restart ,restarts-var)
                (push ,condition-var (restart-associated-conditions ,restart)))
              ,@body)
         (dolist (,restart ,restarts-var)
           (pop (restart-associated-conditions ,restart)))))))

;;; Restart functions

(defvar *restart-clusters* '())

(defgeneric compute-restarts (&optional condition))

(defmethod compute-restarts (&optional condition)
  (loop for restart in (apply #'append *restart-clusters*)
        when (restart-visible-p restart condition)
          collect restart))

(defgeneric find-restart (name &optional condition))

(defmethod find-restart (name &optional condition)
  (dolist (cluster *restart-clusters*)
    (dolist (restart cluster)
      (when (and (or (eq restart name)
                     (eq (restart-name restart) name))
                 (restart-visible-p restart condition))
        (return-from find-restart restart)))))

(defgeneric invoke-restart (restart &rest arguments))

(defmethod invoke-restart (restart &rest arguments)
  (declare (ignore arguments))
  (error "Wrong thing passed to INVOKE-RESTART: ~S" restart))

(defmethod invoke-restart ((restart symbol) &rest arguments)
  (let ((real-restart (or (find-restart restart)
                          (error "Restart ~S is not active." restart))))
    (apply #'invoke-restart real-restart arguments)))

(defmethod invoke-restart ((restart restart) &rest arguments)
  (apply (restart-function restart) arguments))

(defgeneric invoke-restart-interactively (restart))

(defmethod invoke-restart-interactively (restart)
  (error "Wrong thing passed to INVOKE-RESTART-INTERACTIVELY: ~S" restart))

(defmethod invoke-restart-interactively ((restart symbol))
  (let ((real-restart (or (find-restart restart)
                          (error "Restart ~S is not active." restart))))
    (invoke-restart-interactively real-restart)))

(defmethod invoke-restart-interactively ((restart restart))
  (let* ((function (restart-interactive-function restart))
         (arguments (if function (funcall function) '())))
    (apply (restart-function restart) arguments)))

;;; RESTART-BIND

(defun restart-bind-transform-binding (binding)
  (destructuring-bind (name function . arguments) binding
    `(make-restart :name ',name :function ,function ,@arguments)))

(defmacro restart-bind (bindings &body body)
  (let ((cluster (mapcar #'restart-bind-transform-binding bindings)))
    `(let ((*restart-clusters* (cons (list ,@cluster) *restart-clusters*)))
       ,@body)))

;;; RESTART-CASE - keyword parsing

(defun restart-case-make-report-subform (report)
  (typecase report
    (null '())
    (string `(:report-function (lambda (stream) (write-string ,report stream))))
    (t `(:report-function #',report))))

(defun restart-case-make-interactive-subform (interactive)
  (typecase interactive
    (null '())
    (t `(:interactive-function #',interactive))))

(defun restart-case-make-test-subform (test)
  (typecase test
    (null '())
    (t `(:test-function #',test))))

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

(defun restart-case-parse-case (case)
  (destructuring-bind (name lambda-list . rest) case
    (multiple-value-bind (body report interactive test)
        (restart-case-pop-keywords-from-case rest)
      (let ((tag (gensym (format nil "RESTART-~S-TAG" name)))
            (keywords `(,@(restart-case-make-report-subform report)
                        ,@(restart-case-make-test-subform test)
                        ,@(restart-case-make-interactive-subform interactive))))
        (list name lambda-list keywords body tag)))))

;;; RESTART-CASE - associating conditions

(defun restart-case-signaling-form-p (expression env)
  (let ((expansion (macroexpand expression env)))
    (and (consp expansion)
         (member (car expansion) '(signal warn error cerror)))))

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

(defun restart-case-expand-signaling-form (expression env)
  (let ((expansion (macroexpand expression env)))
    (case (car expansion)
      ((signal warn error) (restart-case-expand-non-cerror expansion))
      (cerror (restart-case-expand-cerror expansion)))))

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

;;; RESTART-CASE - main macro body

(defmacro restart-case (expression &body cases &environment env)
  (let ((block-name (gensym "RESTART-CASE-BLOCK"))
        (temp-var (gensym "RESTART-CASE-VAR"))
        (data (mapcar #'restart-case-parse-case cases)))
    (flet ((make-restart-binding (datum)
             (restart-case-make-restart-binding temp-var datum))
           (make-restart-case (datum)
             (restart-case-make-restart-case block-name temp-var datum)))
      `(let ((,temp-var nil))
         (declare (ignorable ,temp-var))
         (block ,block-name
           (tagbody
              (restart-bind ,(mapcar #'make-restart-binding data)
                (return-from ,block-name
                  ,(if (restart-case-signaling-form-p expression env)
                       (restart-case-expand-signaling-form expression env)
                       expression)))
              ,@(apply #'append (mapcar #'make-restart-case data))))))))

;;; WITH-SIMPLE-RESTART

(defmacro with-simple-restart ((name format-control &rest args) &body forms)
  (let ((stream (gensym "STREAM")))
    `(restart-case ,(if (= 1 (length forms)) (car forms) `(progn ,@forms))
       (,name ()
         :report (lambda (,stream) (format ,stream ,format-control ,@args))
         (values nil t)))))

;;; System-defined restarts

(defun maybe-invoke-restart (restart-name &optional condition errorp &rest arguments)
  (let ((restart (find-restart restart-name condition)))
    (cond (restart (apply #'invoke-restart restart arguments))
          (errorp (error 'restart-not-found :restart-name restart-name)))))

(defun abort (&optional condition)
  (maybe-invoke-restart 'abort condition t)
  (error 'abort-failure))

(defun continue (&optional condition)
  (maybe-invoke-restart 'continue condition))

(defun muffle-warning (&optional condition)
  (maybe-invoke-restart 'muffle-warning condition t))

(defun store-value (value &optional condition)
  (maybe-invoke-restart 'store-value condition nil value))

(defun use-value (value &optional condition)
  (maybe-invoke-restart 'use-value condition nil value))
