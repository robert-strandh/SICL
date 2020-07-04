;;;; src/conditions.lisp

(in-package #:portable-condition-system)

;;; Class CONDITION

(defclass condition () ())

(defmethod print-object ((condition condition) stream)
  (format stream "Condition ~S was signaled." (type-of condition)))

(defmethod print-object :around ((condition condition) stream)
  (if *print-escape*
      (print-unreadable-object (condition stream :type t :identity t))
      (call-next-method)))

;;; DEFINE-CONDITION

(defun make-condition (type &rest args)
  (apply #'make-instance type args))

(defun expand-define-condition-report-method (name report-option)
  (let* ((condition (gensym "CONDITION"))
         (stream (gensym "STREAM"))
         (report (second report-option))
         (report-form (if (stringp report)
                          `(write-string ,report ,stream)
                          `(funcall #',report ,condition ,stream))))
    `(defmethod print-object ((,condition ,name) ,stream)
       ,report-form)))

(defun expand-define-condition-remove-report-method (name)
  (let ((method (gensym "CONDITION")))
    `(let ((,method (find-method #'print-object '() '(,name t) nil)))
       (when ,method (remove-method #'print-object ,method)))))

(defun expand-define-condition (name supertypes direct-slots options)
  (let* ((report-option (find :report options :key #'car))
         (other-options (remove report-option options))
         (supertypes (or supertypes '(condition))))
    `(progn (defclass ,name ,supertypes ,direct-slots ,@other-options)
            ,@(if report-option
                  `(,(expand-define-condition-report-method name report-option))
                  `(,(expand-define-condition-remove-report-method name)))
            ',name)))

(defmacro define-condition (name (&rest supertypes) direct-slots &rest options)
  (expand-define-condition name supertypes direct-slots options))
