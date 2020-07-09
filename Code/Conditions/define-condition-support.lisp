(cl:in-package #:sicl-conditions)

;;;; FIXME: We need to do a lot more syntax verification here.

(defun expand-define-condition-report-method (name report-option)
  (let* ((condition (gensym "CONDITION"))
         (stream (gensym "STREAM"))
         (report (second report-option))
         (report-form (if (stringp report)
                          `(write-string ,report ,stream)
                          `(,report ,condition ,stream))))
    `(defmethod print-object ((,condition ,name) ,stream)
       ,report-form)))

(defun expand-define-condition-remove-report-method (name)
  (let ((method (gensym "CONDITION")))
    `(let ((,method (find-method #'print-object '()
                                 (list (find-class ',name)
                                       (find-class 't))
                                 nil)))
       (unless (null ,method)
         (remove-method #'print-object ,method)))))

(defun define-condition-expander (name supertypes direct-slots options)
  (let* ((report-option (find :report options :key #'car))
         (other-options (remove report-option options))
         (supertypes (or supertypes '(condition))))
    `(progn (defclass ,name ,supertypes
              ,direct-slots
              (:metaclass condition-class)
              ,@other-options)
            ,@(if (null report-option)
                  `(,(expand-define-condition-remove-report-method name))
                  `(,(expand-define-condition-report-method name report-option)))
            ',name)))
