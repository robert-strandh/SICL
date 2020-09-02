(cl:in-package #:sicl-conditions)

;;;; FIXME: We need to do a lot more syntax verification here.

(defun expand-define-condition-report-function (report-option)
  (let* ((condition (gensym "CONDITION"))
         (stream (gensym "STREAM"))
         (report (second report-option)))
    `(lambda (,condition ,stream)
       ,(if (stringp report)
            `(write-string ,report ,stream)
            `(,report ,condition ,stream)))))

(defun define-condition-expander (name supertypes direct-slots options)
  (let* ((report-option (find :report options :key #'car))
         (other-options (remove report-option options))
         (default-initargs-option (find :default-initargs options :key #'car))
         (other-options (remove default-initargs-option other-options))
         (supertypes supertypes))
    (when (null supertypes)
      (push 'condition supertypes))
    (unless (null report-option)
      (push 'report-mixin supertypes))
    `(progn (defclass ,name ,supertypes
              ,direct-slots
              (:metaclass condition-class)
              ,@(if (null report-option)
                    '()
                    `((:default-initargs
                       :report
                       ,(expand-define-condition-report-function report-option)
                       ,@(rest default-initargs-option))))
              ,@other-options)
            ',name)))
