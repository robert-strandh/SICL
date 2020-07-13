(cl:in-package #:portable-condition-system)

;;; HANDLER-CASE - :NO-ERROR present

(defun make-handler-case-with-no-error-case (form cases)
  (let* ((no-error-case (assoc :no-error cases))
         (other-cases (remove no-error-case cases)))
    (let ((normal-return (gensym "NORMAL-RETURN"))
          (error-return  (gensym "ERROR-RETURN")))
      `(block ,error-return
         (multiple-value-call (lambda ,@(cdr no-error-case))
           (block ,normal-return
             (return-from ,error-return
               (handler-case (return-from ,normal-return ,form)
                 ,@other-cases))))))))

;;; HANDLER-CASE - :NO-ERROR not present

(defun handler-case-parse-case (datum)
  (destructuring-bind (type lambda-list . forms) datum
    (let ((tag (gensym "HANDLER-TAG")))
      (list tag type lambda-list forms))))

(defun handler-case-make-handler-binding (temp-var datum)
  (destructuring-bind (tag type lambda-list forms) datum
    (declare (ignore forms))
    (let ((condition (gensym "CONDITION")))
      `(,type (lambda (,condition)
                (declare (ignorable ,condition))
                ,@(when lambda-list `((setf ,temp-var ,condition)))
                (go ,tag))))))

(defun handler-case-make-handler-case (block-name temp-var datum)
  (destructuring-bind (tag type lambda-list body) datum
    (declare (ignore type))
    `(,tag
      (return-from ,block-name
        ,(if lambda-list
             `(let ((,(first lambda-list) ,temp-var)) ,@body)
             `(locally ,@body))))))

(defun make-handler-case-without-no-error-case (form cases)
  (let ((block-name (gensym "HANDLER-CASE-BLOCK"))
        (temp-var (gensym "HANDLER-CASE-VAR"))
        (data (mapcar #'handler-case-parse-case cases)))
    (flet ((make-handler-binding (datum)
             (handler-case-make-handler-binding temp-var datum))
           (make-handler-case (datum)
             (handler-case-make-handler-case block-name temp-var datum)))
      `(let ((,temp-var nil))
         (declare (ignorable ,temp-var))
         (block ,block-name
           (tagbody
              (handler-bind ,(mapcar #'make-handler-binding data)
                (return-from ,block-name ,form))
              ,@(apply #'append (mapcar #'make-handler-case data))))))))
