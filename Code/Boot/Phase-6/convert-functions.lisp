(cl:in-package #:sicl-boot-phase-6)

(defun wrap-function (function make-instance)
  (let ((wrapper (funcall make-instance 'sicl-clos:simple-function)))
    (setf (sicl-boot:original-function wrapper) function)
    (sicl-host-mop:set-funcallable-instance-function wrapper function)
    wrapper))

;;; FIXME: Figure out a better way to write this function.
(defun host-function-p (name client e5)
  (if (symbolp name)
      (if (and (env:fboundp client e5 name)
               (null (env:special-operator client e5 name))
               (null (env:macro-function client e5 name)))
          (let ((result (env:fdefinition client e5 name)))
            (if (typep result 'sicl-boot:header)
                nil
                result))
          nil)
      (if (env:fboundp client e5 name)
          (let ((result (env:fdefinition client e5 name)))
            (if (typep result 'sicl-boot:header)
                nil
                result))
          nil)))

(defun convert-functions (e5)
  (let* ((client (env:client e5))
         (make-instance (env:fdefinition client e5 'make-instance)))
    (do-all-symbols (symbol)
      (let ((maybe-function (host-function-p symbol client e5)))
        (unless (null maybe-function)
          (setf (env:fdefinition client e5 symbol)
                (wrap-function maybe-function make-instance))))
      (let ((maybe-function (host-function-p `(setf ,symbol) client e5)))
        (unless (null maybe-function)
          (setf (env:fdefinition client e5 `(setf ,symbol))
                (wrap-function maybe-function make-instance)))))))

