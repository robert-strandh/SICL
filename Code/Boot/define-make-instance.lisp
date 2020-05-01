(cl:in-package #:sicl-boot)

(defun define-make-instance (ea eb)
  (flet ((fdef (name env)
           (sicl-genv:fdefinition name env)))
    (setf (sicl-genv:fdefinition 'make-instance eb)
          (lambda (class &rest initargs)
            (let ((class-metaobject
                    (if (symbolp class)
                        (sicl-genv:find-class class ea)
                        class)))
              (unless (funcall (fdef 'sicl-clos:class-finalized-p ea)
                               class-metaobject)
                (funcall (fdef 'sicl-clos:finalize-inheritance ea)
                         class-metaobject))
              (let ((defaulted-initargs initargs))
                (loop with default = (list nil)
                      for (name form thunk)
                        in (funcall (fdef 'sicl-clos:class-default-initargs ea)
                                    class-metaobject)
                      do (when (eq (getf initargs name default) default)
                           (setf defaulted-initargs
                                 (append defaulted-initargs
                                         (list name (funcall thunk))))))
                (let ((instance (apply (fdef 'allocate-instance ea)
                                       class-metaobject
                                       defaulted-initargs)))
                  (apply (fdef 'initialize-instance eb)
                         instance
                         defaulted-initargs))))))))
