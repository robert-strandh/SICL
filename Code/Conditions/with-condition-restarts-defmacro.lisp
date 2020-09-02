(cl:in-package #:sicl-conditions)

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
