;;;; src/restarts.lisp

(in-package #:portable-condition-system)

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
