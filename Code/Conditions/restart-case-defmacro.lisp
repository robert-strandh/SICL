(cl:in-package #:sicl-conditions)

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
