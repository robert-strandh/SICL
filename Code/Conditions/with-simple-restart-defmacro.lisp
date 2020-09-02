(cl:in-package #:sicl-conditions)

(defmacro with-simple-restart ((name format-control &rest args) &body forms)
  (let ((stream (gensym "STREAM")))
    `(restart-case ,(if (= 1 (length forms)) (car forms) `(progn ,@forms))
       (,name ()
         :report (lambda (,stream) (format ,stream ,format-control ,@args))
         (values nil t)))))
