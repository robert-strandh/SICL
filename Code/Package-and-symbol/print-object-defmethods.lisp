(cl:in-package #:sicl-package)

(defmethod print-object ((object package) stream)
  (print-unreadable-object (object stream :type t)
    (package-name object)))

(defmethod print-object ((object symbol) stream)
  (let ((symbol-package (symbol-package object)))
    (cond ((null symbol-package)
           (format stream "#:"))
          ((eq symbol-package (find-package '#:keyword))
           (format stream ":"))
          ((eq symbol-package *package*)
           nil)
          (t
           ;; This is not quite correct.  The package name can contain
           ;; characters that need to be escaped.
           (format stream "~a:" (package-name symbol-package)))))
  (format stream (symbol-name object)))
           
