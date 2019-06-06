(cl:in-package #:sicl-boot)

(defun load-fasl (relative-pathname environment)
  (format *trace-output* "Loading file ~s~%" relative-pathname)
  (let* ((prefixed (concatenate 'string "ASTs/" relative-pathname))
         (pathname (asdf:system-relative-pathname '#:sicl-new-boot prefixed))
         (ast (cleavir-io:read-model pathname '(v0)))
         (hir (sicl-ast-to-hir:ast-to-hir ast))
         (cl (sicl-hir-to-cl:hir-to-cl nil hir))
         (fun (compile nil cl))
         (sicl-hir-to-cl:*dynamic-environment* '()))
    (funcall fun (sicl-hir-to-cl:function-finder environment))))

(defun import-function-from-host (name environment)
  (setf (sicl-genv:fdefinition name environment)
        (fdefinition name)))

(defun import-functions-from-host (names environment)
  (loop for name in names
        do (import-function-from-host name environment)))

(defun import-package-from-host (name environment)
  (push (find-package name)
        (sicl-genv:packages environment)))

(defun import-class-from-host (name environment)
  (setf (sicl-genv:find-class name environment)
        (find-class name)))
