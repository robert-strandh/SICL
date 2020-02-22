(cl:in-package #:sicl-boot)

(defun load-fasl (relative-pathname global-environment)
  (format *trace-output* "Loading file ~s~%" relative-pathname)
  (let* ((client (make-instance 'client))
         (prefixed (concatenate 'string "ASTs/" relative-pathname))
         (pathname (asdf:system-relative-pathname '#:sicl-boot prefixed))
         (ast (cleavir-io:read-model pathname '(v0)))
         (hir (sicl-ast-to-hir:ast-to-hir client ast))
         (hir2 (sicl-ast-to-hir:ast-to-hir client ast))
         (fun (sicl-hir-interpreter:top-level-hir-to-host-function client hir))
         (sicl-run-time:*dynamic-environment* '()))
    (sicl-hir-to-mir:hir-to-mir client hir2)
    (funcall fun
             (sicl-hir-interpreter:make-function-cell-finder global-environment)
             (apply #'vector
                    nil ; Ultimately, replace with code object.
                    #'sicl-hir-interpreter:enclose
                    #'cons
                    nil
                    (sicl-hir-transformations:constants hir)))))

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
