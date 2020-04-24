(cl:in-package #:sicl-boot)

(defun tie-code-object (client code-object e1 e2)
  (let* ((hir (sicl-compiler:hir code-object))
         (constants (sicl-compiler:constants code-object))
         (function-names (sicl-compiler:function-names code-object))
         (fun (sicl-hir-interpreter:top-level-hir-to-host-function client hir))
         (sicl-run-time:*dynamic-environment* '()))
    (funcall fun
             (apply #'vector
                    (funcall (sicl-genv:fdefinition 'make-instance e2)
                             'sicl-compiler:code-object
                             :instructions (sicl-compiler:instructions code-object)
                             :frame-maps nil
                             :callee-saves-register-maps nil
                             :callee-saves-stack-maps nil)
                    (labels ((enclose (entry-point code-object &rest static-environment-values)
                               (let* ((static-environment
                                        (apply #'vector
                                               code-object
                                               #'enclose
                                               #'cons
                                               nil
                                               static-environment-values))
                                      (closure (funcall (sicl-genv:fdefinition 'make-instance e2)
                                                        'sicl-clos:simple-function
                                                        :environment static-environment)))
                                 (closer-mop:set-funcallable-instance-function
                                  closure
                                  (lambda (&rest args)
                                    (funcall entry-point
                                             args
                                             static-environment
                                             sicl-run-time:*dynamic-environment*)))
                                 closure)))
                      #'enclose)
                    #'cons
                    nil
                    (append (loop for name in function-names
                                  collect (sicl-genv:function-cell name e1))
                            constants)))))

(defun load-fasl (relative-pathname global-environment)
  (format *trace-output* "Loading file ~s~%" relative-pathname)
  (let* ((client (make-instance 'client))
         (prefixed (concatenate 'string "ASTs/" relative-pathname))
         (pathname (asdf:system-relative-pathname '#:sicl-boot prefixed))
         (ast (cleavir-io:read-model pathname '(v0)))
         (hir (sicl-ast-to-hir:ast-to-hir client ast))
         ;; (hir2 (sicl-ast-to-hir:ast-to-hir client ast))
         (fun (sicl-hir-interpreter:top-level-hir-to-host-function client hir))
         (sicl-run-time:*dynamic-environment* '()))
    ;; (sicl-hir-to-mir:hir-to-mir client hir2)
    (funcall fun
             (apply #'vector
                    nil ; Ultimately, replace with code object.
                    #'sicl-hir-interpreter:enclose
                    #'cons
                    nil
                    (append (loop with names = (sicl-hir-transformations:function-names hir)
                                  for name in names
                                  collect (sicl-genv:function-cell name global-environment))
                            (sicl-hir-transformations:constants hir))))))

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

(defmacro with-straddled-function-definition
    ((function-name env1 env2) &body body)
  (let ((name-temp (gensym))
        (local-function-temp (gensym)))
    `(flet ((,local-function-temp ()
              ,@body
              (setf (sicl-genv:fdefinition ',function-name ,env2)
                    (sicl-genv:fdefinition ',function-name ,env1))))
       (if (sicl-genv:fboundp ',function-name ,env1)
           (let ((,name-temp (sicl-genv:fdefinition ',function-name ,env1)))
             (,local-function-temp)
             (setf (sicl-genv:fdefinition ',function-name ,env1) ,name-temp))
           (progn (,local-function-temp)
                  (sicl-genv:fmakunbound ',function-name ,env1))))))
