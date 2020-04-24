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

;;; During bootstrapping, it is common that some function needs to be
;;; present in some environment En, but present in a different
;;; environment En-1.  We call such a function a "straddled function".
;;; If bootstrapping involved only two different environments, we
;;; could load the definition of the function in En-1, and then copy
;;; it over to En.  But, since we have several consecutive
;;; environments during bootstrapping, it is possible that the
;;; definition of the function already exists in En-1 and that we need
;;; to preserve it that way.  In that case, we can not simply load the
;;; definition into En-1.  We must first save the existing definition
;;; (if there is one) and restore it after the new one has been copied
;;; over to En.  This macro addresses that problem.
;;;
;;; FUNCTION-NAMES is a list of function names that need to be saved
;;; from ENV1 before the BODY is executed and restored afterward.  It
;;; is assumed that the BODY defines the names in FUNCTION-NAMES in
;;; ENV1.  So after the BODY has been evaluated, we copy the function
;;; definitions of the names in FUNCTION-NAMES from ENV1 to ENV2.  If
;;; some name was not defined in ENV1, we obviously do not attempt to
;;; save the definition.  Instead, after the definition has been
;;; copied, we make the name unbound in ENV1.
(defmacro with-straddled-function-definitions
    ((function-names env1 env2) &body body)
  (let ((name-temps (loop for name in function-names collect (gensym))))
    `(let ,(loop for function-name in function-names
                 for name-temp in name-temps
                 collect `(,name-temp
                           (if (sicl-genv:fboundp ',function-name ,env1)
                               (sicl-genv:fdefinition ',function-name ,env1)
                               nil)))
       ,@(loop for function-name in function-names
               collect `(sicl-genv:fmakunbound ',function-name ,env1))
       ,@body
       ,@(loop for function-name in function-names
               collect `(setf (sicl-genv:fdefinition ',function-name ,env2)
                              (sicl-genv:fdefinition ',function-name ,env1)))
       ,@(loop for function-name in function-names
               for name-temp in name-temps
               collect `(if (null ,name-temp)
                            (sicl-genv:fmakunbound ',function-name ,env1)
                            (setf (sicl-genv:fdefinition ',function-name ,env1)
                                  ,name-temp))))))
