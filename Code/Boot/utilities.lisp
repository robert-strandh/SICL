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
                                      (closure
                                        (funcall (sicl-genv:fdefinition 'make-instance e2)
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
;;; present in some environment En, but tied to a different
;;; environment En-1.  We call such a function a "straddled function".
;;; If bootstrapping involved only two different environments, we
;;; could load the definition of the function into En-1, and then copy
;;; it over to En.  But, since we have several consecutive
;;; environments during bootstrapping, it is possible that the
;;; definition of the function already exists in En-1 and that we need
;;; to preserve it that way.  In that case, we can not simply load the
;;; definition into En-1.  We solve this problem by intercepting any
;;; attempts to set the FDEFINITION of one of a list of function names.

;;; FUNCTION-NAMES is a list of function names need to be prevented
;;; from causing a SETF of FDEFINITION during the execution of BODY.

(defparameter *intercepted-function-names* '())

(defparameter *intercepted-functions* '())

(defmethod (setf sicl-genv:fdefinition) :around
    (new-definition function-name (environment environment))
  (if (member function-name *intercepted-function-names* :test #'equal)
      (setf *intercepted-functions*
            (append *intercepted-functions* (list new-definition)))
      (call-next-method)))

(defmacro with-straddled-function-definitions
    ((function-names env) &body body)
  `(let ((*intercepted-function-names* ',function-names)
         (*intercepted-functions* '()))
     ,@body
     (setf *intercepted-function-names* '())
     (loop for name in ',function-names
           for function in *intercepted-functions*
           do (setf (sicl-genv:fdefinition name ,env) function))))
