(cl:in-package #:sicl-boot)

(defun import-functions-from-host (names environment)
  (loop with client = (env:client environment)
        for name in names
        do (setf (env:fdefinition client environment name)
                 (fdefinition name))))

(defun define-error-functions (names environment)
  (loop with client = (env:client environment)
        for name in names
        do (setf (env:fdefinition client environment name)
                 (let ((name name))
                   (lambda (&rest arguments)
                     (declare (ignore arguments))
                     (error "Undefined function ~s in environment ~s"
                            name environment))))))

(defun source-relative-to-absolute-pathname (relative-pathname)
  (asdf:system-relative-pathname '#:sicl relative-pathname))

(defun fasl-relative-to-absolute-pathname (relative-pathname)
  (let ((augmented-relative-pathname
          (concatenate 'string "ASTs/" relative-pathname)))
    (asdf:system-relative-pathname
     '#:sicl-boot augmented-relative-pathname)))

(defun source-to-fasl-pathname (pathname)
  (let* ((dot-position (position #\. pathname :from-end t))
         (prefix (subseq pathname 0 dot-position)))
    (concatenate 'string prefix ".fasl")))

(defun read-fasl (relative-pathname)
  (let ((absolute-pathname
          (fasl-relative-to-absolute-pathname relative-pathname)))
    (cleavir-io:read-model absolute-pathname '(v0))))

(defun write-fasl (relative-pathname ast)
  (let ((absolute-pathname
          (fasl-relative-to-absolute-pathname relative-pathname)))
    (cleavir-io:write-model absolute-pathname "V0" ast)))

(defun ast-eval (ast environment)
  (let ((client (env:client environment)))
    (funcall (env:fdefinition client environment 'ast-eval) ast)))

(defun load-fasl (relative-pathname environment)
  (ast-eval (read-fasl relative-pathname) environment))

(defun cst-to-ast (cst environment file-compilation-semantics-p)
  (let ((cleavir-cst-to-ast::*origin* nil)
        (client (env:client environment)))
    (handler-bind
        ((trucler:no-function-description
           (lambda (condition)
             (unless (member (trucler:name condition)
                             (overridden-function-cells
                              (trucler:global-environment client environment))
                             :key #'car :test #'equal)
               (let ((*package* (find-package "KEYWORD")))
                 (warn "Unknown function ~s" (trucler:name condition))))
             (invoke-restart 'cleavir-cst-to-ast:consider-global)))
         (trucler:no-variable-description
           (lambda (condition)
             (let ((*package* (find-package "KEYWORD")))
               (warn "Unknown variable ~s" (trucler:name condition)))
             (invoke-restart 'cleavir-cst-to-ast:consider-special)))
         (cleavir-cst-to-ast::encapsulated-condition
           (lambda (condition)
             (declare (ignore condition))
             (invoke-restart 'cleavir-cst-to-ast:signal-original-condition))))
      (cleavir-cst-to-ast:cst-to-ast
       client cst environment
       :file-compilation-semantics file-compilation-semantics-p))))

(defmethod cleavir-cst-to-ast:cst-eval ((client client) cst environment)
  (let ((ast (cst-to-ast cst environment nil)))
    (funcall (env:fdefinition client environment 'ast-eval) ast)))

(defun load-source-file-common (absolute-pathname environment)
  (let ((*package* *package*))
    (sicl-source-tracking:with-source-tracking-stream-from-file
        (input-stream absolute-pathname)
      (loop with client = (env:client environment)
            with eof-marker = (list nil)
            for cst = (eclector.concrete-syntax-tree:read input-stream nil eof-marker)
            until (eq cst eof-marker)
            do (cleavir-cst-to-ast:cst-eval client cst environment)))))

(defun load-source-file-absolute (absolute-pathname environment)
  (format *trace-output*
          "Loading file ~a into ~a~%"
          absolute-pathname
          (name environment))
  (load-source-file-common absolute-pathname environment))

(defun load-source-file (relative-pathname environment)
  (format *trace-output*
          "Loading file ~a into ~a~%"
          relative-pathname
          (name environment))
  (let ((absolute-pathname
          (source-relative-to-absolute-pathname relative-pathname)))
    (load-source-file-common absolute-pathname environment)))

(defun asdf-system-files (asdf-system-name)
  (labels ((files (component)
             (if (typep component 'asdf/lisp-action:cl-source-file)
                 (list component)
                 (reduce #'append
                         (mapcar #'files (asdf/component:module-components component))))))
    (files (asdf:find-system asdf-system-name))))

(defun asdf-system-absolute-file-names (asdf-system-name)
  (loop for file in (asdf-system-files asdf-system-name)
        collect (namestring (asdf/component:component-pathname file))))

(defun compile-source-file (relative-pathname environment)
  (let ((absolute-pathname
          (source-relative-to-absolute-pathname relative-pathname)))
    (sicl-source-tracking:with-source-tracking-stream-from-file
        (input-stream absolute-pathname)
      (let* ((evaluation-environment
               (make-instance 'env:evaluation-environment
                 :parent environment))
             (compilation-environment
               (make-instance 'env:compilation-environment
                 :parent evaluation-environment))
             (asts (loop with eof-marker = (list nil)
                         for cst = (eclector.concrete-syntax-tree:read
                                    input-stream nil eof-marker)
                         until (eq cst eof-marker)
                         collect (cst-to-ast cst compilation-environment t)))
             (ast (make-instance 'cleavir-ast:progn-ast
                    :form-asts asts))
             (output-relative-pathname
               (source-to-fasl-pathname relative-pathname)))
        (ensure-directories-exist
         (fasl-relative-to-absolute-pathname output-relative-pathname))
        (write-fasl output-relative-pathname ast)))))

(defun copy-macro-functions (from-environment to-environment)
  (let ((table (make-hash-table :test #'eq))
        (client (env:client from-environment)))
    (do-all-symbols (symbol)
      (unless (gethash symbol table)
        (setf (gethash symbol table) t)
        (let ((macro-function
                (env:macro-function client from-environment symbol)))
          (unless (null macro-function)
            (setf (env:macro-function client to-environment symbol)
                  macro-function)))))))
          
(defun host-load (relative-filename)
  (let ((absolute-filename
          (source-relative-to-absolute-pathname relative-filename)))
    (load absolute-filename)))

(defun repl (environment)
  (loop with results = nil
        with client = (env:client environment)
        for ignore = (progn (format *query-io* "~a>> " (package-name *package*))
                            (finish-output *query-io*))
        for form = (eclector.reader:read *query-io*)
        for cst = (cst:cst-from-expression form)
        do (setf results
                 (multiple-value-list 
                  (cleavir-cst-to-ast:cst-eval client cst environment)))
           (loop for result in results
                 do (prin1 result *query-io*)
                    (terpri *query-io*))))

(defmacro with-intercepted-function-cells
    ((environment-form &body clauses) &body body)
  (let ((function-cells-temp-var (gensym))
        (environment-var (gensym)))
    `(let* ((,environment-var ,environment-form)
            (,function-cells-temp-var
              (overridden-function-cells ,environment-var)))
       (setf (overridden-function-cells ,environment-var)
             (list* ,@(loop for (name form) in clauses
                            collect `(cons ',name ,form))
                    ,function-cells-temp-var))
       (unwind-protect (progn ,@body)
         (setf (overridden-function-cells ,environment-var)
               ,function-cells-temp-var)))))

(defun show-asdf-tree (system-name)
  (let ((table (make-hash-table :test #'equalp)))
    (labels ((aux (system-name indentation)
               (unless (or (consp system-name)
                           (gethash system-name table))
                 (setf (gethash system-name table) t)
                 (loop repeat indentation
                       do (format *trace-output* "  "))
                 (format *trace-output* "~s~%" system-name)
                 (loop with system = (asdf:find-system system-name)
                       with children = (asdf:system-depends-on system)
                       for child in children
                       do (aux child (1+ indentation))))))
      (aux system-name 0))))
