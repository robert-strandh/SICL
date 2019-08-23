(cl:in-package #:sicl-boot-phase-0)

(defun read-model-object (stream char)
  (declare (ignore char))
  (apply #'make-instance (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-model-object nil)
(set-syntax-from-char #\] #\))

(defmethod make-load-form ((object sicl-source-tracking:source-position) &optional environment)
  (declare (ignore environment))
  `(make-instance 'sicl-source-tracking:source-position
     :lines ,(sicl-source-tracking:lines object)
     :line-index ,(sicl-source-tracking:line-index object)
     :character-index ,(sicl-source-tracking:character-index object)))

(defun compile-file (client relative-pathname environment)
  (let ((*package* *package*)
        (input-pathname (asdf:system-relative-pathname '#:sicl relative-pathname)))
    (sicl-source-tracking:with-source-tracking-stream-from-file
        (input-stream input-pathname)
      (let ((first-form (eclector.reader:read input-stream nil nil)))
        (assert (and (consp first-form)
                     (eq (first first-form) 'in-package)))
        (setf *package* (find-package (second first-form))))
      (let* ((csts (loop with eof-marker = (list nil)
                         for cst = (eclector.concrete-syntax-tree:cst-read input-stream nil eof-marker)
                         until (eq cst eof-marker)
                         collect cst))
             (cst (cst:cons (cst:cst-from-expression 'progn)
                            (apply #'cst:list csts)))
             (ast (let ((cleavir-cst-to-ast::*origin* nil))
                    (handler-bind
                        ((trucler:no-function-description
                           (lambda (condition)
                             (declare (ignore condition))
                             (invoke-restart 'cleavir-cst-to-ast:consider-global)))
                         (trucler:no-variable-description
                           (lambda (condition)
                             (declare (ignore condition))
                             (invoke-restart 'cleavir-cst-to-ast:consider-special))))
                      (cleavir-cst-to-ast:cst-to-ast
                       client cst environment
                       :file-compilation-semantics t)))))
        (let* ((dot-pos (position #\. relative-pathname))
               (prefix (subseq relative-pathname 0 dot-pos))
               (filename (concatenate 'string prefix ".fasl"))
               (output-relative-pathname (concatenate 'string
                                                      "Boot/ASTs/"
                                                      filename))
               (output-pathname (asdf:system-relative-pathname '#:sicl output-relative-pathname)))
          (ensure-directories-exist output-pathname)
          (cleavir-io:write-model output-pathname "V0" ast))
        (let* ((dot-pos (position #\. relative-pathname))
               (prefix (subseq relative-pathname 0 dot-pos))
               (filename (concatenate 'string prefix ".lisp"))
               (fasl-filename (concatenate 'string prefix ".fasl"))
               (output-relative-pathname (concatenate 'string
                                                      "Boot/Host-FASLs/"
                                                      filename))
               (fasl-relative-pathname (concatenate 'string
                                                    "Boot/Host-FASLs/"
                                                    fasl-filename))
               (output-pathname (asdf:system-relative-pathname '#:sicl output-relative-pathname))
               (fasl-pathname (asdf:system-relative-pathname '#:sicl fasl-relative-pathname)))
          (ensure-directories-exist output-pathname)
          (let* ((hir (sicl-ast-to-hir:ast-to-hir ast))
                 (cl (sicl-hir-to-cl:hir-to-cl nil hir))
                 (wrapped-cl `(progn (defparameter sicl-boot::*top-level-function*
                                       ,cl)
                                     (defparameter sicl-boot::*constants*
                                       ',(sicl-hir-transformations:constants hir)))))
            (unless (and (probe-file fasl-pathname)
                         (> (file-write-date fasl-pathname)
                            (file-write-date input-pathname)))
              (with-open-file (stream output-pathname
                                      :direction :output
                                      :if-exists :supersede)
                (cleavir-io:write-model output-pathname nil wrapped-cl)
                (cl:compile-file output-pathname)))))))))
