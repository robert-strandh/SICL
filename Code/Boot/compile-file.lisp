(cl:in-package #:sicl-boot)

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
  (format *trace-output* "Compiling file ~s~%" relative-pathname)
  (let ((*package* *package*)
        (input-pathname (asdf:system-relative-pathname '#:sicl relative-pathname)))
    (sicl-source-tracking:with-source-tracking-stream-from-file
        (input-stream input-pathname)
      (let ((first-form (eclector.reader:read input-stream nil nil)))
        (assert (and (consp first-form)
                     (eq (first first-form) 'in-package)))
        (setf *package* (find-package (second first-form))))
      (let* ((csts (loop with eof-marker = (list nil)
                         for cst = (eclector.concrete-syntax-tree:read input-stream nil eof-marker)
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
                             (invoke-restart 'cleavir-cst-to-ast:consider-special)))
                         (cleavir-cst-to-ast::encapsulated-condition
                           (lambda (condition)
                             (declare (ignore condition))
                             (invoke-restart 'cleavir-cst-to-ast:signal-original-condition))))
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
          (cleavir-io:write-model output-pathname "V0" ast))))))
