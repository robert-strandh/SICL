(cl:in-package #:sicl-boot-phase-0)

(defun load-file (relative-filename environment)
  (let ((*package* *package*)
        (filename (asdf:system-relative-pathname '#:sicl relative-filename)))
    (sicl-source-tracking:with-source-tracking-stream-from-file
        (stream filename)
      (let ((first-form (eclector.reader:read stream nil nil)))
        (unless (eq (first first-form) 'in-package)
          (error "File must start with an IN-PACKAGE form."))
        (setf *package* (find-package (second first-form))))
      (loop with client = (make-instance 'trucler-reference:environment)
            with eof-marker = (list nil)
            for cst = (eclector.concrete-syntax-tree:cst-read stream nil eof-marker)
            until (eq cst eof-marker)
            do (sicl-hir-to-cl:cst-eval client cst environment)))))
      
    
