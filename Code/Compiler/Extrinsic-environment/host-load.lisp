(cl:in-package #:sicl-extrinsic-environment)

(defun host-load (relative-filename)
  (let ((*package* *package*)
        (filename (asdf:system-relative-pathname :sicl-extrinsic-environment
                                                 relative-filename)))
    (with-open-file (stream filename :direction :input)
      (loop with eof-marker = (list nil)
            for form = (eclector.reader:read stream nil eof-marker)
            until (eq form eof-marker)
            do (eval form)))))
