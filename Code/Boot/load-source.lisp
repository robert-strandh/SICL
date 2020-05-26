(cl:in-package #:sicl-boot)

(defun load-source (relative-pathname environment)
  (let* ((dot-pos (position #\. relative-pathname))
         (prefix (subseq relative-pathname 0 dot-pos))
         (relative-fasl-pathname (concatenate 'string prefix ".fasl")))
    (let* ((boot-relative-fasl-pathname
             (concatenate 'string "Boot/ASTs/" relative-fasl-pathname))
           (fasl-pathname
             (asdf:system-relative-pathname '#:sicl boot-relative-fasl-pathname)))
      (if (probe-file fasl-pathname)
          (format *trace-output* "Reusing existing FASL.~%")
          (compile-file (sicl-genv:client *e0*) relative-pathname *e0*)))
    (load-fasl relative-fasl-pathname environment)))
