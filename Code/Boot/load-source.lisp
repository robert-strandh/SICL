(cl:in-package #:sicl-boot)

(defun load-source (relative-pathname environment)
  (compile-file (sicl-genv:client *e0*) relative-pathname *e0*)
  (let* ((dot-pos (position #\. relative-pathname))
         (prefix (subseq relative-pathname 0 dot-pos))
         (relative-fasl-pathname (concatenate 'string prefix ".fasl")))
    (load-fasl relative-fasl-pathname environment)))

                
