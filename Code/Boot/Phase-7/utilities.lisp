(cl:in-package #:sicl-boot-phase-7)

(defun load-source (relative-pathname environment)
  (let* ((dot-pos (position #\. relative-pathname))
         (prefix (subseq relative-pathname 0 dot-pos))
         (relative-fasl-pathname (concatenate 'string prefix ".fasl")))
    (sicl-boot:compile-file
     (sicl-genv:client sicl-boot:*e0*)
     relative-pathname
     environment)
    (load-fasl relative-fasl-pathname environment)))
