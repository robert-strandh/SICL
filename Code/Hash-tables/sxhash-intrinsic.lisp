(cl:in-package #:sicl-hash-table)

(defun eq-hash (last-hash object)
  (typecase object
    ;; Instances of a built-in-class can't change-class, so we can gather some
    ;; entropy from their classes at the very least.
    (cons      (fnv-1a last-hash 1))
    (character (let ((code (char-code object)))
                 (fnv-1a last-hash
                         2
                         (ldb (byte 8 0) code)
                         (ldb (byte 8 8) code))))
    (integer   (fnv-1a last-hash
                       3
                       (ldb (byte 8 0) object)
                       (ldb (byte 8 8) object)
                       (ldb (byte 8 16) object)
                       (ldb (byte 8 24) object)))
    (float     (multiple-value-bind (significand exponent)
                   (integer-decode-float object)
                 (fnv-1a last-hash
                         4
                         (ldb (byte 8 0) significand)
                         (ldb (byte 8 8) significand)
                         (ldb (byte 8 16) significand)
                         (ldb (byte 8 0) exponent))))
    ;; The element-type of an array won't change.
    (standard-object
     (let ((hash (sicl-clos:hash-code object)))
       (macrolet ((it ()
                    `(fnv-1a last-hash 5
                             ,@(loop for byte below 8
                                     collect `(ldb (byte 8 ,(* byte 8))
                                                   hash)))))
         (it))))
    (t last-hash)))
