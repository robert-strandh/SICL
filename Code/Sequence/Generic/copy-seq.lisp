(cl:in-package #:sicl-sequence)

(defmethod copy-seq ((list list))
  (sicl-utilities:with-collectors ((result collect))
    (do ((rest list (cdr rest)))
        ((endp list) (result))
      (collect (car rest)))))

(seal-domain #'copy-seq '(list))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod copy-seq ((vector #1#))
    (make-sequence-like vector (length vector) :initial-contents vector)))

(seal-domain #'copy-seq '(vector))
