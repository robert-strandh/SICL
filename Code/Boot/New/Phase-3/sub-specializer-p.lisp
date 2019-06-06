(cl:in-package #:sicl-clos)

(defun sub-specializer-p (specializer1 specializer2 class-of-argument)
  (let ((precedence-list (class-precedence-list class-of-argument)))
    (< (position specializer1 precedence-list)
       (position specializer2 precedence-list))))

