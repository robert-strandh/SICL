(cl:in-package #:sicl-boot)

(defun phase3 (boot)
  (let ((c (c1 boot))
	(r (r2 boot)))
    (define-effective-slot-definition-class boot)))
