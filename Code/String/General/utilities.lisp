(cl:in-package #:sicl-string)

(defun check-bounding-indices (string start end)
  (let ((length (length string)))
    (unless (typep start `(integer 0 ,length))
      (error 'type-error
	     :datum start
	     :expected-type `(integer 0 ,length)))
    (unless (typep end `(integer 0 ,length))
      (error 'type-error
	     :datum end
	     :expected-type `(integer 0 ,length)))
    (unless (<= start end)
      ;; FIXME: signal a more appropriate condition.
      (error "START must be less than or equal to END."))))
