(cl:in-package #:sicl-string)

;;; Create a fresh string containing the characters of STRING in the
;;; interval between START and END.
(defun extract-interval (string start end)
  (let ((result (make-string (- end start))))
    (loop for source-index from start below end
          for destination-index from 0
          do (setf (char result destination-index)
                   (char string source-index)))
    result))

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
      (error 'invalid-bounding-indices
             :start start
             :end end
             :target string))))
