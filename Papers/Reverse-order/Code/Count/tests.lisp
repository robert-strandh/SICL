(defun test-reverse-count-once (n reverse-count-fun)
  (let* ((k (random 10))
	 (l (make-list n :initial-element k)))
    (assert (= (funcall reverse-count-fun k l) n))))

(defun test-reverse-count (n reverse-count-fun)
  (loop for i from 1 to n
	do (test-reverse-count-once i reverse-count-fun)))
