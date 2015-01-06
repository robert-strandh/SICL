
(defun test-reverse-count-once (n fun)
  (let ((l (make-list n :initial-element 0)))
    (assert (= (funcall fun 0 l) n))))

(defun test-reverse-count (n fun)
  (loop for i from 1 to n
	do (test-reverse-count-once i fun)))

(defmacro divide (rest length k)
  (let* ((n (ash 1 k))
	 (gensyms (loop repeat n collect (gensym)))
	 (f (gensym)))
    `(let ((,f (ash length (- ,k)))
       (,(car gensyms) ,rest))
       (let* ,(loop
         for gensym1 in gensyms
         for gensym2 in (cdr gensyms)
         collect `(,gensym2 (nthcdr ,f ,gensym1)))
     (traverse (nthcdr ,f ,(car (last gensyms)))
           (- ,length (ash ,f ,k)))
     ,@(loop
          for gensym in (reverse gensyms)
          collect `(traverse ,gensym ,f))))))
