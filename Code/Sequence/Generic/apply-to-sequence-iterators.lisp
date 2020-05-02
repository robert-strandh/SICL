(cl:in-package #:sicl-sequence)

(defun apply-to-sequence-iterators (writer function readers)
  (declare (function writer function)
           (type simple-vector readers))
  (macrolet ((loop-apply-n (arity)
               (let ((gensyms (loop repeat arity collect (gensym))))
                 `(let ,(loop for gensym in gensyms
                              for index from 0
                              collect
                              `(,gensym (the function (svref readers ,index))))
                    (loop
                      (funcall
                       writer
                       (funcall
                        function
                        ,@(loop for gensym in gensyms collect `(funcall ,gensym)))))))))
    (let ((n (length readers)))
      (case n
        (0 (loop-apply-n 0))
        (1 (loop-apply-n 1))
        (2 (loop-apply-n 2))
        (3 (loop-apply-n 3))
        (4 (loop-apply-n 4))
        (5 (loop-apply-n 5))
        (6 (loop-apply-n 6))
        (7 (loop-apply-n 7))
        (8 (loop-apply-n 8))
        (9 (loop-apply-n 9))
        (otherwise
         (loop
           (let ((args '()))
             (loop for index from (1- n) downto 0 do
               (push (funcall (elt readers index)) args))
             (funcall writer (apply function args)))))))))
