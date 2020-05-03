(cl:in-package #:sicl-sequence)

(defun map-into (result-sequence function &rest sequences)
  (symbol-macrolet ((terminate
                      (lambda (n)
                        (when (adjustable-array-p result-sequence)
                          (setf (fill-pointer result-sequence) n))
                        (return-from map-into result-sequence))))
    (when (adjustable-array-p result-sequence)
      (setf (fill-pointer result-sequence)
            (array-total-size result-sequence)))
    (apply
     #'map-over-iterators
     (make-sequence-writer result-sequence 0 nil nil terminate)
     (function-designator-function function)
     (loop for sequence in sequences
           collect
           (make-sequence-reader sequence 0 nil nil terminate)))))

(define-compiler-macro map-into (result-sequence function &rest sequences)
  (sicl-utilities:with-gensyms (map-into)
    (sicl-utilities:once-only (result-sequence)
      (let ((terminate `(lambda (n)
                          (when (adjustable-array-p ,result-sequence)
                            (setf (fill-pointer ,result-sequence) n))
                          (return-from ,map-into ,result-sequence))))
        `(block ,map-into
           (when (adjustable-array-p ,result-sequence)
             (setf (fill-pointer ,result-sequence)
                   (array-total-size ,result-sequence)))
           (map-over-iterators
            (make-sequence-writer ,result-sequence 0 nil nil ,terminate)
            (function-designator-function ,function)
            ,@(loop for sequence in sequences
                    collect
                    `(make-sequence-reader ,sequence 0 nil nil ,terminate))))))))
