(cl:in-package #:sicl-sequence)

(defun map (result-type function sequence &rest more-sequences)
  (case result-type
    ((nil)
     (flet ((terminate (n)
              (declare (ignore n))
              (return-from map nil)))
       (apply
        #'map-nil-over-iterators
        (function-designator-function function)
        (make-sequence-reader sequence 0 nil nil #'terminate)
        (loop for sequence in more-sequences
              collect
              (make-sequence-reader sequence 0 nil nil #'terminate)))))
    ((list)
     (sicl-utilities:with-collectors ((result collect))
       (flet ((terminate (n)
                (declare (ignore n))
                (return-from map (result))))
         (apply
          #'map-over-iterators
          (lambda (elt)
            (collect elt))
          (function-designator-function function)
          (make-sequence-reader sequence 0 nil nil #'terminate)
          (loop for sequence in more-sequences
                collect
                (make-sequence-reader sequence 0 nil nil #'terminate))))))
    (otherwise
     (let ((result-sequence
             (make-sequence
              result-type
              (apply #'length-of-shortest-sequence sequence more-sequences))))
       (flet ((terminate (n)
                (declare (ignore n))
                (return-from map result-sequence)))
         (apply
          #'map-over-iterators
          (make-sequence-writer result-sequence 0 nil nil #'identity)
          (function-designator-function function)
          (make-sequence-reader sequence 0 nil nil #'terminate)
          (loop for sequence in more-sequences
                collect
                (make-sequence-reader sequence 0 nil nil #'terminate))))))))

;;; We perform two important compile time optimizations.  The first one is
;;; that we attempt to simplify the specified RESULT-TYPE.  The second one
;;; is that we replace every input sequence by its corresponding sequence
;;; reader.  By inlining these calls into the call site, we can often avoid
;;; the costly dispatch of MAKE-SEQUENCE-READER.
(define-compiler-macro map (result-type function sequence &rest more-sequences)
  (when (constantp result-type)
    (setf result-type (simplify-sequence-type-specifier (eval result-type))))
  (case result-type
    ((nil)
     (sicl-utilities:with-gensyms (map)
       `(block ,map
          (map-nil-over-iterators
           (function-designator-function ,function)
           ,@(loop for sequence in (list* sequence more-sequences)
                   collect
                   `(make-sequence-reader
                     ,sequence
                     0 nil nil
                     (lambda (n)
                       (declare (ignore n))
                       (return-from ,map nil))))))))
    ((list)
     (sicl-utilities:with-gensyms (map result collect)
       `(block ,map
          (sicl-utilities:with-collectors ((,result ,collect))
            (map-over-iterators
             (lambda (elt)
               (,collect elt))
             (function-designator-function ,function)
             ,@(loop for sequence in (list* sequence more-sequences)
                     collect
                     `(make-sequence-reader
                       ,sequence
                       0 nil nil
                       (lambda (n) (return-from ,map (,result))))))))))
    (otherwise
     (break "TODO"))))
