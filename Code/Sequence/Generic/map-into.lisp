(cl:in-package #:sicl-sequence)

(declaim (inline adjustable-sequence-p))
(defun adjustable-sequence-p (x)
  (typecase x
    (vector (adjustable-array-p x))
    (sequence nil)
    (otherwise
     (error 'must-be-sequence
            :datum x))))

(defun map-into (result-sequence function &rest sequences)
  (when (adjustable-sequence-p result-sequence)
    (setf (fill-pointer result-sequence)
          (array-total-size result-sequence)))
  (let (min-amount)
    (block block
      (symbol-macrolet ((terminate (lambda () (return-from block))))
        (multiple-value-bind (writer amount)
            (make-sequence-writer result-sequence 0 nil nil terminate)
          (setf min-amount amount)
          (sicl-utilities:with-collectors ((readers collect-reader))
            (loop for sequence in sequences do
              (multiple-value-bind (reader amount)
                  (make-sequence-reader sequence 0 nil nil terminate)
                (cond ((not min-amount)
                       (setf min-amount amount))
                      ((< amount min-amount)
                       (setf min-amount amount)))
                (collect-reader reader)))
            (apply
             #'map-over-iterators
             writer
             (function-designator-function function)
             (readers))))))
    (when (adjustable-sequence-p result-sequence)
      (setf (fill-pointer result-sequence) min-amount))
    result-sequence))

(define-compiler-macro map-into (result-sequence function &rest sequences)
  (sicl-utilities:with-gensyms (min-amount block writer amount)
    (sicl-utilities:once-only (result-sequence function)
      (let* ((terminate `(lambda () (return-from ,block)))
             (n-sequences (length sequences))
             (reader-gensyms
               (loop repeat n-sequences
                     collect (gensym "READER")))
             (amount-gensyms
               (loop repeat n-sequences
                     collect (gensym "AMOUNT"))))
        (labels ((wrap-in-bindings (sequences readers amounts form)
                   (if (null sequences)
                       form
                       `(multiple-value-bind (,(first readers) ,(first amounts))
                            (make-sequence-reader ,(first sequences) 0 nil nil ,terminate)
                          ,(wrap-in-bindings
                            (rest sequences)
                            (rest readers)
                            (rest amounts)
                            form)))))
          `(let (,min-amount)
             (block ,block
               (multiple-value-bind (,writer ,amount)
                   (make-sequence-writer ,result-sequence 0 nil nil ,terminate)
                 (when (adjustable-sequence-p ,result-sequence)
                   (setf (fill-pointer ,result-sequence) ,amount))
                 ,(wrap-in-bindings
                   sequences
                   reader-gensyms
                   amount-gensyms
                   `(progn
                      (setf ,min-amount (min ,amount ,@amount-gensyms))
                      (map-over-iterators
                       ,writer
                       (function-designator-function ,function)
                       ,@reader-gensyms)))))
             (when (adjustable-sequence-p ,result-sequence)
               (setf (fill-pointer ,result-sequence) ,min-amount))
             ,result-sequence))))))
