(cl:in-package #:sicl-sequence)

(defun map (result-type function sequence &rest more-sequences)
  (let ((sequences (cons sequence more-sequences)))
    (declare (dynamic-extent sequences))
    (case result-type
      ((nil)
       (flet ((terminate ()
                (return-from map nil)))
         (apply
          #'map-nil-over-iterators
          (function-designator-function function)
          (loop for sequence in sequences
                collect
                (make-sequence-reader sequence 0 nil nil #'terminate)))))
      ((list)
       (sicl-utilities:with-collectors ((result collect))
         (flet ((terminate ()
                  (return-from map (result))))
           (apply
            #'map-over-iterators
            (lambda (elt) (collect elt))
            (function-designator-function function)
            (loop for sequence in sequences
                  collect
                  (make-sequence-reader sequence 0 nil nil #'terminate))))))
      (otherwise
       (let (result-sequence min-amount)
         (flet ((terminate ()
                  (return-from map result-sequence)))
           (sicl-utilities:with-collectors ((readers collect-reader))
             (loop for sequence in sequences do
               (multiple-value-bind (reader amount)
                   (make-sequence-reader sequence 0 nil nil #'terminate)
                 (cond ((not min-amount)
                        (setf min-amount amount))
                       ((< amount min-amount)
                        (setf min-amount amount)))
                 (collect-reader reader)))
             (setf result-sequence
                   (make-sequence result-type min-amount))
             (apply
              #'map-over-iterators
              (make-sequence-writer result-sequence 0 nil nil #'terminate)
              (function-designator-function function)
              (readers)))))))))

(define-compiler-macro map (result-type function sequence &rest more-sequences)
  (multiple-value-bind (type-form type)
      (if (constantp result-type)
          (let ((value (simplify-sequence-type-specifier (eval result-type))))
            (values `',value value))
          (values result-type '*))
    (case type
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
      ((list cons)
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
                         (lambda (n)
                           (declare (ignore n))
                           (return-from ,map (,result))))))))))
      (otherwise
       (sicl-utilities:once-only (type-form function)
         (sicl-utilities:with-gensyms (result-sequence block)
           (let* ((sequences (list* sequence more-sequences))
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
                                 (make-sequence-reader
                                  ,(first sequences) 0 nil nil
                                  (lambda () (return-from ,block ,result-sequence)))
                               ,(wrap-in-bindings
                                 (rest sequences)
                                 (rest readers)
                                 (rest amounts)
                                 form)))))
               `(let (,result-sequence)
                  (block ,block
                    ,(wrap-in-bindings
                      sequences
                      reader-gensyms
                      amount-gensyms
                      `(progn
                         (setf ,result-sequence (make-sequence ,type-form (min ,@amount-gensyms)))
                         (map-over-iterators
                          (make-sequence-writer ,result-sequence 0 nil nil (lambda ()))
                          (function-designator-function ,function)
                          ,@reader-gensyms)))))))))))))
