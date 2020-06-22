(cl:in-package #:sicl-sequence)

(defun concatenate (result-type &rest sequences)
  (with-reified-result-type (prototype result-type)
    (apply #'concatenate-sequence-like prototype sequences)))

(define-compiler-macro concatenate
    (&whole form result-type &rest sequences &environment env)
  (if (not (constantp result-type))
      form
      (let* ((type (eval result-type))
             (prototype (reify-sequence-type-specifier type env))
             (bindings (loop for sequence in sequences collect `(,(gensym) ,sequence))))
        `(the
          ,type
          (let ,bindings
            ,(typecase prototype
               (list
                (sicl-utilities:with-gensyms (result collect)
                  `(sicl-utilities:with-collectors ((,result ,collect))
                     ,@(loop for (var nil) in bindings collect
                             `(map nil (function ,collect) ,var))
                     (,result))))
               (vector
                (sicl-utilities:with-gensyms (length result index collect)
                  `(let* ((,length (+ ,@(loop for (var nil) in bindings
                                              collect `(length ,var))))
                          (,result (make-sequence-like ',prototype ,length))
                          (,index 0))
                     (declare (vector-length ,length ,index))
                     (declare (type ,type ,result))
                     (flet ((,collect (value)
                              (setf (elt ,result ,index) value)
                              (incf ,index)))
                       (declare (inline ,collect))
                       ,@(loop for (var nil) in bindings collect
                               `(map nil (function ,collect) ,var)))
                     ,result)))
               (otherwise
                `(concatenate-sequence-like ',prototype ,@(mapcar #'first bindings)))))))))

(defmethod concatenate-sequence-like ((list list) &rest sequences)
  (sicl-utilities:with-collectors ((result collect))
    (loop for sequence in sequences do
      (map nil #'collect sequence))
    (result)))

(seal-domain #'concatenate-sequence-like '(list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod concatenate-sequence-like ((vector #1#) &rest sequences)
    (let* ((length (loop for sequence in sequences sum (length sequence)))
           (result (make-sequence-like vector length))
           (index 0))
      (declare (vector-length length index))
      (declare (type #1# result))
      (flet ((collect (value)
               (setf (elt result index) value)
               (incf index)))
        (loop for sequence in sequences do
          (map nil #'collect sequence)))
      result)))

(seal-domain #'concatenate-sequence-like '(vector))
