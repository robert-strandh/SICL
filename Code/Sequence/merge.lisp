(cl:in-package #:sicl-sequence)

(defun merge (result-type sequence-1 sequence-2 predicate &key key)
  (with-reified-result-type (prototype result-type)
    (merge-sequence-like prototype sequence-1 sequence-2 predicate :key key)))

(define-compiler-macro merge
    (&whole form result-type sequence-1 sequence-2 predicate &rest rest &environment env)
  (if (constantp result-type)
      (let ((type (eval result-type)))
        `(the ,type
              (merge-sequence-like
               ',(reify-sequence-type-specifier type env)
               ,sequence-1
               ,sequence-2
               ,predicate
               ,@rest)))
      form))

(defmethod merge-sequence-like
    ((prototype list) (sequence-1 sequence) (sequence-2 sequence) predicate &key key)
  (let ((predicate (function-designator-function predicate))
        (a (coerce sequence-1 'list))
        (b (coerce sequence-2 'list)))
    (with-key-function (key key)
      ;; This technique of destructively merging lists is inspired by
      ;; lmsort.scm by Olin Shivers.  It minimizes the number of writes to
      ;; memory.
      (labels ((scan-a (prev a a-key b b-key)
                 (declare (cons prev a b))
                 (if (funcall predicate b-key a-key)
                     (let ((next-b (cdr b)))
                       (setf (cdr prev) b)
                       (if (endp next-b)
                           (setf (cdr b) a)
                           (scan-b b a a-key next-b (key (car next-b)))))
                     (let ((next-a (cdr a)))
                       (if (endp next-a)
                           (setf (cdr a) b)
                           (if (endp next-a)
                               (setf (cdr a) b)
                               (scan-a a next-a (key (car next-a)) b b-key))))))
               (scan-b (prev a a-key b b-key)
                 (declare (cons prev a b))
                 (if (funcall predicate b-key a-key)
                     (let ((next-b (cdr b)))
                       (if (endp next-b)
                           (setf (cdr b) a)
                           (scan-b b a a-key next-b (key (car next-b)))))
                     (let ((next-a (cdr a)))
                       (setf (cdr prev) a)
                       (if (endp next-a)
                           (setf (cdr a) b)
                           (if (endp next-a)
                               (setf (cdr a) b)
                               (scan-a a next-a (key (car next-a)) b b-key)))))))
        (cond ((endp a) b)
              ((endp b) a)
              (t
               (let ((a-key (key (car a)))
                     (b-key (key (car b))))
                 (if (funcall predicate b-key a-key)
                     (prog1 b
                       (let ((next-b (cdr b)))
                         (if (endp next-b)
                             (setf (cdr b) a)
                             (scan-b b a a-key next-b (key (car next-b))))))
                     (prog1 a
                       (let ((next-a (cdr a)))
                         (if (endp next-a)
                             (setf (cdr a) b)
                             (scan-a a next-a (key (car next-a)) b b-key))))))))))))

(seal-domain #'merge-sequence-like '(list t t t))

(replicate-for-each #1=#:vector-class (simple-vector vector)
  ;; Merging of vectors of the same type as the prototype.
  (defmethod merge-sequence-like
      ((prototype #1#) (vector-1 #1#) (vector-2 #1#) predicate &key key)
    (let ((predicate (function-designator-function predicate))
          (length-1 (length vector-1))
          (length-2 (length vector-2)))
      (declare (vector-length length-1 length-2))
      (with-key-function (key key)
        (cond ((zerop length-1) (coerce vector-2 (class-of prototype)))
              ((zerop length-2) (coerce vector-2 (class-of prototype)))
              (t
               (let* ((result (make-sequence-like prototype (+ length-1 length-2)))
                      (elt-1 (elt vector-1 0))
                      (elt-2 (elt vector-2 0))
                      (key-1 (key elt-1))
                      (key-2 (key elt-2))
                      (result-index 0)
                      (index-1 0)
                      (index-2 0))
                 (declare (vector-length result-index index-1 index-1))
                 (flet ((finish (vector index length)
                          (loop for i fixnum from index below length do
                            (setf (elt result result-index)
                                  (elt vector i))
                            (incf result-index))
                          (return-from merge-sequence-like result)))
                   (loop
                     (if (funcall predicate key-2 key-1)
                         (progn
                           (setf (elt result result-index) elt-2)
                           (incf index-2)
                           (incf result-index)
                           (when (= index-2 length-2)
                             (finish vector-1 index-1 length-1))
                           (setf elt-2 (elt vector-2 index-2))
                           (setf key-2 (key elt-2)))
                         (progn
                           (setf (elt result result-index) elt-1)
                           (incf index-1)
                           (incf result-index)
                           (when (= index-1 length-1)
                             (finish vector-2 index-2 length-2))
                           (setf elt-1 (elt vector-1 index-1))
                           (setf key-1 (key elt-1)))))))))))))

(replicate-for-each #1=#:vector-class (simple-vector vector)
  ;; Merging of arbitrary sequences.
  (defmethod merge-sequence-like
      ((prototype #1#) (sequence-1 sequence) (sequence-2 sequence) predicate &key key)
    (let ((predicate (function-designator-function predicate))
          (length-1 (length sequence-1))
          (length-2 (length sequence-2)))
      (declare (sequence-length length-1 length-2))
      (with-key-function (key key)
        (cond
          ((zerop length-1) (coerce sequence-2 (class-of prototype)))
          ((zerop length-2) (coerce sequence-1 (class-of prototype)))
          (t
           (with-sequence-scanner (iterator-1 sequence-1)
             (with-sequence-scanner (iterator-2 sequence-2)
               (multiple-value-bind (element-1-p element-1) (iterator-1)
                 (multiple-value-bind (element-2-p element-2) (iterator-2)
                   (let ((result (make-sequence-like prototype (+ length-1 length-2)))
                         (index 0))
                     (declare (vector-length index))
                     (flet ((collect (element)
                              (setf (elt result index) element)
                              (incf index)))
                       (macrolet
                           ((finish (element-p element iterator)
                              `(loop while ,element-p
                                     do (collect ,element)
                                     do (multiple-value-setq (,element-p ,element) (,iterator))
                                     finally (return-from merge-sequence-like result)))
                            (gather (element-p element iterator key
                                     other-element-p other-element other-iterator)
                              `(multiple-value-bind (new-element-p new-element) (,iterator)
                                 (collect ,element)
                                 (if (not new-element-p)
                                     (finish ,other-element-p ,other-element ,other-iterator)
                                     (setf ,element-p new-element-p
                                           ,element new-element
                                           ,key (key new-element))))))
                         (let ((key-1 (key element-1))
                               (key-2 (key element-2)))
                           (loop
                             (if (funcall predicate key-2 key-1)
                                 (gather element-2-p element-2 iterator-2 key-2
                                         element-1-p element-1 iterator-1)
                                 (gather element-1-p element-1 iterator-1 key-1
                                         element-2-p element-2 iterator-2)))))))))))))))))

(seal-domain #'merge-sequence-like '(vector t t t))
