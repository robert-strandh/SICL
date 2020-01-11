(cl:in-package #:sicl-sequence)

(defmethod replace ((datum t) sequence &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod replace (sequence (datum t) &key &allow-other-keys)
  (error 'must-be-sequence
         :datum datum))

(defmethod replace ((list1 list) (list2 list) &key (start1 0) end1 (start2 0) end2)
  (let ((dst (make-list-writer list1 start1 end1 nil (lambda () (return-from replace list1))))
        (src (make-list-reader list2 start2 end2 nil (lambda () (return-from replace list1)))))
    (loop (funcall dst (funcall src)))))

(replicate-for-each-relevant-vectoroid #1=#:vectoroid
  (defmethod replace ((list list) (vector #1#) &key (start1 0) end1 (start2 0) end2)
    (let ((dst (make-list-writer list start1 end1 nil (lambda () (return-from replace list))))
          (src (make-vector-reader vector start2 end2 nil (lambda () (return-from replace list)))))
      (loop (funcall dst (funcall src)))))

  (defmethod replace ((vector #1#) (list list) &key (start1 0) end1 (start2 0) end2)
    (let ((dst (make-vector-writer vector start1 end1 nil (lambda () (return-from replace vector))))
          (src (make-list-reader list start2 end2 nil (lambda () (return-from replace vector)))))
      (loop (funcall dst (funcall src))))))

(replicate-for-all-compatible-vectoroids #1=#:vectoroid-1 #2=#:vectoroid-2
  (defmethod replace ((vectoroid-1 #1#) (vectoroid-2 #2#) &key start1 end1 start2 end2)
    (let ((dst (make-vector-writer vectoroid-1 start1 end1 nil (lambda () (return-from replace vectoroid-1))))
          (src (make-vector-reader vectoroid-2 start2 end2 nil (lambda () (return-from replace vectoroid-1)))))
      (loop (funcall dst (funcall src))))))
