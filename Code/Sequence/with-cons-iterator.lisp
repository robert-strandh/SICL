(cl:in-package #:sicl-sequence)

(defmacro with-cons-iterator ((name list start end) &body body)
  (check-type name symbol)
  (sicl-utilities:once-only (list start end)
    (sicl-utilities:with-gensyms (rest index limit)
      `(let ((,index
               (typecase ,start
                 (list-length ,start)
                 (otherwise
                  (error 'invalid-start-index
                         :expected-type 'list-length
                         :datum ,start
                         :in-sequence ,list))))
             (,limit
               (typecase ,end
                 (null +most-positive-list-length+)
                 (list-length
                  (if (<= ,start ,end)
                      ,end
                      (error 'end-less-than-start
                             :datum ,start
                             :expected-type `(integer 0 ,,end)
                             :end-index ,end
                             :in-sequence ,list)))
                 (otherwise
                  (error 'invalid-end-index
                         :expected-type '(or null (integer 0 ,+most-positive-list-length+))
                         :datum ,end
                         :in-sequence ,list))))
             (,rest (nthcdr ,start ,list)))
         (declare (list-length ,index ,limit))
         (flet ((,name ()
                  (if (= ,index ,limit)
                      (values nil nil ,index)
                      (if (endp ,rest)
                          (if (null ,end)
                              (values nil nil ,index)
                              (error 'invalid-end-index
                                     :expected-type `(integer 0 ,,index)
                                     :datum ,end
                                     :in-sequence ,list))
                          (multiple-value-prog1 (values t ,rest ,index)
                            (incf ,index)
                            (pop ,rest))))))
           (declare (inline ,name))
           ,@body)))))
