(cl:in-package #:sicl-sequence)

(defmacro with-cons-iterator
    ((name &optional (reset nil reset-supplied-p))
     (list start end)
     &body body)
  (check-type name symbol)
  (sicl-utilities:once-only (list start end)
    (sicl-utilities:with-gensyms (head rest index limit)
      `(if (not (typep ,start 'list-length))
           (error 'invalid-start-index
                  :expected-type 'list-length
                  :datum ,start
                  :in-sequence ,list)
           (let* ((,index ,start)
                  (,head (nthcdr ,start ,list))
                  (,rest ,head)
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
                              :in-sequence ,list)))))
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
                                (pop ,rest)))))
                    ,@(when reset-supplied-p
                        `((,reset () (setf ,rest ,head ,index ,start)))))
               (declare (inline ,name))
               ,@body))))))
