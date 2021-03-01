(in-package #:cleavir-kildall)

(defgeneric pool-meet (specialization pool1 pool2))
(defgeneric pool<= (specialization pool1 pool2))
(defgeneric object1 (specialization key))

(defgeneric find-in-pool (specialization key pool))

(defgeneric map-into-pool (specialization function pool))

(defmacro do-into-pool (s pool (var val) &body body)
  `(map-into-pool ,s (lambda (,var ,val) ,@body) ,pool))

(defmacro with-pool-reader (s instruction reader &body body)
  (let ((ss (gensym "SPECIALIZATION"))
        (sinstruction (gensym "INSTRUCTION"))
        (pool (gensym "POOL"))
        (variable (gensym "VARIABLE")))
    `(let* ((,ss ,s) (,sinstruction ,instruction)
            (,pool (maybe-instruction-pool ,ss ,sinstruction)))
       (flet ((,reader (,variable)
                (find-in-pool ,ss ,variable ,pool)))
         (declare (inline ,reader))
         ,@body))))

(defmacro copy (s instruction var from-reader
                (&body lists) (&body singles))
  (let ((ss (gensym "SPECIALIZATION"))
        (sinstruction (gensym "INSTRUCTION"))
        (dest (gensym "DESTINATION-POOL"))
        (update (gensym "UPDATE"))
        (dest-val (gensym "DEST-VAL")))
    `(let* ((,ss ,s) (,sinstruction ,instruction)
            ;; If an instruction doesn't have a pool we need to
            ;; transfer to it, to ensure that all reachable
            ;; instructions are transferred to at least once.
            (,update (if (pool-present-p ,ss ,sinstruction) nil t))
            (,dest (maybe-instruction-pool ,ss ,sinstruction)))
       (do-into-pool ,ss ,dest (,var ,dest-val)
         (let ((new-val
                 (cond ,@(loop for (list . body) in lists
                               collect `((find ,var ,list)
                                         ,@body))
                       ,@(loop for (single . body) in singles
                               collect `((eql ,var ,single)
                                         ,@body))
                       (t (,from-reader ,var)))))
           (cond ((pool<= ,ss ,dest-val new-val) ,dest-val)
                 (t
                  (setf ,dest-val (pool-meet ,ss ,dest-val new-val)
                        ,update t)
                  new-val))))
       (when ,update (add-work ,sinstruction)))))
