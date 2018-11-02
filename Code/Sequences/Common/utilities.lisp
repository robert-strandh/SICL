(cl:in-package #:sicl-sequence)

;;; Skip a prefix of a list and signal an error if the list is too
;;; short, or if it is not a proper list.  Also check that start is a
;;; nonnegative integer.
(defun skip-to-start (name list start)
  (let ((start-bis start)
        (remaining list))
    (loop until (zerop start-bis)
          until (atom remaining)
          do (setf remaining (cdr remaining))
             (decf start-bis)
          finally (when (and (atom remaining) (not (null remaining)))
                    (error 'must-be-proper-list
                           :name name
                           :datum list))
                  (when (plusp start-bis)
                    (error 'invalid-start-index
                           :name name
                           :datum start
                           :expected-type `(integer 0 ,(- start start-bis))
                           :in-sequence list)))
    remaining))

;;; This function is called at the end of some list traversal
;;; to make sure that the list is a proper list.
(defun tail-must-be-proper-list (name list tail)
  (when (and (atom tail) (not (null tail)))
    (error 'must-be-proper-list
           :name name
           :datum list)))

;;; This function is called at the end of some list traversal
;;; to make sure that the list is a proper list, and to make sure
;;; that end is a valid index.
(defun tail-must-be-proper-list-with-end (name list tail end length)
  (when (and (atom tail) (not (null tail)))
    (error 'must-be-proper-list
           :name name
           :datum list))
  (when (and (atom tail) (< length end))
    (error 'invalid-end-index
           :name name
           :datum end
           :in-sequence list
           :expected-type `(integer 0 ,length))))

;;; This function is used when the sequence is a vector of some kind
;;; in order to verify that start and end are valid bounding indexes.
;;; It has already been verified that start is a nonnegative integer.
;;; FIXME: What do we know about end?
(defun verify-bounding-indexes (name vector start end)
  (let ((length (length vector)))
    (when (> start length)
      (error 'invalid-start-index
             :name name
             :datum start
             :expected-type `(integer 0 ,length)
             :in-sequence vector))
    (unless (<= 0 end length)
      (error 'invalid-end-index
             :name name
             :datum end
             :expected-type `(integer 0 ,length)
             :in-sequence vector))
    (unless (<= start end)
      (error 'end-less-than-start
             :name name
             :datum start
             :expected-type `(integer 0 ,end)
             :end-index end
             :in-sequence vector))))

;;; This function is used to compute the length of the list
;;; given a remainder and a start index.
(defun compute-length-from-remainder (name list remainder start)
  (loop for length from start
        until (atom remainder)
        do (setf remainder (cdr remainder))
        finally (unless (null remainder)
                  (error 'must-be-proper-list
                         :name name
                         :datum list))
                (return length)))

;;; This function is used to verify that the end sequence index
;;; is valid, and that, if we reach the end of the list, it is
;;; a proper list.  The remainder of the list is returned.
(defun verify-end-index (name list remainder start end)
  (loop for length from start
        until (or (atom remainder) (>= length end))
        do (setf remainder (cdr remainder))
           finally (unless (or (null remainder) (consp remainder))
                     (error 'must-be-proper-list
                         :name name
                         :datum list))
                   (when (< length end)
                     (error 'invalid-end-index
                            :name name
                            :datum end
                            :expect-type `(integer 0 ,length)
                            :in-sequence list))
                   (return remainder)))

(defmacro process-list-elements-from-start-without-end
    ((list-form cons-var) &body body)
  `(loop for ,cons-var = ,list-form then (cdr ,cons-var)
         while (consp ,cons-var)
         do ,@body
         finally (unless (null ,cons-var)
                   (error "Not a proper list"))))

(defmacro for-each-relevant-cons
    ((cons-var index-var list start end from-end) &body body)
  (let ((fun-var (gensym))
        (list-var (gensym))
        (start-var (gensym))
        (end-var (gensym))
        (rest-var (gensym))
        (length-var (gensym))
        (iter-var (gensym)))
    `(let (,length-var
           (,list-var ,list)
           (,start-var ,start)
           (,end-var ,end))
       (if ,from-end
           (progn (when (null ,end-var)
                    (setf ,end-var (length ,list-var)))
                  (if (null ,start-var)
                      (setf ,start-var 0)
                      (setf ,list-var (nthcdr ,start-var ,list-var)))
                  (setf ,length-var (- ,end-var ,start-var))
                  (labels ((,fun-var (,rest-var ,index-var ,length-var)
                             (declare (type fixnum ,length-var))
                             (when (> ,length-var 0)
                               (when (consp ,rest-var)
                                 (traverse (cdr ,rest-var)
                                           (1+ ,index-var)
                                           (1- ,length-var))
                                 (let ((,cons-var ,rest-var))
                                   ,@body))))
                           (traverse (rest index length)
                             (declare (type fixnum index length))
                             (if (< length 100)
                                 (,fun-var rest index length)
                                 (let* ((p8 (ash length -1))
                                        (p4 (ash p8 -1))
                                        (p2 (ash p4 -1))
                                        (p1 (ash p2 -1))
                                        (p3 (+ p2 (ash (- p4 p2) -1)))
                                        (p6 (+ p4 (ash (- p8 p4) -1)))
                                        (p5 (+ p4 (ash (- p6 p4) -1)))
                                        (p7 (+ p6 (ash (- p8 p6) -1)))
                                        (p12 (+ p8 (ash (- length p8) -1)))
                                        (p10 (+ p8 (ash (- p12 p8) -1)))
                                        (p9 (+ p8 (ash (- p10 p8) -1)))
                                        (p11 (+ p10 (ash (- p12 p10) -1)))
                                        (p14 (+ p12 (ash (- length p12) -1)))
                                        (p13 (+ p12 (ash (- p14 p12) -1)))
                                        (p15 (+ p14 (ash (- length p14) -1)))
                                        (r1 (nthcdr p1 rest))
                                        (r2 (nthcdr (- p2 p1) r1))
                                        (r3 (nthcdr (- p3 p2) r2))
                                        (r4 (nthcdr (- p4 p3) r3))
                                        (r5 (nthcdr (- p5 p4) r4))
                                        (r6 (nthcdr (- p6 p5) r5))
                                        (r7 (nthcdr (- p7 p6) r6))
                                        (r8 (nthcdr (- p8 p7) r7))
                                        (r9 (nthcdr (- p9 p8) r8))
                                        (r10 (nthcdr (- p10 p9) r9))
                                        (r11 (nthcdr (- p11 p10) r10))
                                        (r12 (nthcdr (- p12 p11) r11))
                                        (r13 (nthcdr (- p13 p12) r12))
                                        (r14 (nthcdr (- p14 p13) r13))
                                        (r15 (nthcdr (- p15 p14) r14)))
                                   (declare (type fixnum p1 p2 p3 p4 p5 p6)
                                            (type fixnum p7 p8 p9 p10 p11)
                                            (type fixnum p12 p13 p14 p15))
                                   (traverse r15 (+ index p15) (- length p15))
                                   (traverse r14 (+ index p14) (- p15 p14))
                                   (traverse r13 (+ index p13) (- p14 p13))
                                   (traverse r12 (+ index p12) (- p13 p12))
                                   (traverse r11 (+ index p11) (- p12 p11))
                                   (traverse r10 (+ index p10) (- p11 p10))
                                   (traverse r9 (+ index p9) (- p10 p9))
                                   (traverse r8 (+ index p8) (- p9 p8))
                                   (traverse r7 (+ index p7) (- p8 p7))
                                   (traverse r6 (+ index p6) (- p7 p6))
                                   (traverse r5 (+ index p5) (- p6 p5))
                                   (traverse r4 (+ index p4) (- p5 p4))
                                   (traverse r3 (+ index p3) (- p4 p3))
                                   (traverse r2 (+ index p2) (- p3 p2))
                                   (traverse r1 (+ index p1) (- p2 p1))
                                   (traverse rest (+ index p1) p1)))))
                    (traverse ,list-var ,start-var ,length-var)))
           (progn (if (null ,start-var)
                      (setf ,start-var 0)
                      (setf ,list-var (nthcdr ,start-var ,list-var)))
                  (loop for ,iter-var of-type fixnum from ,start-var
                        for ,rest-var = ,list-var then (cdr ,rest-var)
                        do (if (atom ,rest-var)
                               (if (null ,rest-var)
                                   (if (and (not (null ,end-var))
                                            (> ,end-var ,iter-var))
                                       (error "END too large")
                                       (loop-finish))
                                   (error "List must be proper"))
                               (if (and (not (null ,end-var))
                                        (>= ,iter-var ,end-var))
                                   (loop-finish)
                                   (let ((,cons-var ,rest-var)
                                         (,index-var ,iter-var))
                                     (declare (ignorable ,index-var))
                                     ,@body)))))))))

(defmacro canonicalize-test-and-test-not (test-var test-not-var caller-name)
  `(cond ((and (not (null ,test-var)) (not (null ,test-not-var)))
          (error 'both-test-and-test-not-given
                 :name ',caller-name))
         ((null ,test-var)
          (cond ((null ,test-not-var)
                 (setf ,test-var #'eql))
                ((not (functionp ,test-not-var))
                 (setf ,test-not-var (fdefinition ,test-not-var)))
                (t nil)))
         ((null ,test-not-var)
          (unless (functionp ,test-var)
            (setf ,test-var (fdefinition ,test-var))))
         (t nil)))

(defun make-situation (vector-var type accessor-name body)
  `((typep ,vector-var ',type)
    (locally (declare (type ,type ,vector-var))
      (macrolet ((vref (vector index)
                   `(,',accessor-name ,vector ,index)))
        ,@body))))

(defun verify-bounding-indices-list (start end)
  (unless (typep start 'fixnum)
    (error 'invalid-start-index-type
           :datum start
           :expected-type 'fixnum))
  (unless (typep end '(or null fixnum))
    (error 'invalid-end-index-type
           :datum start
           :expected-type '(or null fixnum)))
  (when (and (typep end 'fixnum)
             (< end start))
    (error 'end-less-than-start
           :datum end
           :expected-type `(integer ,start))))

;;; This function is used when the sequence is a vector of some kind
;;; in order to verify that start and end are valid bounding indexes.
(defun verify-bounding-indices-vector (vector start end)
  (let ((length (length vector)))
    (when (> start length)
      (error 'invalid-start-index
             :datum start
             :expected-type `(integer 0 ,length)
             :in-sequence vector))
    (unless (<= 0 end length)
      (error 'invalid-end-index
             :datum end
             :expected-type `(integer 0 ,length)
             :in-sequence vector))
    (unless (<= start end)
      (error 'end-less-than-start
             :datum start
             :expected-type `(integer 0 ,end)
             :end-index end
             :in-sequence vector))))
