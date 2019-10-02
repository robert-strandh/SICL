(cl:in-package #:sicl-sequence-tiny)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Split a list into prefix and rest, as indicated by the
;;; bounding index designator START.  Return three values:
;;;
;;;   * The last cons cell of the prefix, or NIL if the prefix is empty.
;;;     If this is a cons cell, then its CDR points to the rest of
;;;     the list.  In other words, the list was not modified.
;;;
;;;   * The rest of the list.  This could be a list, in which case
;;;     the prefix has exactly START cons cells.  Or it could be
;;;     an atom, in which case the prefix may or may not contain
;;;     START cons cells.
;;;
;;;   * The number of cons cells in the prefix returned as the first
;;;     return value.  If this value is not the same as START
;;;     (in which case it is less than START but greater than or equal
;;;     to 0), then the list contains fewer than START cons cells.
;;;
;;; This function always succeeds, so it is safe to call it on any
;;; object, and it is guaranteed that no condition is signaled.

(defun split-list (list start)
  (let ((last-prefix-cell nil)
        (prefix-length 0)
        (remainder list))
    (loop until (atom remainder)
          repeat start
          do (incf prefix-length)
             (setf last-prefix-cell remainder)
             (setf remainder (cdr remainder)))
    (values last-prefix-cell remainder prefix-length)))

;;; Compute the length of a vector.
(defun vector-length (vector)
  (if (array-has-fill-pointer-p vector)
      (fill-pointer vector)
      (array-dimension vector 0)))

;;; For a vector and a pair of bouding index designators,
;;; check that the boudning index designators are valid,
;;; and signal the appropriate error condition if not.
(defun check-bounding-index-designators-of-vector (client vector start end)
  (cond ((not (and (integerp start) (>= start 0)))
         (error 'invalid-start-index-type
                :datum start
                :expected-type '(integer 0)))
        ((> start (vector-length vector))
         (error 'invalid-start-index
                :datum start
                :expected-type `(integer 0 ,(vector-length vector))
                :in-sequence vector))
        ((not (or (null end) (and (integerp end) (>= end 0))))
         (error 'invalid-end-index-type
                :datum end
                :expected-type '(or null (integer 0))))
        ((and (integerp end) (> end (vector-length vector)))
         (error 'invalid-end-index
                :datum end
                :expected-type `(integer 0 ,(vector-length vector))
                :in-sequence vector))
        ((and (integerp end) (< end start))
         (error 'end-less-than-start
                :datum start
                :end-index end
                :in-sequence vector))))

;;; For a list and a pair of bouding index designators,
;;; check that the boudning index designators are valid,
;;; and signal the appropriate error condition if not.
;;; This function does a lot less checking than the
;;; corresponding one for vectors, because we do not want
;;; to traverse the list to figure out its length
(defun check-bounding-index-designators-of-list (client list start end)
  (cond ((not (and (integerp start) (>= start 0)))
         (error 'invalid-start-index-type
                :datum start
                :expected-type '(integer 0)))
        ((not (or (null end) (and (integerp end) (>= end 0))))
         (error 'invalid-end-index-type
                :datum end
                :expected-type '(or null (integer 0))))
        ((and (integerp end) (< end start))
         (error 'end-less-than-start
                :datum start
                :end-index end
                :in-sequence list))))

;;; Map a function over the cells of the prefix of a list
;;; and then return the remainder of the list.
;;; The prefix of a list consists of the first START cells
;;; of the list.  If there are fewer than START cells in
;;; the list, then signal an error, either because the list
;;; is not proper, or because it is too short.
(defun map-over-prefix-cells (client fun list start)
  (when (not (and (integerp start) (>= start 0)))
    (error 'invalid-start-index-type
           :datum start
           :expected-type '(integer 0)))
  (let ((remaining list))
    (loop repeat start
          do (cond ((null remaining)
                    (error 'invalid-start-index
                           :datum start
                           :in-sequence list))
                   ((atom remaining)
                    (error 'must-be-proper-list
                           :datum list
                           :expected-type 'list))
                   (t
                    (funcall fun remaining)
                    (pop remaining))))
    remaining))

;;; Map a function over the cells of the interval of a list
;;; and then return the suffix of the list.
;;; The interval of a list consists of the cells between
;;; the START and the END bounding index designators.
;;; This function takes both the original list and that list
;;; with the prefix removed.  The original list is only used
;;; for condition reporting.
;;; If there are fewer than END - START cells in the interval,
;;; then signal an error either becaue the list is not proper,
;;; or because it is too short.
;;; It is assumed that START has already been checked, so that
;;; it is a valid bounding index designator for the list.
(defun map-over-interval-cells (client fun list remainder start end)
  (when (not (typep end '(or null (integer 0))))
    (error 'invalid-end-index-type
           :datum end
           :expected-type '(or null (integer 0))))
  (let ((remaining remainder))
    (loop until (if (null end)
                    (null remaining)
                    (= start end))
          do (cond ((null remaining)
                    (error 'invalid-end-index
                           :datum end
                           :in-sequence list))
                   ((atom remaining)
                    (error 'must-be-proper-list
                           :datum list
                           :expected-type 'list))
                   (t
                    (funcall fun remaining)
                    (incf start)
                    (pop remaining))))
    remaining))

;;; Extract the cells of the prefix and the interval of a list.
;;; This is useful when some processing needs to be done from the
;;; end of the list.
(defun extract-list-parts (client list start end)
  (let* ((prefix-cells '())
         (start-of-interval (map-over-prefix-cells client
                                                   (lambda (cell)
                                                     (push cell prefix-cells))
                                                   list
                                                   start))
         (interval-cells '())
         (start-of-suffix (map-over-interval-cells client
                                                   (lambda (cell)
                                                     (push cell interval-cells))
                                                   list
                                                   start-of-interval
                                                   start
                                                   end)))
    (values prefix-cells interval-cells start-of-suffix)))

(defun my-mapc (fun list)
  (loop for element in list
        do (funcall fun element)))

(defun map-over-list-cells-from-start (client list start end
                                       prefix-fun interval-fun suffix-fun)
  (let* ((start-of-interval (map-over-prefix-cells client
                                                   prefix-fun
                                                   list
                                                   start))
         (start-of-suffix (map-over-interval-cells client
                                                   interval-fun
                                                   list
                                                   start-of-interval
                                                   start
                                                   end)))
    (funcall suffix-fun start-of-suffix)))

(defun map-over-list-cells-from-end (client prefix-fun interval-fun suffix-fun
                                     list start end)
  (multiple-value-bind (prefix-cells interval-cells suffix)
      (extract-list-parts client list start end)
    (funcall suffix-fun suffix)
    (mapc interval-fun interval-cells)
    (mapc prefix-fun prefix-cells)))

;;; Take a vector, a function and an interval, as defined by
;;; the bounding index designators START and END, and map
;;; the function over the elements of the interval from the
;;; start of the interval to the end of the interval.
;;; The function to be mapped should return two values:
;;;   - A generalized boolean indicating whether the
;;;     original element should be replaced.
;;;   - A replacement object to be used when the first
;;;     value is true.
(defun map-over-interval-from-start-vector (client vector fun start end)
  (check-bounding-index-designators-of-vector client vector start end)
  (when (null end)
    (setf end (vector-length vector)))
  (loop for index from start below end
        do (multiple-value-bind (replacep new)
               (funcall fun (aref vector index))
             (when replacep
               (setf (aref vector index) new)))))

;;; Take a list, a function and an interval, as defined by
;;; the bounding index designators START and END, and map
;;; the function over the elements of the interval from the
;;; start of the interval to the end of the interval.
;;; The function to be mapped should return two values:
;;;   - A generalized boolean indicating whether the
;;;     original element should be replaced.
;;;   - A replacement object to be used when the first
;;;     value is true.
(defun map-over-interval-from-start-list (client list fun start end)
  (check-bounding-index-designators-of-list client list start end)
  ;; skip over a prefix indicated by start
  (multiple-value-bind (last-prefix-cell remainder prefix-length)
      (split-list list start)
    (declare (ignore last-prefix-cell))
    (when (< prefix-length start)
      (error 'invalid-start-prefix
             :datum start
             :expected-type 'integer
             :in-sequence list))
    (if (null end)
        ;; do the rest of the list
        (loop until (null remainder)
              do (cond ((atom remainder)
                        (error 'must-be-proper-list
                               :datum list
                               :expected-type 'list))
                       (t
                        (multiple-value-bind (replacep new)
                            (funcall fun (car remainder))
                          (when replacep
                            (setf (car remainder) new))
                          (pop remainder)))))
        ;; do until end of interval
        (if (< end start)
            (error 'end-less-than-start
                   :datum start
                   :expected-type 'integer
                   :in-sequence list
                   :end-index end)
            (loop repeat (- end start)
                  do (cond ((null remainder)
                            (error 'invalid-end-index
                                   :datum end
                                   :expected-type 'integer
                                   :in-sequence list))
                           ((atom remainder)
                            (error 'must-be-proper-list
                                   :datum list
                                   :expected-type 'list))
                           (t
                            (multiple-value-bind (replacep new)
                                (funcall fun (car remainder))
                              (when replacep
                                (setf (car remainder) new))
                              (pop remainder)))))))))

;;; Take a sequence, a function and an interval, as defined by
;;; the bounding index designators START and END, and map
;;; the function over the elements of the interval from the
;;; start of the interval to the end of the interval.
;;; The function to be mapped should return two values:
;;;   - A generalized boolean indicating whether the
;;;     original element should be replaced.
;;;   - A replacement object to be used when the first
;;;     value is true.
(defun map-over-interval-from-start (client sequence fun start end)
  (cond ((vectorp sequence)
         (map-over-interval-from-start-vector client sequence fun start end))
        ((listp sequence)
         (map-over-interval-from-start-list client sequence fun start end))
        (t
         ;; Do this better by defining a condition with a client.
         (error 'type-error
                :datum sequence
                :expected-type '(or list vector)))))

;;; Take a vector, a function and an interval, as defined by
;;; the bounding index designators START and END, and map
;;; the function over the elements of the interval from the
;;; end of the interval to the start of the interval.
;;; The function to be mapped should return two values:
;;;   - A generalized boolean indicating whether the
;;;     original element should be replaced.
;;;   - A replacement object to be used when the first
;;;     value is true.
(defun map-over-interval-from-end-vector (client vector fun start end)
  (check-bounding-index-designators-of-vector client vector start end)
  (when (null end)
    (setf end (vector-length vector)))
  (loop for index downfrom (1- end) to start
        do (multiple-value-bind (replacep new)
               (funcall fun (aref vector index))
             (when replacep
               (setf (aref vector index) new)))))

;;; Take a list, a function and an interval, as defined by
;;; the bounding index designators START and END, and map
;;; the function over the elements of the interval from the
;;; end of the interval to the start of the interval.
;;; The function to be mapped should return two values:
;;;   - A generalized boolean indicating whether the
;;;     original element should be replaced.
;;;   - A replacement object to be used when the first
;;;     value is true.
(defun map-over-interval-from-end-list (client list fun start end)
  (multiple-value-bind (last-prefix-cell remainder prefix-length)
      (split-list list start)
    (declare (ignore last-prefix-cell))
    (when (< prefix-length start)
      (error 'invalid-start-prefix
             :datum start
             :expected-type 'integer
             :in-sequence list))
    (let ((interval '()))
      (if (null end)
          ;; do the rest of the list
          (loop until (null remainder)
                do (cond ((atom remainder)
                          (error 'must-be-proper-list
                                 :datum list
                                 :expected-type 'list))
                         (t
                          (push remainder interval)
                          (pop remainder))))
          ;; do until end of interval
          (if (< end start)
              (error 'end-less-than-start
                     :datum start
                     :expected-type 'integer
                     :in-sequence list
                     :end-index end)
              (loop repeat (- end start)
                    do (cond ((null remainder)
                              (error 'invalid-end-index
                                     :datum end
                                     :expected-type 'integer
                                     :in-sequence list))
                             ((atom remainder)
                              (error 'must-be-proper-list
                                     :datum list
                                     :expected-type 'list))
                             (t
                              (push remainder interval)
                              (pop remainder))))))
      ;; Interval now contains a list of the cons cells in
      ;; the original interval, but in reverse order.
      (loop for cell in interval
            do (multiple-value-bind (replacep new)
                   (funcall fun (car cell))
                 (when replacep
                   (setf (car cell) new)))))))

;;; Take a sequence, a function and an interval, as defined by
;;; the bounding index designators START and END, and map
;;; the function over the elements of the interval from the
;;; end of the interval to the start of the interval.
;;; The function to be mapped should return two values:
;;;   - A generalized boolean indicating whether the
;;;     original element should be replaced.
;;;   - A replacement object to be used when the first
;;;     value is true.
(defun map-over-interval-from-end (client sequence fun start end)
  (cond ((not (numberp start))
         (error "start must be a number"))
        ((minusp start)
         (error "start must be nonnegative"))
        ((not (or (null end) (numberp end)))
         (error "end must be NIL or a number"))
        ((and (numberp end) (minusp end))
         (error "end must be nonnnegative"))
        ((vectorp sequence)
         (map-over-interval-from-end-vector client sequence fun start end))
        ((listp sequence)
         (map-over-interval-from-end-list client sequence fun start end))
        (t
         (error "sequence must be a vector or a list"))))

;;; From a bunch of keyword arguments, extract the
;;; TEST, TEST-NOT, and KEY keyword arguments, and
;;; use them to compute a single test function with
;;; two parameters: an item and an element of the
;;; sequence.  This function returns true when
;;; the "test is satisfied" according to section
;;; 17.2.1 of the Common Lisp HyperSpec.
(defun compute-test-from-tests-and-key (client
                                        item
                                        &key
                                        (test nil test-p)
                                        (test-not nil test-not-p)
                                        (key nil key-p)
                                        &allow-other-keys)
  (if (and test-p test-not-p)
      (error 'both-test-and-test-not-given)
      (if test-p
          (if key-p
              (lambda (element) (funcall test item (funcall key element)))
              test)
          (if test-not-p
              (if key-p
                  (lambda (element) (not (funcall test-not item (funcall key element))))
                  (complement test-not))
              (if key-p
                  (lambda (element) (eql item (funcall key element)))
                  #'eql)))))

(defun compute-test-from-predicate-and-key (predicate key)
  (if (null key)
      predicate
      (lambda (element) (funcall predicate (funcall key element)))))

(defun compute-test-from-negated-predicate-and-key (predicate key)
  (if (null key)
      (lambda (element) (not (funcall predicate element)))
      (lambda (element) (not (funcall predicate (funcall key element))))))

;;; Turn a sequence into an iterator function.
;;; An iterator function in this context is a thunk, i.e. a fucnction
;;; of zero arguments that returns two values.  The first value
;;; is the next element of the sequence, or nil if there are no
;;; more elements left in the sequence.  The second value is a
;;; keyword symbol and is one of :NORMAL, :END, and :NOT-PROPER.
;;; If there are still elements left in the sequence, then the
;;; second value returned will be :NORMAL next time the iterator
;;; is called.  If there are no more elements left in the sequence,
;;; but the sequence is proper, then the second value returned will
;;; be :END next time the iterator is called.  Finally, if there
;;; are no more elements left in the sequence, and the sequence is
;;; not proper, then the second value returned will be :NOT-PROPER
;;; next time the iterator is called.

(defun make-sequence-iterator (sequence)
  (if (vectorp sequence)
      (let ((index 0)
            (length (vector-length sequence)))
        (lambda ()
          (if (= index length)
              (values nil :end)
              ;; We can't use prog1 because it returns only the
              ;; primary value of the first form :-(
              (let ((temp (aref sequence index)))
                (incf index)
                (values temp :normal)))))
      (let ((remaining sequence))
        (lambda ()
          (if (null remaining)
              (values nil :end)
              (if (atom remaining)
                  (values :nil :not-proper)
                  (multiple-value-prog1
                      (values (car remaining) :normal)
                    (pop remaining))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code for the sequence functions.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-seq

;;; This code is the same as in the "fast" version of this module.
;;; We need to figure out a way of factoring out common code.

;;; We test for circular lists.
;;; We can afford to do this because the cost is small compared to
;;; the cost of allocating and initializing cons cells.
;;; Furthermore, this is a good idea, because either way, we fail
;;; on circular lists, so we might as well fail with an error message.

;;; Break out the core of copy-seq into an auxiliary function
;;; that can be used by other functions in this module.

(defun copy-seq-aux (client sequence)
  (cond ((vectorp sequence)
         (let ((result (make-array (length sequence)
                                   :element-type (array-element-type sequence))))
           (loop for i from 0 below (length sequence)
                 do (setf (aref result i) (aref sequence i)))
           result))
        ((null sequence)
         '())
        ((atom sequence)
         (error 'must-be-sequence
                :datum sequence))
        (t
         ;; The sequence is a non-empty list.
         (let* ((fast (cdr sequence))
                (slow sequence)
                (result (cons (car sequence) nil))
                (last result))
           (loop until (or (eq slow fast) (atom fast))
                 do (setf (cdr last) (cons (pop fast) nil)
                          last (cdr last))
                 until (atom fast)
                 do (setf (cdr last) (cons (pop fast) nil)
                          last (cdr last)
                          slow (cdr slow)))
           (cond ((null fast)
                  result)
                 (t
                  (error 'must-be-proper-list
                         :datum sequence)))))))

(defun copy-seq (sequence)
  (copy-seq-aux 'copy-seq sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function length

;;; Compute the length of a proper list, or signal an error if
;;; the list is a dotted list.
(defun length-of-proper-list (name list)
  (loop for remainder = list then (cdr remainder)
        for length from 0
        while (consp remainder)
        finally (if (null remainder)
                    (return length)
                    (error 'must-be-proper-list
                           :datum list))))

(defun length-of-proper-sequence (name sequence)
  (if (vectorp sequence)
      (if (array-has-fill-pointer-p sequence)
          (fill-pointer sequence)
          (array-dimension sequence 0))
      (length-of-proper-list name sequence)))

(defun length (sequence)
  (length-of-proper-sequence 'length sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessor elt

(defun elt (sequence index)
  (unless (typep index 'unsigned-byte)
    (error 'invalid-sequence-index
           :datum index
           :expected-type '(integer 0)
           :in-sequence sequence))
  (if (listp sequence)
      (multiple-value-bind (last-prefix-cell remainder prefix-length)
          (split-list sequence index)
        (declare (ignore last-prefix-cell))
        (cond ((null remainder)
               (error 'invalid-sequence-index
                      :datum index
                      :expected-type `(integer 0 ,(1- prefix-length))
                      :in-sequence sequence))
              ((atom remainder)
               (error 'must-be-proper-list
                      :datum sequence
                      :expected-type 'list))
              (t
               (car remainder))))
      (if (>= index (vector-length sequence))
          (error 'invalid-sequence-index
                 :datum index
                 :expected-type `(integer 0 ,(1- (vector-length sequence)))
                 :in-sequence sequence)
          (aref sequence index))))

(defun (setf elt) (new-object sequence index)
  (unless (typep index 'unsigned-byte)
    (error 'invalid-sequence-index
           :datum index
           :expected-type '(integer 0)
           :in-sequence sequence))
  (if (listp sequence)
      (multiple-value-bind (last-prefix-cell remainder prefix-length)
          (split-list sequence index)
        (declare (ignore last-prefix-cell))
        (cond ((null remainder)
               (error 'invalid-sequence-index
                      :datum index
                      :expected-type `(integer 0 ,(1- prefix-length))
                      :in-sequence sequence))
              ((atom remainder)
               (error 'must-be-proper-list
                      :datum sequence
                      :expected-type 'list))
              (t
               (setf (car remainder) new-object))))
      (if (>= index (vector-length sequence))
          (error 'invalid-sequence-index
                 :datum index
                 :expected-type `(integer 0 ,(1- (vector-length sequence)))
                 :in-sequence sequence)
          (setf (aref sequence index) new-object)))
  new-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function fill.

(defun fill (sequence item &key (start 0) end)
  (map-over-interval-from-start 'fill
                                sequence
                                (lambda (element)
                                  (declare (ignore element))
                                  (values t item))
                                start
                                end)
  sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions nreverse, reverse

;;; Break out the core of nreverse into an auxiliary function
;;; that can be used by other functions in this module.

(defun nreverse-aux (client sequence)
  (if (listp sequence)
      (let ((result '())
            (remaining sequence))
        (loop until (atom remaining)
              do (let ((temp (cdr remaining)))
                   (setf (cdr remaining) result)
                   (setf result remaining)
                   (setf remaining temp))
              finally (unless (null remaining)
                        (error 'must-be-proper-list
                               :datum sequence
                               :expected-type 'list)))
        result)
      (let ((length (vector-length sequence)))
        (loop for i from 0 below (floor length 2)
              do (rotatef (aref sequence i) (aref sequence (- length i 1))))
        sequence)))

(defun nreverse (sequence)
  (nreverse-aux 'nreverse sequence))

(defun reverse (sequence)
  (nreverse-aux 'reverse (copy-seq-aux 'reverse sequence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function subseq.

(defun subseq (sequence start &optional end)
  (let ((elements '())
        (length 0))
    (map-over-interval-from-start 'subseq
                                  sequence
                                  (lambda (element)
                                    (push element elements)
                                    (incf length)
                                    nil)
                                  start
                                  end)
    (if (listp sequence)
        (nreverse elements)
        (let ((result (make-array length
                                  :element-type (array-element-type sequence))))
          ;; FIXME: factor this out and use REPLACE instead
          (loop for i from 0
                for element in (nreverse elements)
                do (setf (aref result i) element))
          result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function reduce.

(defun reduce (function sequence
               &key key from-end (start 0) end (initial-value nil initialp))
  (when (null key)
    (setf key #'identity))
  (let ((never-called-p t)
        (result nil))
    (if from-end
        (map-over-interval-from-end
         'reduce
         sequence
         (lambda (element)
           (setf result
                 (if never-called-p
                     (progn (setf never-called-p nil)
                            (if initialp
                                (funcall function
                                         (funcall key element)
                                         initial-value)
                                (funcall key element)))
                     (funcall function
                              (funcall key element)
                              result)))
           ;; This function should always return nil.
           nil)
         start end)
        (map-over-interval-from-start
         'reduce
         sequence
         (lambda (element)
           (setf result
                 (if never-called-p
                     (progn (setf never-called-p nil)
                            (if initialp
                                (funcall function
                                         initial-value
                                         (funcall key element))
                                (funcall key element)))
                     (funcall function
                              result
                              (funcall key element))))
           ;; This function should always return nil.
           nil)
         start end))
    (if never-called-p
        ;; Then the sequence is empty.
        (if initialp
            ;; Initial value given.  Return it.
            initial-value
            ;; No initial value given. Call the function
            ;; with no arguments and return the result
            (funcall function))
        ;; The sequence was not empty.
        result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function replace.

;;; Entry point to be used when both sequences are vectors.
(defun replace-to-vector-from-vector (client v1 v2 start1 end1 start2 end2)
  (check-bounding-index-designators-of-vector client v1 start1 end1)
  (check-bounding-index-designators-of-vector client v2 start2 end2)
  (when (null end1) (setf end1 (vector-length v1)))
  (when (null end2) (setf end2 (vector-length v2)))
  (if (and (eq v1 v2) (> start1 start2))
      ;; Then copy from end to start.
      (progn
        ;; Make sure the two intervals have the same length.
        (when (< (- end1 start1) (- end2 start2))
          (setf end2 (+ start2 (- end1 start1))))
        (when (< (- end2 start2) (- end1 start1))
          (setf end1 (+ start1 (- end2 start2))))
        (loop for i1 downfrom (1- end1) to start1
              for i2 downfrom (1- end2) to start2
              do (setf (aref v1 i1) (aref v2 i2))))
      ;; Else copy from start to end.
      (loop for i1 from start1 below end1
            for i2 from start2 below end2
            do (setf (aref v1 i1) (aref v2 i2)))))

(defun replace-to-list-from-vector (client list vector start1 end1 start2 end2)
  (check-bounding-index-designators-of-vector client vector start2 end2)
  (let ((index start2))
    (map-over-interval-from-start-list
     client
     list
     (lambda (element)
       (declare (ignore element))
       (when (>= index end2)
         (return-from replace-to-list-from-vector nil))
       (values t (prog1 (aref vector index) (incf index))))
     start1
     end1)))

(defun replace-to-vector-from-list (client vector list start1 end1 start2 end2)
  (check-bounding-index-designators-of-vector client vector start2 end2)
  (let ((index start2))
    (map-over-interval-from-start-list
     client
     list
     (lambda (element)
       (when (>= index end2)
         (return-from replace-to-vector-from-list nil))
       (setf (aref vector index) element)
       (incf index)
       nil)
     start1
     end1)))

(defun replace-to-list-from-list (client l1 l2 start1 end1 start2 end2)
  (if (eq l1 l2)
      ;; The HyperSpec says that it must be "as if he entire source
      ;; region were copied to another place and only then copied back
      ;; into the target region".  Since the list may be circular,
      ;; I don't see any way of doing it other than following the
      ;; wording of the HyperSpec to the letter.
      ;;
      ;; Start by skipping a prefix that corresponds to the smallest
      ;; of start1 and star2.
      (multiple-value-bind (last-prefix-cell remainder prefix-length)
          (split-list l1 (min start1 start2))
        (declare (ignore last-prefix-cell))
        (when (< prefix-length (min start1 start2))
          (if (null remainder)
              (error 'invalid-sequence-index
                     :datum (min start1 start2)
                     :in-sequence l1)
              (error 'must-be-proper-list
                     :datum l1)))
        (cond ((and (null end1) (null end2))
               ;; If both end1 and end2 are NIL, then the largest one
               ;; of start1 and start2 determines which interval is
               ;; the shortest.
               (cond ((> start2 start1)
                      ;; If start2 is larger than start1, then extract
                      ;; the entire source interval and then copy it
                      ;; to the target place.

                      ;; If start1 is larger, then extract only as
                      ;; many elements from the source interval as is
                      ;; determined by the size of the target
                      ;; interval.
                      )))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function concatenate.

;;; FIXME: shadow concatenate

(defun my-concatenate (result-type &rest sequences)
  (let* ((length (loop for sequence in sequences
                      sum (length-of-proper-sequence 'concatenate sequence)))
         ;; FIXME: make-sequence can signal an error
         (result (make-sequence result-type length)))
    (if (listp result)
        (let ((remaining result))
          (loop for sequence in sequences
                do (map-over-interval-from-start
                       'concatenate
                       sequence
                       (lambda (element)
                         (setf (car remaining) element)
                         (pop remaining)
                         nil)
                       0 nil)))
        (let ((index 0))
          (loop for sequence in sequences
                do (map-over-interval-from-start
                       'concatenate
                       sequence
                       (lambda (element)
                         (setf (aref result index) element)
                         (incf index)
                         nil)
                       0 nil))))
    result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions find, find-if, find-if-not.

(defun find-common (client sequence test-and-key from-end start end)
  (funcall (if from-end
               #'map-over-interval-from-end
               #'map-over-interval-from-start)
           client
           sequence
           (lambda (element)
             (when (funcall test-and-key element)
               (return-from find-common element))
             ;; This function should always return nil
             nil)
           start
           end)
  nil)

(defun find (item sequence &rest args
             &key from-end test test-not (start 0) end key)
  (declare (ignore test test-not key))
  (find-common
   'find
   sequence
   (apply #'compute-test-from-tests-and-key 'find item args)
   from-end start end))

(defun find-if (predicate sequence &key from-end (start 0) end key)
  (find-common
   'find-if
   sequence
   (compute-test-from-predicate-and-key predicate key)
   from-end start end))

(defun find-if-not (predicate sequence &key from-end (start 0) end key)
  (find-common
   'find-if-not
   sequence
   (compute-test-from-negated-predicate-and-key predicate key)
   from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions position, position-if, position-if-not.

(defun position-common (client sequence test-and-key from-end start end)
  (let ((position 0))
    (funcall (if from-end
                 #'map-over-interval-from-end
                 #'map-over-interval-from-start)
             client
             sequence
             (lambda (element)
               (if (funcall test-and-key element)
                   (return-from position-common position)
                   (incf position))
               ;; This function should always return nil
               nil)
             start
             end))
  nil)

(defun position (item sequence &rest args
             &key from-end test test-not (start 0) end key)
  (declare (ignore test test-not key))
  (position-common
   'position
   sequence
   (apply #'compute-test-from-tests-and-key 'position item args)
   from-end start end))

(defun position-if (predicate sequence &key from-end (start 0) end key)
  (position-common
   'position-if
   sequence
   (compute-test-from-predicate-and-key predicate key)
   from-end start end))

(defun position-if-not (predicate sequence &key from-end (start 0) end key)
  (position-common
   'position-if-not
   sequence
   (compute-test-from-negated-predicate-and-key predicate key)
   from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions count, count-if, count-if-not.

(defun count-common (client sequence test-and-key from-end start end)
  (let ((count 0))
    (funcall (if from-end
                 #'map-over-interval-from-end
                 #'map-over-interval-from-start)
             client
             sequence
             (lambda (element)
               (when (funcall test-and-key element)
                 (incf count))
               ;; This function should always return nil
               nil)
             start
             end)
    count))

(defun count (item sequence &rest args
             &key from-end test test-not (start 0) end key)
  (declare (ignore test test-not key))
  (count-common
   'count
   sequence
   (apply #'compute-test-from-tests-and-key 'count item args)
   from-end start end))

(defun count-if (predicate sequence &key from-end (start 0) end key)
  (count-common
   'count-if
   sequence
   (compute-test-from-predicate-and-key predicate key)
   from-end start end))

(defun count-if-not (predicate sequence &key from-end (start 0) end key)
  (count-common
   'count-if-not
   sequence
   (compute-test-from-negated-predicate-and-key predicate key)
   from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions remove, remove-if, remove-if-not
;;; Functions deletete, deletete-if, deletete-if-not

;;; For vectors, the behavior of the remove and delete family
;;; functions is the same.  When no elements are removed,
;;; the remove functions are allowed to return the original
;;; vector just like delete, and when elements are removed,
;;; the delete functions cannot reuse the original vector,
;;; so must create another one, just like remove.

(defun remove-delete-vector-common (client vector test-and-key from-end start end count)
  (let ((remaining-interval-elements '())
        (delete-count 0))
    (if from-end
        (map-over-interval-from-end client
                                    vector
                                    (lambda (element)
                                      (if (and (< delete-count count)
                                               (funcall test-and-key element))
                                          (incf delete-count)
                                          (push element remaining-interval-elements)))
                                    start end)
        (progn
          (map-over-interval-from-start client
                                        vector
                                        (lambda (element)
                                          (if (and (< delete-count count)
                                                   (funcall test-and-key element))
                                              (incf delete-count)
                                              (push element remaining-interval-elements)))
                                        start end)
          (setf remaining-interval-elements
                (nreverse remaining-interval-elements))))
    (let ((result (make-array (- (vector-length vector) delete-count)
                              :element-type (array-element-type vector))))
      (replace result vector :start1 0 :start2 0 :end2 start)
      (replace result remaining-interval-elements :start1 start)
      (unless (null end)
        (replace result vector :start1 (- end delete-count) :start2 end))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions substitute, substitute-if, substitute-if-not
;;; Functions nsubstitutete, nsubstitutete-if, nsubstitutete-if-not

(defun nsubstitute-common (client newitem sequence test-and-key from-end start end count)
  (let ((substitute-count 0))
    (funcall (if from-end
                 #'map-over-interval-from-end
                 #'map-over-interval-from-start)
             client
             sequence
             (lambda (element)
               (when (and (numberp count) (>= substitute-count count))
                 (return-from nsubstitute-common sequence))
               (if (funcall test-and-key element)
                   (progn (incf substitute-count)
                          (values t newitem))
                   nil))
             start
             end)
    sequence))

(defun nsubstitute (newitem olditem sequence &rest args
                    &key from-end test test-not (start 0) end key count)
  (declare (ignore test test-not key))
  (nsubstitute-common
   'nsubstitute
   newitem
   sequence
   (apply #'compute-test-from-tests-and-key 'nsubstitute olditem args)
   from-end start end count))

(defun nsubstitute-if (newitem predicate sequence
                       &key from-end (start 0) end key count)
  (nsubstitute-common
   'nsubstitute-if
   newitem
   sequence
   (compute-test-from-predicate-and-key predicate key)
   from-end start end count))

(defun nsubstitute-if-not (newitem predicate sequence
                           &key from-end (start 0) end key count)
  (nsubstitute-common
   'nsubstitute-if-not
   newitem
   sequence
   (compute-test-from-negated-predicate-and-key predicate key)
   from-end start end count))

(defun substitute-common-list-from-start (client newitem list test-and-key start end count)
  (let* (;; The result of the operation is the CDR of the sentinel.
         ;; The sentinel exists only to simplify operations
         (sentinel (cons nil list))
         ;; This variable points to the last cons cell that is not
         ;; shared between the original list and the result, so is
         ;; part of the result but not of the original list.
         ;; If a substitution is required in the shared tail of the
         ;; list, a copy is made up to that point and starting from
         ;; the this cons cell.
         (last sentinel)
         ;; Counts the number of cells that must be copied should we
         ;; detect an element that satisifies the test in the shard
         ;; tail of the list.
         (deficit-count 0)
         ;; The number of susbstitutions made.  This is used to
         ;; stop the iteration if this number is greater than or
         ;; equal to the COUNT given as argument.
         (substitute-count 0))
    (map-over-interval-from-start
     client
     list
     (lambda (element)
       ;; When enough substitutions have been made according
       ;; to the COUNT parameter, return the result.
       (when (and (numberp count) (>= substitute-count count))
         (return-from substitute-common-list-from-start (cdr sentinel)))
       (incf deficit-count)
       (when (funcall test-and-key element)
         ;; We copy cells that lie between LAST and the
         ;; one containing this element, but not the one
         ;; containing this element.
         (loop repeat (1- deficit-count)
               do (setf (cdr last)
                        (cons (car last) (cdr last)))
                  (setf last (cdr last)))
         ;; The cons cell containing this element will not be
         ;; copied, and will have NEWITEM in its CAR instead.
         (setf (cdr last)
               (cons newitem (cdr last)))
         (setf last (cdr last))
         (setf deficit-count 0)
         (incf substitute-count))
       nil)
     start
     end)
    (cdr sentinel)))

(defun substitute-common-list-from-end (client newitem list test-and-key start end count)
  (let ((result nil)
        (copy-cells-p nil)
        (substitute-count 0))
    (map-over-list-cells-from-end
     client
     (lambda (cell)
       (when copy-cells-p
         (push (car cell) result)))
     (lambda (cell)
       (cond ((and (or (null count) (< substitute-count count))
                   (funcall test-and-key (car cell)))
              (push newitem result)
              (incf substitute-count)
              (setf copy-cells-p t))
             (copy-cells-p
              (push (car cell) result))
             (t
              nil)))
     (lambda (suffix) (setf result suffix))
     list start end)
    result))

(defun substitute-common-list (client newitem list test-and-key from-end start end count)
  (if from-end
      (substitute-common-list-from-end client newitem list test-and-key start end count)
      (substitute-common-list-from-start client newitem list test-and-key start end count)))

(defun substitute-common-vector (client newitem vector test-and-key from-end start end count)
  (if (not (plusp count))
      vector
      (let (;; We guess that there will be substitutions and allocate a copy.
            ;; If it turns out there are no substitutions, we return the
            ;; original instead and let the GC deal with the copy.
            (result (copy-seq vector))
            ;; Count the number of substitutions made.  This is used for two
            ;; purposes: to stop when COUNT substitutions have been made,
            ;; and to determine whether the copy or the orginal should
            ;; be returned.
            (substitute-count 0))
        (funcall (if from-end
                     #'map-over-interval-from-end
                     #'map-over-interval-from-start)
                 client
                 result
                 (lambda (element)
                   (when (and (numberp count) (>= substitute-count count))
                     ;; Since count is positive here, we must
                     ;; have made at least one substitution.
                     ;; Therefore, the copy should be returned.
                     (return-from substitute-common-vector result))
                   (if (funcall test-and-key element)
                       (progn (incf substitute-count)
                              (values t newitem))
                       nil))
                 start
                 end)
        (if (plusp substitute-count)
            result
            vector))))

(defun substitute-common (client newitem sequence test-and-key from-end start end count)
  (if (vectorp sequence)
      (substitute-common-vector
       client newitem sequence test-and-key from-end start end count)
      (substitute-common-list
       client newitem sequence test-and-key from-end start end count)))

(defun substitute (newitem olditem sequence &rest args
                    &key from-end test test-not (start 0) end key count)
  (declare (ignore test test-not key))
  (substitute-common
   'substitute
   newitem
   sequence
   (apply #'compute-test-from-tests-and-key 'substitute olditem args)
   from-end start end count))

(defun substitute-if (newitem predicate sequence
                       &key from-end (start 0) end key count)
  (substitute-common
   'substitute-if
   newitem
   sequence
   (compute-test-from-predicate-and-key predicate key)
   from-end start end count))

(defun substitute-if-not (newitem predicate sequence
                           &key from-end (start 0) end key count)
  (substitute-common
   'substitute-if-not
   newitem
   sequence
   (compute-test-from-negated-predicate-and-key predicate key)
   from-end start end count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function map.

(defun map (result-type function &rest sequences)
  (let ((iterators (loop for sequence in sequences
                         collect (make-sequence-iterator sequence)))
        (arguments (make-list (length sequences)))
        (results '())
        (any-end-p nil)
        (first-improper nil))
    (flet ((next-elements ()
             (setf any-end-p nil
                   first-improper nil)
             (loop for iterator in iterators
                   for sequence in sequences
                   for arg-cell on arguments
                   do (multiple-value-bind (element validity)
                          (funcall iterator)
                        (ecase validity
                          (:normal (setf (car arg-cell) element))
                          (:end (setf any-end-p t))
                          (:not-proper (setf first-improper sequence)))))))
      (loop do (next-elements)
               (unless (null first-improper)
                 (error 'must-be-proper-list
                        :datum first-improper))
               (if any-end-p
                   ;; This is not optimal because it can signal
                   ;; an error in coerce.
                   (return-from map (coerce (nreverse results) result-type))
                   (push (apply function arguments) results))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function map-into.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function merge.

;;; When the result type is a subset of list, we convert any non-list
;;; argument to a list before doing the merge.  We know that this
;;; function is called with proper lists.
(defun |merge result-type=list| (list1 list2 predicate key)
  (let* ((sentinel (list nil))
         (last sentinel))
    (loop do (cond ((null list1)
                    (setf (cdr last) list2)
                    (return (cdr sentinel)))
                   ((null list2)
                    (setf (cdr last) list1)
                    (return (cdr sentinel)))
                   ((funcall predicate
                             (funcall key (car list1))
                             (funcall key (car list2)))
                    (setf (cdr last) list1)
                    (setf last (cdr last))
                    (setf list1 (cdr list1)))
                   (t
                    (setf (cdr last) list2)
                    (setf last (cdr last))
                    (setf list2 (cdr list2)))))))

;;; For any result type other than list, the merge operation
;;; is not destructive.  We convert any list argument into a
;;; vector before doing the merge.  That way we can use ELT
;;; and LENGTH on both arguments and result.

;;; Merge into a pre-created non-list sequence.
(defun merge-into (result seq1 seq2 predicate key)
  (let ((length1 (length seq1))
        (length2 (length seq2))
        (index1 0)
        (index2 0))
    (loop for index from 0 below (+ length1 length2)
          do (setf (elt result index)
                   (cond ((= index1 length1)
                          (prog1 (elt seq2 index2)
                            (incf index2)))
                         ((= index2 length2)
                          (prog1 (elt seq1 index1)
                            (incf index1)))
                         ((funcall predicate
                                   (funcall key (elt seq1 index1))
                                   (funcall key (elt seq2 index2)))
                          (prog1 (elt seq1 index1)
                            (incf index1)))
                         (t
                          (prog1 (elt seq2 index2)
                            (incf index2))))))
    result))

(defun |merge result-type=other| (result-type seq1 seq2 predicate key)
  (let (;; This is not ideal because there might be an error message
        ;; from make-sequence rather than from merge.
        (result (make-sequence result-type
                               (+ (length seq1) (length seq2)))))
    (merge-into result seq1 seq2 predicate key)))

(defun merge (result-type sequence1 sequence2 predicate &key key)
  (when (null key)
    (setf key #'identity))
  (cond ((subtypep result-type 'list)
         (cond ((listp sequence1)
                (when (not (null (cdr (last sequence1))))
                  (error 'must-be-proper-list
                         :datum sequence1)))
               ((not (typep sequence1 'sequence))
                (error 'must-be-sequence
                       :datum sequence1))
               (t
                (setf sequence1 (coerce sequence1 'list))))
         (cond ((listp sequence2)
                (when (not (null (cdr (last sequence2))))
                  (error 'must-be-proper-list
                         :datum sequence2)))
               ((not (typep sequence2 'sequence))
                (error 'must-be-sequence
                       :datum sequence2))
               (t
                (setf sequence2 (coerce sequence2 'list))))
         (|merge result-type=list| sequence1 sequence2 predicate key))
        (t
         (cond ((listp sequence1)
                (when (not (null (cdr (last sequence1))))
                  (error 'must-be-proper-list
                         :datum sequence1))
                (setf sequence1 (coerce sequence1 'vector)))
               ((not (typep sequence1 'sequence))
                (error 'must-be-sequence
                       :datum sequence1))
               (t
                nil))
         (cond ((listp sequence2)
                (when (not (null (cdr (last sequence2))))
                  (error 'must-be-proper-list
                         :datum sequence2))
                (setf sequence2 (coerce sequence2 'vector)))
               ((not (typep sequence2 'sequence))
                (error 'must-be-sequence
                       :datum sequence2))
               (t
                nil))
         (|merge result-type=other| result-type sequence1 sequence2 predicate key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function sort.
;;; Function stable-sort.

;;; Sort a list of exactly 3 elements.
;;; This algorithm is stable.
(defun |sort seq-type=list length=3| (list predicate key)
  (let* ((temp list)
         (a (pop temp))
         (b (pop temp))
         (c (pop temp)))
    (if (funcall predicate (funcall key a) (funcall key b))
        (cond ((funcall predicate (funcall key b) (funcall key c))
               ;; already sorted
               nil)
              ((funcall predicate (funcall key c) (funcall key a))
               (setf temp list)
               (setf (car temp) c
                     temp (cdr temp)
                     (car temp) a
                     temp (cdr temp)
                     (car temp) b))
              (t
               (setf temp (cdr list))
               (setf (car temp) c
                     temp (cdr temp)
                     (car temp) b)))
        (cond ((funcall predicate (funcall key c) (funcall key b))
               (setf temp list)
               (setf (car temp) c
                     temp (cddr temp)
                     (car temp) a))
              ((funcall predicate (funcall key a) (funcall key c))
               (setf temp list)
               (setf (car temp) b
                     temp (cdr temp)
                     (car temp) a))
              (t
               (setf temp list)
               (setf (car temp) b
                     temp (cdr temp)
                     (car temp) c
                     temp (cdr temp)
                     (car temp) a))))
    list))

;;; Sort a list.
;;; We use the merge-sort algorithm because it is stable,
;;; and especially be cause it is incredebly fast; significantly
;;; faster than quicksort.
(defun |sort seq-type=list|
    (list predicate key)
  (labels ((sort-with-length (list length)
             (case length
               ((0 1) list)
               (2 (let ((temp (cdr list)))
                    (when (funcall predicate
                                   (funcall key (car temp))
                                   (funcall key (car list)))
                      (rotatef (car temp) (car list))))
                  list)
               (3 (|sort seq-type=list length=3| list predicate key))
               (t (let* ((l1 (floor length 2))
                         (l2 (- length l1))
                         (middle (nthcdr (1- l1) list))
                         (second (cdr middle)))
                    (setf (cdr middle) nil)
                    (|merge result-type=list|
                     (sort-with-length list l1)
                     (sort-with-length second l2)
                     predicate key))))))
    (sort-with-length list (length list))))

;;; Sort a subsequence of a sequence (other than a list) containing
;;; exactly three elements.  This algorithm is stable.
(defun |sort seq-type=other length=3|
    (sequence predicate start key)
  (let* ((a (elt sequence start))
         (b (elt sequence (1+ start)))
         (c (elt sequence (+ start 2))))
    (if (funcall predicate a b)
        (cond ((funcall predicate (funcall key b) (funcall key c))
               ;; already sorted
               nil)
              ((funcall predicate (funcall key c) (funcall key a))
               (setf (elt sequence start) c
                     (elt sequence (1+ start)) a
                     (elt sequence (+ start 2)) b))
              (t
               (setf (elt sequence (1+ start)) c
                     (elt sequence (+ start 2)) b)))
        (cond ((funcall predicate (funcall key c) (funcall key b))
               (setf (elt sequence start) c
                     (elt sequence (+ start 2)) a))
              ((funcall predicate (funcall key a) (funcall key c))
               (setf (elt sequence start) b
                     (elt sequence (1+ start)) a))
              (t
               (setf (elt sequence start) b
                     (elt sequence (1+ start)) c
                     (elt sequence (+ start 2)) a))))))

;;; When we sort anything other than a list in-place, we use the
;;; quicksort algorithm, because it does not require any extra storage,
;;; and the SORT function need not be stable.
(defun |sort seq-type=other|
    (sequence predicate key)
  (labels ((sort-interval (start end)
             (declare (type fixnum start end))
             (case (- end start)
               ((0 1) nil)
               (2 (when (funcall predicate
                                 (funcall key (elt sequence (1+ start)))
                                 (funcall key (elt sequence start)))
                    (rotatef (elt sequence (1+ start))
                             (elt sequence start))))
               (3 (|sort seq-type=simple-sequence length=3|
                   sequence predicate start key))
               (t
                  (let* ((middle (floor (+ start end) 2))
                         (pivot (elt sequence middle)))
                    ;; Exclude the pivot element in order
                    ;; to make sure each part is strictly
                    ;; smaller than the whole.
                    (rotatef (elt sequence middle)
                             (elt sequence (1- end)))
                    (let ((i start)
                          (j (- end 2)))
                      (declare (type fixnum i j))
                      (loop while (<= i j)
                            do (loop while (and (<= i j)
                                                (not (funcall predicate
                                                              (funcall key pivot)
                                                              (funcall key (elt sequence i)))))
                                     do (incf i))
                               (loop while (and (<= i j)
                                                (not (funcall predicate
                                                              (funcall key (elt sequence j))
                                                              (funcall key pivot))))
                                     do (decf j))
                               (when (< i j)
                                 (rotatef (elt sequence i) (elt sequence j))
                                 (incf i)
                                 (decf j)))
                      (setf (elt sequence (1- end))
                            (elt sequence i))
                      (setf (elt sequence i) pivot)
                      (sort-interval start i)
                      (sort-interval (1+ i) end)))))
             nil))
    (sort-interval 0 (length sequence)))
  sequence)

(defun sort (sequence predicate &key key)
  (when (null key)
    (setf key #'identity))
  (if (listp sequence)
      (|sort seq-type=list| sequence predicate key)
      (|sort seq-type=other| sequence predicate key)))

;;; We use the merge-sort algorithm for stable-sort on
;;; a sequence other than a list because it is stable.
;;; The main drawback is that it requires extra storage.
(defun |stable-sort seq-type=other| (sequence predicate key)
  (case (length sequence)
    ((0 1) nil)
    (2 (when (funcall predicate
                      (funcall key (elt sequence 0))
                      (funcall key (elt sequence 1)))
         (rotatef (elt sequence 0) (elt sequence 1))))
    (3 (|sort seq-type=other length=3|
        sequence predicate 0 key))
    (t (let* ((length (length sequence))
              (middle (floor length 2))
              (seq1 (subseq sequence 0 middle))
              (seq2 (subseq sequence middle)))
         (|stable-sort seq-type=other| seq1 predicate key)
         (|stable-sort seq-type=other| seq2 predicate key)
         (merge-into sequence seq1 seq2 predicate key))))
  sequence)

(defun stable-sort (sequence predicate &key key)
  (when (null key)
    (setf key #'identity))
  (if (listp sequence)
      ;; Use the sort function which is stable on lists.
      (|sort seq-type=list| sequence predicate key)
      (|stable-sort seq-type=other| sequence predicate key)))
