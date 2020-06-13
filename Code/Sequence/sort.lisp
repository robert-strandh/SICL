(cl:in-package #:sicl-sequence)

(defmethod sort ((list list) predicate &key key)
  (stable-sort list predicate :key key))

;; The interval size below which we switch to shellsort.
(defconstant +introsort-shellsort-threshold+ 11)

(replicate-for-each #1=#:vector-class (simple-vector vector)
  (defmethod sort ((vector #1#) predicate &key key)
    (declare (#1# vector))
    (let ((predicate (function-designator-function predicate)))
      (declare (function predicate))
      (with-key-function (key key)
        (labels
            ;; Sort the interval from START below END using shellsort.
            ((shellsort (start end)
               (declare (vector-length start end))
               (shellsort-one-pass start end 10)
               (shellsort-one-pass start end 4)
               (shellsort-one-pass start end 1))
             ;; Perform a single shellsort pass with the supplied gap.
             (shellsort-one-pass (start end gap)
               (declare (vector-length start end gap))
               (loop for position fixnum from (+ start gap) below end do
                 (let* ((pivot (elt vector position))
                        (pivot-key (key pivot))
                        (index position))
                   (declare (vector-length index))
                   (loop for peek = (elt vector (- index gap))
                         while (funcall predicate pivot-key (key peek))
                         do (setf (elt vector index) peek)
                         do (decf index gap)
                         while (>= index (+ start gap)))
                   (setf (elt vector index) pivot))))
            ;; Sort the interval from START below END using heapsort.
             (heapsort (start end)
               (declare (vector-length start end))
               (let ((n (- end start)))
                 (loop for index fixnum downfrom (floor (1- n) 2) to 0 do
                   (sift-down start index n))
                 (loop for index fixnum downfrom (1- n) to 0 do
                   (rotatef (elt vector start)
                            (elt vector (+ start index)))
                   (sift-down start 0 index))))
             ;; Ensure the heap property on the elements from LEFT below
             ;; RIGHT, relative to START.
             (sift-down (start left right)
               (declare (vector-length start left right))
               (multiple-value-bind (largest key)
                   (let ((child-1 (+ (* 2 left) 1))
                         (child-2 (+ (* 2 left) 2)))
                     (if (>= child-1 right)
                         (return-from sift-down)
                         (if (>= child-2 right)
                             (values child-1 (key (elt vector (+ start child-1))))
                             (let ((key-1 (key (elt vector (+ start child-1))))
                                   (key-2 (key (elt vector (+ start child-2)))))
                               (if (funcall predicate key-1 key-2)
                                   (values child-2 key-2)
                                   (values child-1 key-1))))))
                 (when (funcall predicate (key (elt vector (+ start left))) key)
                   (rotatef (elt vector (+ start left))
                            (elt vector (+ start largest)))
                   (sift-down start largest right))))
             ;; Sort the three elements with the supplied indices.
             (sort-3 (index-1 index-2 index-3)
               (macrolet ((cswap ((index-1 index-2) &body body)
                            `(when (funcall predicate
                                            (key (elt vector ,index-1))
                                            (key (elt vector ,index-2)))
                               (rotatef (elt vector ,index-1)
                                        (elt vector ,index-2))
                               ,@body)))
                 (cswap (index-2 index-1))
                 (cswap (index-3 index-2)
                        (cswap (index-2 index-1)))))
             ;; Sort the interval from START below END using quicksort.
             (quicksort (start end max-depth)
               (declare (vector-length start end max-depth))
               ;; Try to find a good pivot element and move it to (1+ START).
               (let ((middle (floor (+ start end) 2)))
                 (sort-3 start middle (1- end))
                 (rotatef (elt vector (1+ start))
                          (elt vector middle)))
               (let* ((pivot-key (key (elt vector (1+ start))))
                      (lo (1+ start))
                      (hi (1- end)))
                 (declare (vector-length lo hi))
                 (loop
                   (loop do (incf lo)
                         while (< lo end) ; Only reached for weird predicates.
                         while (funcall predicate (key (elt vector lo)) pivot-key))
                   (loop do (decf hi)
                         while (> hi start) ; Only reached for weird predicates.
                         while (funcall predicate pivot-key (key (elt vector hi))))
                   (when (<= hi lo) (return))
                   (rotatef (elt vector lo)
                            (elt vector hi)))
                 (rotatef (elt vector (1+ start))
                          (elt vector hi))
                 (introsort start hi (1- max-depth))
                 (introsort hi end (1- max-depth))))
             ;; Sort the interval from START below END using either
             ;; shellsort, quicksort, or heapsort, depending on which is
             ;; the best fit for the data.
             (introsort (start end max-depth)
               (if (<= (- end start) +introsort-shellsort-threshold+)
                   (shellsort start end)
                   (if (zerop max-depth)
                       (heapsort start end)
                       (quicksort start end max-depth)))))
          (let ((n (length vector)))
            (introsort 0 n (* 2 (integer-length (1+ n))))
            vector))))))

(replicate-for-each #1=#:bit-vector (bit-vector simple-bit-vector)
  (defmethod sort ((#1# #1#) predicate &key key)
    (stable-sort #1# predicate :key key)))
