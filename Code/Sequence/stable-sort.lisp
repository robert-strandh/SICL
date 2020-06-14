(cl:in-package #:sicl-sequence)

(defmethod stable-sort ((list list) predicate &key key)
  (declare (list list))
  (let ((predicate (function-designator-function predicate)))
    (declare (function predicate))
    (with-key-function (key key)
      (macrolet
          ;; We define a macro for sorting a given, small number of conses
          ;; by applying a certain permutation.  The expansion returns two
          ;; values: The first cons, and the item that used to be the rest
          ;; of the rightmost supplied cons before permuting.
          ((cons-sorter (conses permutation)
             (let* ((n (length conses))
                    (first-cons (nth (first permutation) conses))
                    (last-cons (nth (nth (1- n) permutation) conses)))
               (sicl-utilities:with-gensyms (rest)
                 `(let ((,rest (cdr ,(first (last conses)))))
                    ;; Set the correct successor for each cons.
                    ,@(loop repeat (1- n)
                            for (i j) on permutation
                            ;; As an optimization, do not set the CDR of a
                            ;; cons that already has the correct successor.
                            unless (= j (1+ i))
                              collect `(setf (cdr ,(nth i conses))
                                             ,(nth j conses)))
                    (setf (cdr ,last-cons) nil)
                    (values ,first-cons ,rest))))))
        ;; The actual stable sorting code consists of special cases for
        ;; sorting exactly two or three conses, and the general case of
        ;; sorting N conses.
        (labels
            ((predicate (a b)
               (funcall predicate a b))
             (stable-sort-2 (cons-1 &aux (cons-2 (cdr cons-1)))
               (declare (cons cons-1 cons-2))
               (let ((key-1 (key (car cons-1)))
                     (key-2 (key (car cons-2))))
                 (if (predicate key-2 key-1)
                     (cons-sorter (cons-1 cons-2) (1 0))
                     (cons-sorter (cons-1 cons-2) (0 1)))))
             (stable-sort-3 (cons-1 &aux (cons-2 (cdr cons-1)) (cons-3 (cdr cons-2)))
               (declare (cons cons-1 cons-2 cons-3))
               (let ((key-1 (key (car cons-1)))
                     (key-2 (key (car cons-2)))
                     (key-3 (key (car cons-3))))
                 (if (predicate key-2 key-1)
                     (if (predicate key-3 key-2)
                         (cons-sorter (cons-1 cons-2 cons-3) (2 1 0))
                         (if (predicate key-3 key-1)
                             (cons-sorter (cons-1 cons-2 cons-3) (1 2 0))
                             (cons-sorter (cons-1 cons-2 cons-3) (1 0 2))))
                     (if (predicate key-3 key-2)
                         (if (predicate key-3 key-1)
                             (cons-sorter (cons-1 cons-2 cons-3) (2 0 1))
                             (cons-sorter (cons-1 cons-2 cons-3) (0 2 1)))
                         (cons-sorter (cons-1 cons-2 cons-3) (0 1 2))))))
             (stable-sort-n (list n)
               (declare (list-length n))
               (case n
                 (2 (stable-sort-2 list))
                 (3 (stable-sort-3 list))
                 (otherwise
                  (let ((n/2 (floor n 2)))
                    (multiple-value-bind (list-1 rest)
                        (stable-sort-n list n/2)
                      (multiple-value-bind (list-2 rest)
                          (stable-sort-n rest (- n n/2))
                        (values
                         (merge 'list list-1 list-2 predicate :key key)
                         rest))))))))
          (declare (inline predicate))
          (let ((n (length list)))
            ;; By handling the case of sorting zero elements and a single
            ;; element immediately, we ensure that the remaining code will
            ;; only ever deal with intervals of length two, three, or
            ;; larger than three.
            (case n
              ((0 1) list)
              (otherwise
               (values
                (stable-sort-n list n))))))))))

;;; We use binary insertion sort for stable sorting small blocks of
;;; elements, and then merge these blocks.  For merging, we use a technique
;;; by Pok-Son Kim and Arne Kutzner from the paper "Stable Minimum Storage
;;; Merging by Symmetric Comparisons" (DOI: 10.1007/978-3-540-30140-0_63)
;;;
;;; This code was inspired by a stable sorting function for the Go
;;; programming language, written by Volker Dobler.
;;;
;;; A note on performance: For random data, this code is about half as fast
;;; as a decent merge sort, and also requires about twice the number of
;;; comparisons.  However, it adapts marvelously to partially sorted data
;;; and does not cons.

(defconstant +symsort-block-size+ 16)

(replicate-for-each #1=#:vector (vector simple-vector)
 (defmethod stable-sort ((vector #1#) predicate &key key)
  (declare (#1# vector))
  (let ((length (length vector))
        (predicate (function-designator-function predicate)))
    (declare (vector-length length))
    (declare (function predicate))
    (with-key-function (key key)
      (labels
          ;; Sort the interval from START to END.
          ((insertion-sort (start end)
             (declare (vector-length start end))
             (loop for position fixnum from (1+ start) below end do
               (let* ((pivot (elt vector position))
                      (pivot-key (key pivot))
                      (index position))
                 (declare (vector-length index))
                 (loop for peek = (elt vector (1- index))
                       while (funcall predicate pivot-key (key peek))
                       do (setf (elt vector index) peek)
                       do (decf index)
                       until (= index start))
                 (setf (elt vector index) pivot))))
           ;; Merge the adjacent array intervals U (from FIRST1 to FIRST2)
           ;; and V (from FIRST2 to LAST).
           (symmerge (first1 first2 last)
             (declare (vector-length first1 first2 last))
             (when (< first1 first2 last)
               (cond
                 ;; If the interval U has exactly one element, we can insert that
                 ;; element into V using binary search.
                 ((= (- first2 first1) 1)
                  (let* ((u (elt vector first1))
                         (k (key u))
                         (l first2)
                         (r last))
                    (loop until (= l r) do
                      (let ((m (floor (+ l r) 2)))
                        (if (funcall predicate (key (elt vector m)) k)
                            (setf l (1+ m))
                            (setf r m))))
                    (replace vector vector :start1 first1 :start2 (1+ first1) :end1 (1- l))
                    (setf (elt vector (1- l)) u)))
                 ;; If the interval V has exactly one element, we can insert that
                 ;; element into U using binary search.
                 ((= (- last first2) 1)
                  (let* ((v (elt vector first2))
                         (k (key v))
                         (l first1)
                         (r first2))
                    (loop until (= l r) do
                      (let ((m (floor (+ l r) 2)))
                        (if (funcall predicate k (key (elt vector m)))
                            (setf r m)
                            (setf l (1+ m)))))
                    (replace vector vector :start1 (1+ l) :start2 l :end2 first2)
                    (setf (elt vector l) v)))
                 ;; Otherwise, we perform the actual symmerge.
                 (t
                  (let* ((m (floor (+ first1 last) 2))
                         (n (+ m first2))
                         (start
                           (if (> first2 m)
                               (bsearch (- n last) m (1- n))
                               (bsearch first1 first2 (1- n))))
                         (end (- n start)))
                    (rotate start first2 end)
                    (symmerge first1 start m)
                    (symmerge m end last))))))
           (bsearch (l r p)
             (declare (vector-length l r p))
             (if (= l r)
                 l
                 (let ((m (floor (+ l r) 2)))
                   (if (funcall predicate
                                (key (elt vector (- p m)))
                                (key (elt vector m)))
                       (bsearch l m p)
                       (bsearch (1+ m) r p)))))
           (swap-range (start1 start2 n)
             (declare (vector-length start1 start2 n))
             (loop for offset fixnum below n do
               (rotatef (elt vector (+ start1 offset))
                        (elt vector (+ start2 offset)))))
           ;; Rotate the two consecutive blocks L and R, where L is the
           ;; range from START below POSITION, and R is the range from
           ;; POSITION below END.
           (rotate (start position end)
             (declare (vector-length start position end))
             (when (< start position end)
               (let ((n-left (- position start))
                     (n-right (- end position)))
                 (declare (type vector-length n-left n-right))
                 (loop
                   (cond ((= n-left n-right)
                          (swap-range (- position n-left) position n-left)
                          (return))
                         ((> n-left n-right)
                          (swap-range (- position n-left) position n-right)
                          (decf n-left n-right))
                         ((< n-left n-right)
                          (swap-range (- position n-left) (+ position (- n-right n-left)) n-left)
                          (decf n-right n-left))))))))
        ;; Sort each block.
        (let ((start 0)
              (end +symsort-block-size+))
          (declare (vector-length start end))
          (loop while (< end length) do
            (insertion-sort start end)
            (shiftf start end (+ end +symsort-block-size+)))
          (insertion-sort start length))
        ;; Successively merge all blocks.
        (do ((block-size +symsort-block-size+ (* 2 block-size)))
            ((>= block-size length) vector)
          (declare (vector-length block-size))
          (let ((start 0)
                (end (* 2 block-size)))
            (loop while (< end length) do
              (symmerge start (+ start block-size) end)
              (shiftf start end (+ end (* 2 block-size))))
            (let ((pos (+ start block-size)))
              (when (< pos length)
                (symmerge start pos length))))))))))

;;; Stable sorting of bit vectors is one of the few cases where counting
;;; sort really shines.

(replicate-for-each #1=#:bit-vector (bit-vector simple-bit-vector)
  (defmethod stable-sort ((bit-vector #1#) predicate &key key)
    (declare (#1# bit-vector))
    (let ((predicate (function-designator-function predicate))
          (length (length bit-vector))
          (zeros (count 0 bit-vector)))
      (declare (vector-length length zeros))
      (with-key-function (key key)
        (if (zerop zeros)
            bit-vector
            (if (= zeros length)
                bit-vector
                (let ((k0 (key 0))
                      (k1 (key 1)))
                  (if (funcall predicate k0 k1)
                      (progn
                        (fill bit-vector 0 :end zeros)
                        (fill bit-vector 1 :start zeros))
                      (if (funcall predicate k1 k0)
                          (let ((ones (- length zeros)))
                            (fill bit-vector 1 :end ones)
                            (fill bit-vector 0 :start ones))
                          bit-vector)))))))))

(seal-domain #'stable-sort '(list t))
(seal-domain #'stable-sort '(vector t))
