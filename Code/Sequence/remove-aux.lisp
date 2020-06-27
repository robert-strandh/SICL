(cl:in-package #:sicl-sequence)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; List Removal

(declaim (inline remove-from-list))
(defun remove-from-list (predicate list from-end start end count)
  (if (not from-end)
      (let ((count (canonicalize-count count))
            ;; COPY is a list of items that need to be copied if any sublist of
            ;; it starts with an element that satisfies PREDICATE.
            (copy list))
        (sicl-utilities:with-collectors ((result collect))
          (flet ((terminate ()
                   (return-from remove-from-list (result copy))))
            (let ((cons-iterator (make-cons-iterator list start end #'terminate)))
              (loop for n-removed from 0 until (= n-removed count) do
                (loop for current = (funcall cons-iterator)
                      until (funcall predicate (car current))
                      finally
                         (loop for cons on copy
                               until (eq cons current) do
                                 (collect (car cons)))
                         (setf copy (cdr current))))
              (terminate)))))
      (delete-in-list predicate (copy-seq list) t start end count)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Vector Removal

(declaim (inline remove-from-vector))
(defun remove-from-vector (predicate vector from-end start end count)
  (with-vector-start-and-end (start end length) (vector start end)
    (let ((count (canonicalize-count count))
          (bit-vector (make-sequence 'bit-vector (- end start) :initial-element 0))
          (n-removed 0))
      (if (not from-end)
          (loop for index from start below end
                until (= n-removed count) do
                  (when (funcall predicate (elt vector index))
                    (setf (elt bit-vector (- index start)) 1)
                    (incf n-removed)))
          (loop for index from (1- end) downto start
                until (= n-removed count) do
                  (when (funcall predicate (elt vector index))
                    (setf (elt bit-vector (- index start)) 1)
                    (incf n-removed))))
      (if (zerop n-removed)
          vector
          (let ((result (make-sequence-like vector (- length n-removed))))
            (loop for index below start do
              (setf (elt result index)
                    (elt vector index)))
            (let ((result-index start))
              (loop for index from start below end do
                (when (zerop (elt bit-vector (- index start)))
                  (setf (elt result result-index)
                        (elt vector index))
                  (incf result-index)))
              (loop for index from end below length do
                (setf (elt result (- index n-removed))
                      (elt vector index)))
              result))))))
