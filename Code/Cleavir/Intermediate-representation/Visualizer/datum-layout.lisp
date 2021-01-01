(cl:in-package #:cleavir-ir-visualizer)

(defun overlap-with-next-p (data index)
  (< (- (datum-vertical-position (aref data (1+ index)))
        (datum-vertical-position (aref data index)))
     30))

(defun move-datum-up-to-avoid-overlap (data index)
  (assert (overlap-with-next-p data index))
  (setf (datum-vertical-position (aref data index))
        (- (datum-vertical-position (aref data (1+ index))) 31)))

(defun fix-overlap-by-moving-down (data index)
  (loop for i from index below (length data)
        while (overlap-with-next-p data (1- i))
        do (move-datum-down-to-avoid-overlap data i)))

(defun move-datum-down-to-avoid-overlap (data index)
  (assert (overlap-with-next-p data (1- index)))
  (setf (datum-vertical-position (aref data index))
        (+ (datum-vertical-position (aref data (1- index))) 31)))

(defun fix-overlap-by-moving-up (data index)
  (loop for i from index downto 0
        while (overlap-with-next-p data i)
        do (move-datum-up-to-avoid-overlap data i)))

(defun enough-space-above-p (data index)
  (> (datum-vertical-position (aref data index))
     (+ 15 (* (1+ index) 31))))

(defun fix-overlap (data index)
  (if (enough-space-above-p data index)
      (fix-overlap-by-moving-up data index)
      (fix-overlap-by-moving-down data (1+ index))))
      
(defun find-overlap (data)
  (loop for i from 0 below (1- (length data))
        when (overlap-with-next-p data i)
          return i
        finally (return nil)))

(defun fix-overlaps (data)
  (loop for index = (find-overlap data)
        until (null index)
        do (fix-overlap data index)))

(defun find-all-data (initial-instruction)
  (let ((data '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for datum in (append (cleavir-ir:inputs instruction)
                                  (cleavir-ir:outputs instruction))
             do (pushnew datum data)))
     initial-instruction)
    data))

(defun sort-data (data)
  (sort data #'< :key #'datum-horizontal-position))

(defun extract-column (data)
  (let ((result (list (car data))))
    (loop for element in (cdr data)
          until (> (- (datum-horizontal-position element)
                      (datum-horizontal-position (car result)))
                   100)
          do (push element result))
    (sort result #'< :key #'datum-vertical-position)))

(defun fix-data-overlaps (initial-instruction)
  (let ((data (sort-data (find-all-data initial-instruction))))
    (loop until (null data)
          do (let ((column (extract-column data)))
               (setf data (nthcdr (length column) data))
               (fix-overlaps (coerce column 'vector))))))
