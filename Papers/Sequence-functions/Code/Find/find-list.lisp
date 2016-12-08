(in-package :find)

(defun find-list-1 (item list &key end)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop for index from 0
        for element in vector
        when (and (not (null end)) (>= index end))
          return nil
        when (eql item element)
          return element))
