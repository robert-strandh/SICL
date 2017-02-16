(in-package #:cleavir-kildall-escape)

;;; Pools are alists variable -> escape-indicator.

(defun find-in-pool (input pool)
  (let ((a (assoc input pool)))
    (if a
        (cdr a)
        (error "BUG: missing input in escape analysis pool"))))

(defun find-in-pool-permissively (input pool)
  (or (cdr (assoc input pool)) +none+))

(defmethod cleavir-kildall:pool-meet ((s escape) p1 p2)
  (let ((result (copy-alist p2)))
    (loop for pair in p1
          for (location . indicator) = pair
          for a = (assoc location result)
          when a
            do (setf (cdr a) (indicator-union (cdr a) indicator))
          else do (push pair result))
    result))

;;; iff p1 has all variables p2 does, and with <= indicators.
(defmethod cleavir-kildall:pool<= ((s escape) p1 p2)
  (every (lambda (pair2)
           (let ((pair1 (assoc (car pair2) p1)))
             (and pair1 (indicator<= (cdr pair1) (cdr pair2)))))
         p2))

(defmethod cleavir-kildall:entry-pool ((s escape) instruction)
  nil)
