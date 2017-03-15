(in-package #:cleavir-kildall)

;;;; Commonly pools are maps from HIR data to specification-specific
;;;; things. So we can abstract that.
;;;; Right now it's just alists, but FIXME, performance could probably
;;;; be improved by establishing an index number for each input.

(defclass map-pool-mixin () ())

(defgeneric object-meet (specialization object1 object2))
(defgeneric object<= (specialization object1 object2))

(defmethod pool-meet ((s map-pool-mixin) p1 p2)
  (let ((result (copy-alist p2)))
    (loop for pair in p1
          for (location . indicator) = pair
          for a = (assoc location result)
          when a
            do (setf (cdr a) (object-meet s (cdr a) indicator))
          else do (push pair result))
    result))

;;; iff p1 has all the variables p2 does, and with <= objects.
(defmethod pool<= ((s map-pool-mixin) p1 p2)
  (every (lambda (pair2)
           (let ((pair1 (assoc (car pair2) p1)))
             (and pair1 (object<= s (cdr pair1) (cdr pair2)))))
         p2))

(declaim (inline empty-map-pool))
(defun empty-map-pool () nil)

(declaim (inline alist->map-pool))
(defun alist->map-pool (alist) alist)

(defgeneric find-in-pool (specialization location pool
                          &key default))

(defmethod find-in-pool ((specialization map-pool-mixin)
                         location pool
                         &key (default nil default-p))
  (let ((a (assoc location pool)))
    (cond (a (cdr a))
          (default-p default)
          (t (error "BUG: missing location ~a" location)))))

;;; not a SETF since it makes a new one.
(defun replace-in-pool (new location pool)
  (let ((a (assoc location pool)))
    (if a
        (acons location new (remove a pool))
        (acons location new pool))))

;;; convenient way to do the above for some fixed set of locations
(defun pool-subst (pool &rest locations-and-values)
  (loop for (location value) on locations-and-values by #'cddr
        do (setf pool (replace-in-pool value location pool))
        finally (return pool)))
