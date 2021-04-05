(cl:in-package #:sicl-register-allocation)

(defclass pool-item ()
  ((%lexical-location :initarg :lexical-location :reader lexical-location)
   (%distance :initarg :distance :reader distance)
   (%call-probability :initarg :call-probability :reader call-probability)))

(defmethod print-object ((object pool-item) stream)
  (print-unreadable-object (object stream)
    (format stream
            "location: ~s distance: ~s probability: ~s"
            (lexical-location object)
            (distance object)
            (call-probability object))))

(defgeneric item-meet (probability item1 item2))

(defmethod item-meet (probability (item1 null) (item2 pool-item))
  (make-instance 'pool-item
    :lexical-location (lexical-location item2)
    :distance (floor (/ (distance item2) (- 1 probability)))
    :call-probability (ceiling (* (- 1 probability) (call-probability item2)))))

(defmethod item-meet (probability (item1 pool-item) (item2 null))
  (make-instance 'pool-item
    :lexical-location (lexical-location item1)
    :distance (floor (/ (distance item1) probability))
    :call-probability (ceiling (* probability (call-probability item1)))))

(defun combine-distances (probability distance1 distance2)
  (if (or (zerop distance1) (zerop distance2))
      0
      (floor (/ (+ (/ probability distance1)
                   (/ (- 1 probability) distance2))))))

(defmethod item-meet (probability (item1 pool-item) (item2 pool-item))
  (make-instance 'pool-item
    :lexical-location (lexical-location item1)
    :distance (combine-distances probability (distance item1) (distance item1))
    :call-probability
    (ceiling (+ (* probability (call-probability item1))
                (* (- 1 probability) (call-probability item2))))))

(defun make-pool ()
  '())

(defun make-pool-item
    (lexical-location distance &optional (call-probability 0))
  (make-instance 'pool-item
    :lexical-location lexical-location
    :distance distance
    :call-probability call-probability))

;;; Remove a variable from a pool
(defun remove-variable (pool variable)
  (remove variable pool :key #'lexical-location :test #'eq))

;;; Add a variable associated with an estimated distance of use of 0.
(defun add-variable (pool variable)
  (cons (make-pool-item variable 0)
        (remove-variable pool variable)))

;;; This is the <= operation from Kildall's paper.
(defun pool<= (pool1 pool2)
  (loop for entry1 in pool1
        for entry2 = (find (lexical-location entry1)  pool2
                           :test #'eq :key #'lexical-location)
        always (and (not (null entry2))
                    (<= (distance entry2) (distance entry1))
                    (<= (call-probability entry1) (call-probability entry2)))))

(defun pool-meet (probability pool1 pool2)
  (let ((result '()))
    ;; First handle variables that are present in pool1.
    (loop for entry1 in pool1
          for variable = (lexical-location entry1)
          for entry2 = (find variable pool2
                             :test #'eq :key #'lexical-location)
          do (push (item-meet probability entry1 entry2) result))
    ;; Now handle the variables that are present in pool2.  Except
    ;; that if a variable is also present in pool1, it has already
    ;; been handled.
    (loop for entry2 in pool2
          when (null (find (lexical-location entry2) pool1
                           :test #'eq :key #'lexical-location))
            do (push (item-meet probability nil entry2) result))
    result))

(defun increment-all-distances (pool)
  (loop for pool-item in pool
        collect (make-pool-item
                 (lexical-location pool-item)
                 (1+ (distance pool-item)))))

;;; Return the estimated distance to use of LEXICAL-LOCATION in POOL.
;;; If LEXICAL-LOCATION is not present in POOL, then an error is
;;; signaled.
(defun distance-of-lexical-location (lexical-location pool)
  (let ((pool-item (find lexical-location pool
                         :test #'eq :key #'lexical-location)))
    (assert (not (null pool-item)))
    (distance pool-item)))
