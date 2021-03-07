(cl:in-package #:sicl-register-allocation)

(defclass pool-item ()
  ((%lexical-location :initarg :lexical-location :reader lexical-location)
   (%distance :initarg :distance :reader distance)
   (%call-probability :initarg :call-probability :reader call-probability)))

(defgeneric item-meet (probability item1 item2))

(defmethod item-meet (probability (item1 null) (item2 pool-item))
  (make-instance 'pool-item
    :lexical-location (lexical-location item2)
    :distance (floor (/ (distance item2) (- 1 probability)))
    :call-probability (ceiling (* (- 1 probability) (call-probability item2)))))

(defmethod item-meet (probability (item1 pool-item) (item2 null))
  (make-instance 'pool-item
    :lexical-location (lexical-location item2)
    :distance (floor (/ (distance item1) probability))
    :call-probability (ceiling (* probability (call-probability item1)))))

(defmethod item-meet (probability (item1 pool-item) (item2 pool-item))
  (make-instance 'pool-item
    :lexical-location (lexical-location item1)
    :distance
    (floor (/ (+ (/ probability (distance item1))
                 (/ (- 1 probability) (distance item2)))))
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
                    (eq (lexical-location entry2) (lexical-location entry1))
                    (<= (distance entry2) (distance entry1)))))

(defun pool-meet (probability pool1 pool2)
  (let ((result '()))
    ;; First handle variables that are present in pool1.
    (loop for entry1 in pool1
          for variable = (lexical-location entry1)
          for edu = (distance entry1)
          for entry2 = (find variable pool2
                             :test #'eq :key #'lexical-location)
          do (cond ((null entry2)
                    ;; The variable is present only in pool1 so the
                    ;; meet must contain its entry in pool1.
                    (push entry1 result))
                   ((or (zerop edu)
                        (zerop (distance entry2)))
                    ;; The variable is present in both pools, and one
                    ;; of the estimated distance to use is 0.  Then
                    ;; the resulting entry must also have an estimated
                    ;; distance to use of 0.
                    (push (make-pool-item variable 0)
                          result))
                   (t
                    (push (make-pool-item
                           variable
                           (floor (/ (+ (/ probability edu)
                                        (/ (- 1 probability)
                                           (distance entry2))))))
                          result))))
    ;; Now handle the variables that are present in pool2.  Except
    ;; that if a variable is also present in pool1, it has already
    ;; been handled.
    (loop for entry in pool2
          when (null (find (lexical-location entry) pool1
                           :test #'eq :key #'lexical-location))
            do (push entry result))
    result))

(defun increment-all-distances (pool)
  (loop for pool-item in pool
        collect (make-pool-item
                 (lexical-location pool-item)
                 (1+ (distance pool-item)))))
