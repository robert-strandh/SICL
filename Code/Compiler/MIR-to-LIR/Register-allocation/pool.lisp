(cl:in-package #:sicl-register-allocation)

;;; For now a pool is an alist where the keys are lexical locations
;;; and the values are non-negative integers indicating estimated
;;; distance to use.

(defun make-pool (&rest pairs)
  pairs)

;;; This is the <= operation from Kildall's paper.
(defun pool<= (pool1 pool2)
  (loop for (variable . estimated-distance-to-use) in pool1
        for entry = (assoc variable pool2 :test #'eq)
        always (and (not (null entry))
                    (eq (car entry) variable)
                    (>= (cdr entry) estimated-distance-to-use))))

(defun pool-meet (probability pool1 pool2)
  (let ((result '()))
    ;; First handle variables that are present in pool1.
    (loop for entry1 in pool1
          for (variable . edu) = entry1
          for entry2 = (assoc variable pool2 :test #'eq)
          do (cond ((null entry2)
                    ;; The variable is present only in pool1 so the
                    ;; meet must contain its entry in pool1.
                    (push entry1 result))
                   ((or (zerop edu)
                        (zerop (cdr entry2)))
                    ;; The variable is present in both pools, and one
                    ;; of the estimated distance to use is 0.  Then
                    ;; the resulting entry must also have an estimated
                    ;; distance to use of 0.
                    (push (cons variable 0) result))
                   (t
                    (push (cons variable
                                (floor (/ (+ (/ probability edu)
                                             (/ (- 1 probability) (cdr entry2))))))
                          result))))
    ;; Now handle the variables that are present in pool2.  Except
    ;; that if a variable is also present in pool1, it has already
    ;; been handled.
    (loop for entry in pool2
          when (null (assoc (car entry) pool1 :test #'eq))
            do (push entry result))
    result))
