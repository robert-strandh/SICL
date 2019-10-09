(cl:in-package #:sicl-cons)

(defun ldiff (list object)
  (if (or (eql list object) (atom list))
      nil
      (let* ((result (list (car list)))
             (current result)
             (remaining (cdr list)))
        (loop until (or (eql remaining object) (atom remaining))
              do (setf (cdr current) (list (car remaining)))
                 (setf current (cdr current))
                 (setf remaining (cdr remaining)))
        (if (eql remaining object)
            result
            (progn (setf (cdr current) remaining)
                   result)))))
