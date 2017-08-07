(in-package #:cleavir-liveness)

;;; An implementation of "small sets" as sorted lists.
;;; This code is important to performance, so it's written
;;; a bit strangel.

(defun sset-union (s1 s2)
  (declare (type list s1 s2)
           (optimize speed))
  (cond ((null s1) s2)
        ((null s2) s1)
        (t
         ;; Since s1 and s2 are sorted lists, we can iterate through
         ;; them once only.
         ;; If either is out of elements, we just append the other
         ;; and we are done.
         ;; Otherwise, we compare the first element of both.
         ;; We collect the lesser, and move that list forward.
         ;; On the off chance they're equal, we collect either
         ;; and move both lists forward.
         (loop with x1 fixnum = (car s1)
               with x2 fixnum = (car s2)
               when (< x1 x2)
                 collect x1
                 and do (setf s1 (cdr s1) x1 (car s1))
                 and when (null s1) nconc s2 and do (loop-finish) end
               else when (> x1 x2)
                      collect x2
                      and do (setf s2 (cdr s2) x2 (car s2))
                      and when (null s2) nconc s1 and do (loop-finish) end
               else ; (= x1 x2), unlikeliest case
                 collect x1
                 and do (setf s1 (cdr s1) s2 (cdr s2))
                 and when (null s1) nconc s2 and do (loop-finish) end
                 and do (setf x1 (car s1))
                 and when (null s2) nconc s1 and do (loop-finish) end
                 and do (setf x2 (car s2))))))

(defun sset-difference (s1 s2)
  (declare (type list s1 s2)
           (optimize speed))
  (cond ((null s1) nil)
        ((null s2) s1)
        (t
         (loop with x1 fixnum = (car s1)
               with x2 fixnum = (car s2)
               when (< x1 x2)
                 collect x1
                 and do (setf s1 (cdr s1) x1 (car s1))
                 and when (null s1) do (loop-finish) end
               else when (> x1 x2)
                      do (setf s2 (cdr s2) x2 (car s2))
                      and when (null s2) nconc s1 and do (loop-finish) end
               else
                 do (setf s1 (cdr s1) s2 (cdr s2))
                 and when (null s1) do (loop-finish) end
                 and when (null s2) nconc s1 and do (loop-finish) end
                 and do (setf x1 (car s1) x2 (car s2))))))

(defun subssetp (s1 s2)
  (declare (type list s1 s2)
           (optimize speed))
  (cond ((null s1) t)
        ((null s2) nil)
        (t
         (loop with x1 fixnum = (car s1)
               with x2 fixnum = (car s2)
               when (< x1 x2)
                 return nil
               when (> x1 x2)
                 do (setf s2 (cdr s2) x2 (car s2))
                 and when (null s2) return nil end ; nothing left in s2 for x1 to be
               else
                 do (setf s1 (cdr s1) s2 (cdr s2))
                 and when (null s1) return t end
                 and when (null s2) return nil end
                 and do (setf x1 (car s1) x2 (car s2))))))
