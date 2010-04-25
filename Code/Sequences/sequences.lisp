(in-package #:sicl-sequences)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function find-if

(defmacro forward-list (name boundedp keyp)
  `(defun ,name (predicate
                 list
                 start
                 ,@(if boundedp '(end) '())
                 ,@(if keyp '(key) '()))
     (loop for element in (nthcdr start list)
           ,@(if endp '(repeat (- end start)))
           when (funcall predicate
                         ,(if keyp '(funcall key element) 'element))
             return element)))

(defmacro backward-list (name boundedp keyp)
  `(defun ,name (predicate
                 list
                 start
                 ,@(if boundedp '(end) '())
                 ,@(if keyp '(key) '()))
     (labels ((aux (list ,@(if boundedp '(index) '()))
                (if ,(if boundedp
                         '(or (null list) (>= index end))
                         '(null list))
                    nil
                    (progn (aux (cdr list) ,@(if boundedp '(1+ index) '()))
                           (when (funcall predicate ,key-test)
                             (return-from find-if-list-from-end-unbounded-key
                               (car list)))))))
       (aux (nthcdr start list) ,@(if boundedp '(start) '())))))

(forward-list  find-if-list-from-start-unbounded-key      nil t)
(backward-list find-if-list-from-end-unbounded-key        nil t)
(forward-list  find-if-list-from-start-bounded-key        t   t)
(backward-list find-if-list-from-end-bounded-key          t   t)
(forward-list  find-if-list-from-start-unbounded-identity nil nil)
(backward-list find-if-list-from-end-unbounded-identity   nil nil)
(forward-list  find-if-list-from-start-bounded-identity   t   nil)
(backward-list find-if-list-from-end-bounded-identity     t   nil)

;;; Try to aviod this much code duplication by strategic use of
;;; macros.


;;; General version on vectors from start
(defun find-if-vector-from-start-key
    (predicate vector start end key)
  (loop for index from start below (min end (length vector))
        when (funcall predicate (funcall key (aref vector index)))
          return (aref vector index)))

;;; General version on vectors from end
(defun find-if-vector-from-end-key
    (predicate vector start end key)
  (loop for index downfrom (1- (min end (length vector))) to start
        when (funcall predicate (funcall key (aref vector index)))
          return (aref vector index)))

;;; Version on vectors from start where the key is #'identity
(defun find-if-vector-from-start-identity
    (predicate vector start end)
  (loop for index from start below (min end (length vector))
        when (funcall predicate (aref vector index))
          return (aref vector index)))

;;; Version on vectors from end where the key is #'identity
(defun find-if-vector-from-end-identity
    (predicate vector start end)
  (loop for index downfrom (1- (min end (length vector))) to start
        when (funcall predicate (aref vector index))
          return (aref vector index)))

(defun find-if (predicate sequence
                &key
                (from-end nil)
                (start 0)
                (end nil)
                (key nil))
  (assert (plusp start))
  (etypecase sequence
    (vector
       (if from-end
           (if key
               (find-if-vector-from-end-key
                predicate sequence start (or end (length sequence)) key)
               (find-if-vector-from-end-identity
                predicate sequence start (or end (length sequence))))
           (if key
               (find-if-vector-from-start-key
                predicate sequence start (or end (length sequence)) key)
               (find-if-vector-from-start-identity
                predicate sequence start (or end (length sequence)))))
       (list
        (if from-end
            (if key
                (if end
                    (find-if-list-from-end-bounded-key
                     predicate sequence start end key)
                    (find-if-list-from-end-unbounded-key
                     predicate sequence start key))
                (if end
                    (find-if-list-from-end-bounded-identity
                     predicate sequence start end)
                    (find-if-list-from-end-unbounded-identity
                     predicate sequence start)))
            (if key
                (if end
                    (find-if-list-from-start-bounded-key
                     predicate sequence start end key)
                    (find-if-list-from-start-unbounded-key
                     predicate sequence start key))
                (if end
                    (find-if-list-from-start-bounded-identity
                     predicate sequence start end)
                    (find-if-list-from-start-unbounded-identity
                     predicate sequence start))))))))

                    
