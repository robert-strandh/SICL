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
     (declare (optimize (speed 3) (debug 0) (safety 0))
              (type list list))
     (loop for element in (nthcdr start list)
           ,@(if boundedp '(repeat (- end start)))
           when (funcall predicate
                         ,(if keyp '(funcall key element) 'element))
             return element)))

(defmacro backward-list (name boundedp keyp)
  `(defun ,name (predicate
                 list
                 start
                 ,@(if boundedp '(end) '())
                 ,@(if keyp '(key) '()))
     (declare (optimize (speed 3) (debug 0) (safety 0))
              (type list list))
     (labels ((aux (list ,@(if boundedp '(index) '()))
                (if ,(if boundedp
                         '(or (null list) (>= index end))
                         '(null list))
                    nil
                    (progn (aux (cdr list) ,@(if boundedp '((1+ index)) '()))
                           (when (funcall predicate
                                          ,(if keyp
                                               '(funcall key (car list))
                                               '(car list)))
                             (return-from ,name
                               (car list)))))))
       (aux (nthcdr start list) ,@(if boundedp '(start) '())))))

(forward-list  find-if-list-from-start-unbounded-identity nil nil)
(forward-list  find-if-list-from-start-unbounded-key      nil t)
(forward-list  find-if-list-from-start-bounded-identity   t   nil)
(forward-list  find-if-list-from-start-bounded-key        t   t)

(backward-list find-if-list-from-end-unbounded-identity   nil nil)
(backward-list find-if-list-from-end-unbounded-key        nil t)
(backward-list find-if-list-from-end-bounded-identity     t   nil)
(backward-list find-if-list-from-end-bounded-key          t   t)

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

;;; This is the main function.  It first checks what type of
;;; sequence it is.  If it is a vector it then distinquishes 
;;; between 4 cases according to whether FROM-END and a KEY
;;; function was given.  If it is a list, it distinguishes
;;; between 8 cases according to whether FROM-END, a KEY 
;;; function, and an explicit END was given. 
;;;
;;; It is expected that this function will not be used very 
;;; often.  In most cases, the compiler macro will be used 
;;; instead. 
(defun find-if (predicate sequence
                &key
                (from-end nil)
                (start 0)
                (end nil)
                (key nil))
  (assert (not (minusp start)))
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
                predicate sequence start (or end (length sequence))))))
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
                    predicate sequence start)))))))

;;; The compiler macro is trying to detect situations where 
;;; either no keyword arguments were given, or only constant
;;; keyword arguments were given, so that one of several special
;;; versions can be used.  In general, those special versions
;;; will have to check what type of sequence it is (bacause
;;; that is something the compiler macro cannot do), and then
;;; invoke one of the special versions defined above.  On the
;;; other hand, these functions will likely be inlined so that
;;; type inferencing can determine which type of sequence it
;;; is at compile time. 

;;; Again, try to figure out a way to avoid all this code 
;;; duplication by using some macrology. 

(defun find-if-from-start-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-identity
        predicate sequence start (length sequence)))
    (list
       (find-if-list-from-start-unbounded-identity
        predicate sequence start))))

(defun find-if-from-start-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-key
        predicate sequence start (length sequence) key))
    (list
       (find-if-list-from-start-unbounded-key
        predicate sequence start key))))

(defun find-if-from-start-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-identity
        predicate sequence start end))
    (list
       (find-if-list-from-start-bounded-identity
        predicate sequence start end))))

(defun find-if-from-start-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-key
        predicate sequence start end key))
    (list
       (find-if-list-from-start-bounded-key
        predicate sequence start end key))))

(defun find-if-from-end-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-identity
        predicate sequence start (length sequence)))
    (list
       (find-if-list-from-end-unbounded-identity
        predicate sequence start))))

(defun find-if-from-end-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-key
        predicate sequence start (length sequence) key))
    (list
       (find-if-list-from-end-unbounded-key
        predicate sequence start key))))

(defun find-if-from-end-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-identity
        predicate sequence start end))
    (list
       (find-if-list-from-end-bounded-identity
        predicate sequence start end))))

(defun find-if-from-end-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-key
        predicate sequence start end key))
    (list
       (find-if-list-from-end-bounded-key
        predicate sequence start end key))))
