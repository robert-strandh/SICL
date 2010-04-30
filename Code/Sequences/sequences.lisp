(in-package #:sicl-sequences)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function find-if

;;; We tried using some macrology here in order to decrease
;;; the amount of code duplication, but the amount of code 
;;; saved wasn't that great, and the macro code became
;;; incomprehensible instead.

;;; For the versions on lists, we distinguish between 
;;; three different characteristics: 
;;;
;;;   * whether from-end has been given or not
;;;
;;;   * whether the end is the end of the list or not
;;;
;;;   * whether there is a key function or not
;;;
;;; When from-end was not given, we stop iteration as soon
;;; as we find an element that satisfies the test.  When 
;;; from-end was given, we keep going until the end, and
;;; when an element is found that satisifies the test, it
;;; is saved in a variable.  The value of that variable
;;; is then returned at the end.  This method avoids consing
;;; and using up stack space proportional to the length of the
;;; list, but it is costly if the predicate is costly to apply.
;;;
;;; When the end is the end of the list, we avoid a counter
;;; in the loop that checks when the end has been reached.
;;;
;;; When there is no key function, we avoid funcalling the 
;;; identity function. 

;;; Version on lists, from start, no end, no key
(defun find-if-list-from-start-unbounded-identity
    (predicate list start)
  (loop for element in (nthcdr start list)
        when (funcall predicate element)
          return element))

;;; Version on lists, from start, no end, key
(defun find-if-list-from-start-unbounded-key
    (predicate list start key)
  (loop for element in (nthcdr start list)
        when (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from start, end, no key
(defun find-if-list-from-start-bounded-identity
    (predicate list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (funcall predicate element)
          return element))

;;; Version on lists, from start, end, key
(defun find-if-list-from-start-bounded-key
    (predicate list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from end, no end, no key
(defun find-if-list-from-end-unbounded-identity
    (predicate list start)
  (loop with value = nil
        for element in (nthcdr start list)
        when (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, no end, key
(defun find-if-list-from-end-unbounded-key
    (predicate list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (funcall predicate (funcall key element))
          do (setf value element)
        finally (return value)))
  
;;; Version on lists, from end, end, no key
(defun find-if-list-from-end-bounded-identity
    (predicate list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, end, key
(defun find-if-list-from-end-bounded-key
    (predicate list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (funcall predicate (funcall key element))
          do (setf value element)
        finally (return value)))
  
;;; For the versions on lists, we distinguish between 
;;; two different characteristics: 
;;; 
;;;   * whether from-end has been given or not
;;;
;;;   * whether there is a key function or not
;;;
;;; We do not need to distinguish between when an explic
;;; end has been given, and when it has not been given, 
;;; because the loop looks the same anyway; it is just the 
;;; incices of the loop that will change. 
;;;
;;; When from-end has been given, we loop from higher indices 
;;; to lower, otherwise from lower to higher.
;;;
;;; When there is no key function, we avoid a funcall of
;;; identity, just as with lists. 

;;; Version on vectors, from start, no key
(defun find-if-vector-from-start-identity
    (predicate vector start end)
  (loop for index from start below (min end (length vector))
        when (funcall predicate (aref vector index))
          return (aref vector index)))

;;; Version on vectors, from start, key
(defun find-if-vector-from-start-key
    (predicate vector start end key)
  (loop for index from start below (min end (length vector))
        when (funcall predicate (funcall key (aref vector index)))
          return (aref vector index)))

;;; Version on vectors, from end, no key
(defun find-if-vector-from-end-identity
    (predicate vector start end)
  (loop for index downfrom (1- (min end (length vector))) to start
        when (funcall predicate (aref vector index))
          return (aref vector index)))

;;; Version on vectors, from end, key
(defun find-if-vector-from-end-key
    (predicate vector start end key)
  (loop for index downfrom (1- (min end (length vector))) to start
        when (funcall predicate (funcall key (aref vector index)))
          return (aref vector index)))

;;; The compiler macro is trying to detect situations where either no
;;; keyword arguments were given, or only constant keyword arguments
;;; were given, so that one of several special versions can be used.
;;; Those special versions will have to check what type of sequence it
;;; is (bacause that is something the compiler macro cannot do), and
;;; then invoke one of the special versions defined above.  On the
;;; other hand, these functions will likely be inlined so that type
;;; inferencing can determine which type of sequence it is at compile
;;; time.

;;; Version on any sequence, from start, no end, no key
(defun find-if-from-start-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-identity
        predicate sequence start (length sequence)))
    (list
       (find-if-list-from-start-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from start, no end, key
(defun find-if-from-start-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-key
        predicate sequence start (length sequence) key))
    (list
       (find-if-list-from-start-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from start, end, no key
(defun find-if-from-start-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-identity
        predicate sequence start end))
    (list
       (find-if-list-from-start-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from start, end, key
(defun find-if-from-start-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (find-if-vector-from-start-key
        predicate sequence start end key))
    (list
       (find-if-list-from-start-bounded-key
        predicate sequence start end key))))

;;; Version on any sequence, from end, no end, no key
(defun find-if-from-end-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-identity
        predicate sequence start (length sequence)))
    (list
       (find-if-list-from-end-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from end, no end, key
(defun find-if-from-end-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-key
        predicate sequence start (length sequence) key))
    (list
       (find-if-list-from-end-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from end, end, no key
(defun find-if-from-end-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-identity
        predicate sequence start end))
    (list
       (find-if-list-from-end-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from end, end, key
(defun find-if-from-end-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (find-if-vector-from-end-key
        predicate sequence start end key))
    (list
       (find-if-list-from-end-bounded-key
        predicate sequence start end key))))

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
  ;; FIXME do this better
  (assert (not (minusp start)))
  (if from-end
      (if key
          (if end
              (find-if-from-end-bounded-key
               predicate sequence start end key)
              (find-if-from-end-unbounded-key
               predicate sequence start key))
          (if end
              (find-if-from-end-bounded-identity
               predicate sequence start end)
              (find-if-from-end-unbounded-identity
               predicate sequence start)))
      (if key
          (if end
              (find-if-from-start-bounded-key
               predicate sequence start end key)
              (find-if-from-start-unbounded-key
               predicate sequence start key))
          (if end
              (find-if-from-start-bounded-identity
               predicate sequence start end)
              (find-if-from-start-unbounded-identity
               predicate sequence start)))))

(define-compiler-macro find-if (&whole form &rest args)
  (handler-case 
      (destructuring-bind (predicate sequence
                           &key
                           (from-end nil from-end-p)
                           (start 0 startp)
                           (end nil endp)
                           (key nil keyp))
          args
        (if (and endp (not (null end)))
            (if (and keyp (not (null key)))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-from-end-bounded-key
                          ,predicate ,sequence ,start ,end ,key)
                        `(if ,from-end
                             (find-if-from-end-bounded-key
                              ,predicate ,sequence ,start ,end ,key)
                             (find-if-from-start-bounded-key
                              ,predicate ,sequence ,start ,end ,key)))
                    `(find-if-from-start-bounded-key
                      ,predicate ,sequence ,start ,end ,key))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-from-end-bounded-identity
                          ,predicate ,sequence ,start ,end)
                        `(if ,from-end
                             (find-if-from-end-bounded-identity
                              ,predicate ,sequence ,start ,end)
                             (find-if-from-start-bounded-identity
                              ,predicate ,sequence ,start ,end)))
                    `(find-if-from-start-bounded-identity
                      ,predicate ,sequence ,start ,end)))
            (if (and keyp (not (null key)))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-from-end-unbounded-key
                          ,predicate ,sequence ,start ,key)
                        `(if ,from-end
                             (find-if-from-end-unbounded-key
                              ,predicate ,sequence ,start ,key)
                             (find-if-from-start-unbounded-key
                              ,predicate ,sequence ,start ,key)))
                    `(find-if-from-start-unbounded-key
                      ,predicate ,sequence ,start ,key))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-from-end-unbounded-identity
                          ,predicate ,sequence ,start)
                        `(if ,from-end
                             (find-if-from-end-unbounded-identity
                              ,predicate ,sequence ,start)
                             (find-if-from-start-unbounded-identity
                              ,predicate ,sequence ,start)))
                    `(find-if-from-start-unbounded-identity
                      ,predicate ,sequence ,start)))))
    (error () form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function find-if-not

;;; We tried using some macrology here in order to decrease
;;; the amount of code duplication, but the amount of code 
;;; saved wasn't that great, and the macro code became
;;; incomprehensible instead.

;;; For the versions on lists, we distinguish between 
;;; three different characteristics: 
;;;
;;;   * whether from-end has been given or not
;;;
;;;   * whether the end is the end of the list or not
;;;
;;;   * whether there is a key function or not
;;;
;;; When from-end was not given, we stop iteration as soon
;;; as we find an element that satisfies the test.  When 
;;; from-end was given, we keep going until the end, and
;;; when an element is found that satisifies the test, it
;;; is saved in a variable.  The value of that variable
;;; is then returned at the end.  This method avoids consing
;;; and using up stack space proportional to the length of the
;;; list, but it is costly if the predicate is costly to apply.
;;;
;;; When the end is the end of the list, we avoid a counter
;;; in the loop that checks when the end has been reached.
;;;
;;; When there is no key function, we avoid funcalling the 
;;; identity function. 

;;; Version on lists, from start, no end, no key
(defun find-if-not-list-from-start-unbounded-identity
    (predicate list start)
  (loop for element in (nthcdr start list)
        unless (funcall predicate element)
          return element))

;;; Version on lists, from start, no end, key
(defun find-if-not-list-from-start-unbounded-key
    (predicate list start key)
  (loop for element in (nthcdr start list)
        unless (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from start, end, no key
(defun find-if-not-list-from-start-bounded-identity
    (predicate list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        unless (funcall predicate element)
          return element))

;;; Version on lists, from start, end, key
(defun find-if-not-list-from-start-bounded-key
    (predicate list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        unless (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from end, no end, no key
(defun find-if-not-list-from-end-unbounded-identity
    (predicate list start)
  (loop with value = nil
        for element in (nthcdr start list)
        unless (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, no end, key
(defun find-if-not-list-from-end-unbounded-key
    (predicate list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        unless (funcall predicate (funcall key element))
          do (setf value element)
        finally (return value)))
  
;;; Version on lists, from end, end, no key
(defun find-if-not-list-from-end-bounded-identity
    (predicate list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        unless (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, end, key
(defun find-if-not-list-from-end-bounded-key
    (predicate list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        unless (funcall predicate (funcall key element))
          do (setf value element)
        finally (return value)))
  
;;; For the versions on lists, we distinguish between 
;;; two different characteristics: 
;;; 
;;;   * whether from-end has been given or not
;;;
;;;   * whether there is a key function or not
;;;
;;; We do not need to distinguish between when an explic
;;; end has been given, and when it has not been given, 
;;; because the loop looks the same anyway; it is just the 
;;; incices of the loop that will change. 
;;;
;;; When from-end has been given, we loop from higher indices 
;;; to lower, otherwise from lower to higher.
;;;
;;; When there is no key function, we avoid a funcall of
;;; identity, just as with lists. 

;;; Version on vectors, from start, no key
(defun find-if-not-vector-from-start-identity
    (predicate vector start end)
  (loop for index from start below (min end (length vector))
        unless (funcall predicate (aref vector index))
          return (aref vector index)))

;;; Version on vectors, from start, key
(defun find-if-not-vector-from-start-key
    (predicate vector start end key)
  (loop for index from start below (min end (length vector))
        unless (funcall predicate (funcall key (aref vector index)))
          return (aref vector index)))

;;; Version on vectors, from end, no key
(defun find-if-not-vector-from-end-identity
    (predicate vector start end)
  (loop for index downfrom (1- (min end (length vector))) to start
        unless (funcall predicate (aref vector index))
          return (aref vector index)))

;;; Version on vectors, from end, key
(defun find-if-not-vector-from-end-key
    (predicate vector start end key)
  (loop for index downfrom (1- (min end (length vector))) to start
        unless (funcall predicate (funcall key (aref vector index)))
          return (aref vector index)))

;;; The compiler macro is trying to detect situations where either no
;;; keyword arguments were given, or only constant keyword arguments
;;; were given, so that one of several special versions can be used.
;;; Those special versions will have to check what type of sequence it
;;; is (bacause that is something the compiler macro cannot do), and
;;; then invoke one of the special versions defined above.  On the
;;; other hand, these functions will likely be inlined so that type
;;; inferencing can determine which type of sequence it is at compile
;;; time.

;;; Version on any sequence, from start, no end, no key
(defun find-if-not-from-start-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-start-identity
        predicate sequence start (length sequence)))
    (list
       (find-if-not-list-from-start-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from start, no end, key
(defun find-if-not-from-start-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-start-key
        predicate sequence start (length sequence) key))
    (list
       (find-if-not-list-from-start-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from start, end, no key
(defun find-if-not-from-start-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-start-identity
        predicate sequence start end))
    (list
       (find-if-not-list-from-start-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from start, end, key
(defun find-if-not-from-start-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-start-key
        predicate sequence start end key))
    (list
       (find-if-not-list-from-start-bounded-key
        predicate sequence start end key))))

;;; Version on any sequence, from end, no end, no key
(defun find-if-not-from-end-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-end-identity
        predicate sequence start (length sequence)))
    (list
       (find-if-not-list-from-end-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from end, no end, key
(defun find-if-not-from-end-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-end-key
        predicate sequence start (length sequence) key))
    (list
       (find-if-not-list-from-end-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from end, end, no key
(defun find-if-not-from-end-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-end-identity
        predicate sequence start end))
    (list
       (find-if-not-list-from-end-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from end, end, key
(defun find-if-not-from-end-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (find-if-not-vector-from-end-key
        predicate sequence start end key))
    (list
       (find-if-not-list-from-end-bounded-key
        predicate sequence start end key))))

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
(defun find-if-not (predicate sequence
		    &key
		    (from-end nil)
		    (start 0)
		    (end nil)
		    (key nil))
  ;; FIXME do this better
  (assert (not (minusp start)))
  (if from-end
      (if key
          (if end
              (find-if-not-from-end-bounded-key
               predicate sequence start end key)
              (find-if-not-from-end-unbounded-key
               predicate sequence start key))
          (if end
              (find-if-not-from-end-bounded-identity
               predicate sequence start end)
              (find-if-not-from-end-unbounded-identity
               predicate sequence start)))
      (if key
          (if end
              (find-if-not-from-start-bounded-key
               predicate sequence start end key)
              (find-if-not-from-start-unbounded-key
               predicate sequence start key))
          (if end
              (find-if-not-from-start-bounded-identity
               predicate sequence start end)
              (find-if-not-from-start-unbounded-identity
               predicate sequence start)))))

(define-compiler-macro find-if-not (&whole form &rest args)
  (handler-case 
      (destructuring-bind (predicate sequence
                           &key
                           (from-end nil from-end-p)
                           (start 0 startp)
                           (end nil endp)
                           (key nil keyp))
          args
        (if (and endp (not (null end)))
            (if (and keyp (not (null key)))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-not-from-end-bounded-key
                          ,predicate ,sequence ,start ,end ,key)
                        `(if ,from-end
                             (find-if-not-from-end-bounded-key
                              ,predicate ,sequence ,start ,end ,key)
                             (find-if-not-from-start-bounded-key
                              ,predicate ,sequence ,start ,end ,key)))
                    `(find-if-not-from-start-bounded-key
                      ,predicate ,sequence ,start ,end ,key))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-not-from-end-bounded-identity
                          ,predicate ,sequence ,start ,end)
                        `(if ,from-end
                             (find-if-not-from-end-bounded-identity
                              ,predicate ,sequence ,start ,end)
                             (find-if-not-from-start-bounded-identity
                              ,predicate ,sequence ,start ,end)))
                    `(find-if-not-from-start-bounded-identity
                      ,predicate ,sequence ,start ,end)))
            (if (and keyp (not (null key)))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-not-from-end-unbounded-key
                          ,predicate ,sequence ,start ,key)
                        `(if ,from-end
                             (find-if-not-from-end-unbounded-key
                              ,predicate ,sequence ,start ,key)
                             (find-if-not-from-start-unbounded-key
                              ,predicate ,sequence ,start ,key)))
                    `(find-if-not-from-start-unbounded-key
                      ,predicate ,sequence ,start ,key))
                (if from-end-p
                    (if (eq from-end t)
                        `(find-if-not-from-end-unbounded-identity
                          ,predicate ,sequence ,start)
                        `(if ,from-end
                             (find-if-not-from-end-unbounded-identity
                              ,predicate ,sequence ,start)
                             (find-if-not-from-start-unbounded-identity
                              ,predicate ,sequence ,start)))
                    `(find-if-not-from-start-unbounded-identity
                      ,predicate ,sequence ,start)))))
    (error () form)))

