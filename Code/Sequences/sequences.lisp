(in-package #:sicl-sequences)

;;; Preserving order of evaluation with a compiler macro gets
;;; complicated.  The left-to-right order in the original 
;;; call must be respected.  So for instance, if we have 
;;; a call such as (find-if p s :key <x> :end <y> :start <z>)
;;; the expressions <x>, <y>, and <z> must be evaluated in
;;; that order.  One solution to this problem is to generate
;;; code such as (let ((key <x>) (end <y>) (start <z>)) ...)
;;; and then use the variables key, end, start in the body of
;;; the let.  However, things are a bit more complicated, 
;;; because section 3.4.1.4 of the HyperSpec says that there
;;; can be multiple occurences of a keyword in a call, so that
;;; (find-if p s :key <x> :end <y> :start <z> :key <w>) is
;;; legal.  In this case, <w> must be evaluated last, but 
;;; the value of the :key argument is the value of <x>.  
;;; So we must handle multiple occurences by generating something
;;; like (let ((key <x>) (end <y>) (start <z>) (ignore <w>)) ...) 
;;; where ignore is a unique symbol.  But we must also preserve 
;;; that symbol for later so that we can declare it to be ignored
;;; in order to avoid compiler warnings. 

;;; Yet another complication happens because if the call contains
;;; :allow-other-keys t, then pretty much any other keyword can be 
;;; present.  Again, we generate a unique symbol for that case. 

;;; Things are further complicated by the fact that the special
;;; versions of many functions always take a start parameter.  If
;;; the call doesn't have a :start keyword argument, we need to 
;;; initialize start to 0. 

;;; Translate from a keyword to a variable name
(defparameter *vars* '((:start . start)
                       (:end . end)
                       (:from-end . from-end)
                       (:key . key)
                       (:test . test)
                       (:test-not . test-not)
                       (:count . count)))

;;; For a list with alternating keywords, and expressions, 
;;; generate a list of binding for let.  For instance,
;;; if we have (:key <x> :end <y> :start <z>), we generate
;;; ((key <x>) (end <y>) (start <z>)).  If a keyword occurs 
;;; more than once in the list, generate a binding with a 
;;; generated symbol instead. 
(defun make-bindings (plist)
  (loop with keywords = '()
        for (key value) on plist by #'cddr
        collect (list (if (member key keywords)
                          (gensym)
                          (or (cdr (assoc key *vars*)) (gensym)))
                      value)
        do (push key keywords)))

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
        (declare (ignore start))
        (let ((bindings (make-bindings (cddr args))))
          `(let ((start 0))
             ;; start must have a value in case no :start keyword
             ;; argument was given.  On the other hand, if a :start
             ;; keyword argument WAS given, then this variable will
             ;; be shadowed by the let bindings below, and in that
             ;; case, this variable is not used, which is why we
             ;; declare it ignorable. 
             (declare (ignorable start))
             (let ((predicate ,predicate)
                   (sequence ,sequence)
                   ,@bindings)
               ;; Just make every variable ignorable in
               ;; case there are gensyms among them.
               (declare (ignorable ,@(mapcar #'car bindings)))
               ,(if (and endp (not (null end)))
                    (if (and keyp (not (null key)))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-from-end-bounded-key
                                  predicate sequence start end key)
                                `(if from-end
                                     (find-if-from-end-bounded-key
                                      predicate sequence start end key)
                                     (find-if-from-start-bounded-key
                                      predicate sequence start end key)))
                            `(find-if-from-start-bounded-key
                              predicate sequence start end key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-from-end-bounded-identity
                                  predicate sequence start end)
                                `(if from-end
                                     (find-if-from-end-bounded-identity
                                      predicate sequence start end)
                                     (find-if-from-start-bounded-identity
                                      predicate sequence start end)))
                            `(find-if-from-start-bounded-identity
                              predicate sequence start end)))
                    (if (and keyp (not (null key)))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-from-end-unbounded-key
                                  predicate sequence start key)
                                `(if from-end
                                     (find-if-from-end-unbounded-key
                                      predicate sequence start key)
                                     (find-if-from-start-unbounded-key
                                      predicate sequence start key)))
                            `(find-if-from-start-unbounded-key
                              predicate sequence start key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-from-end-unbounded-identity
                                  predicate sequence start)
                                `(if from-end
                                     (find-if-from-end-unbounded-identity
                                      predicate sequence start)
                                     (find-if-from-start-unbounded-identity
                                      predicate sequence start)))
                            `(find-if-from-start-unbounded-identity
                              predicate sequence start))))))))
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
        (declare (ignore start))
        (let ((bindings (make-bindings (cddr args))))
          `(let ((start 0))
             ;; start must have a value in case no :start keyword
             ;; argument was given.  On the other hand, if a :start
             ;; keyword argument WAS given, then this variable will
             ;; be shadowed by the let bindings below, and in that
             ;; case, this variable is not used, which is why we
             ;; declare it ignorable. 
             (declare (ignorable start))
             (let ((predicate ,predicate)
                   (sequence ,sequence)
                   ,@bindings)
               ;; Just make every variable ignorable in
               ;; case there are gensyms among them.
               (declare (ignorable ,@(mapcar #'car bindings)))
               ,(if (and endp (not (null end)))
                    (if (and keyp (not (null key)))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-not-from-end-bounded-key
                                  predicate sequence start end key)
                                `(if from-end
                                     (find-if-not-from-end-bounded-key
                                      predicate sequence start end key)
                                     (find-if-not-from-start-bounded-key
                                      predicate sequence start end key)))
                            `(find-if-not-from-start-bounded-key
                              predicate sequence start end key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-not-from-end-bounded-identity
                                  predicate sequence start end)
                                `(if from-end
                                     (find-if-not-from-end-bounded-identity
                                      predicate sequence start end)
                                     (find-if-not-from-start-bounded-identity
                                      predicate sequence start end)))
                            `(find-if-not-from-start-bounded-identity
                              predicate sequence start end)))
                    (if (and keyp (not (null key)))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-not-from-end-unbounded-key
                                  predicate sequence start key)
                                `(if from-end
                                     (find-if-not-from-end-unbounded-key
                                      predicate sequence start key)
                                     (find-if-not-from-start-unbounded-key
                                      predicate sequence start key)))
                            `(find-if-not-from-start-unbounded-key
                              predicate sequence start key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(find-if-not-from-end-unbounded-identity
                                  predicate sequence start)
                                `(if from-end
                                     (find-if-not-from-end-unbounded-identity
                                      predicate sequence start)
                                     (find-if-not-from-start-unbounded-identity
                                      predicate sequence start)))
                            `(find-if-not-from-start-unbounded-identity
                              predicate sequence start))))))))
    (error () form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function find

(defun find-list-from-start-unbounded-eq-identity
    (item list start)
  (loop for element in (nthcdr start list)
        when (eq item element)
          return element))  

(defun find-list-from-start-unbounded-eq-key
    (item list start key)
  (loop for element in (nthcdr start list)
        when (eq item (funcall key element))
          return element))  

(defun find-list-from-start-unbounded-not-eq-identity
    (item list start)
  (loop for element in (nthcdr start list)
        when (not (eq item element))
          return element))  

(defun find-list-from-start-unbounded-not-eq-key
    (item list start key)
  (loop for element in (nthcdr start list)
        when (not (eq item (funcall key element)))
          return element))  

(defun find-list-from-start-unbounded-eql-identity
    (item list start)
  (loop for element in (nthcdr start list)
        when (eql item element)
          return element))  

(defun find-list-from-start-unbounded-eql-key
    (item list start key)
  (loop for element in (nthcdr start list)
        when (eql item (funcall key element))
          return element))  

(defun find-list-from-start-unbounded-not-eql-identity
    (item list start)
  (loop for element in (nthcdr start list)
        when (not (eql item element))
          return element))  

(defun find-list-from-start-unbounded-not-eql-key
    (item list start key)
  (loop for element in (nthcdr start list)
        when (not (eql item (funcall key element)))
          return element))  

(defun find-list-from-start-unbounded-test-identity
    (item list start test)
  (loop for element in (nthcdr start list)
        when (funcall test item element)
          return element))  

(defun find-list-from-start-unbounded-test-key
    (item list start test key)
  (loop for element in (nthcdr start list)
        when (funcall test item (funcall key element))
          return element))

(defun find-list-from-start-unbounded-test-not-identity
    (item list start test)
  (loop for element in (nthcdr start list)
        when (not (funcall test item element))
          return element))

(defun find-list-from-start-unbounded-test-not-key
    (item list start test key)
  (loop for element in (nthcdr start list)
        when (not (funcall test item (funcall key element)))
          return element))

(defun find-list-from-start-bounded-eq-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (eq item element)
          return element))  

(defun find-list-from-start-bounded-eq-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (eq item (funcall key element))
          return element))  

(defun find-list-from-start-bounded-not-eq-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (not (eq item element))
          return element))  

(defun find-list-from-start-bounded-not-eq-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (not (eq item (funcall key element)))
          return element))  

(defun find-list-from-start-bounded-eql-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (eql item element)
          return element))  

(defun find-list-from-start-bounded-eql-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (eql item (funcall key element))
          return element))  

(defun find-list-from-start-bounded-not-eql-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (not (eql item element))
          return element))  

(defun find-list-from-start-bounded-not-eql-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (not (eql item (funcall key element)))
          return element))  

(defun find-list-from-start-bounded-test-identity
    (item list start end test)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (funcall test item element)
          return element))  

(defun find-list-from-start-bounded-test-key
    (item list start end test key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (funcall test item (funcall key element))
          return element))

(defun find-list-from-start-bounded-test-not-identity
    (item list start end test)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (not (funcall test item element))
          return element))

(defun find-list-from-start-bounded-test-not-key
    (item list start end test key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (not (funcall test item (funcall key element)))
          return element))

(defun find-list-from-end-unbounded-eq-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        when (eq item element)
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-eq-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (eq item (funcall key element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-not-eq-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        when (not (eq item element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-not-eq-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (not (eq item (funcall key element)))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-eql-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        when (eql item element)
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-eql-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (eql item (funcall key element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-not-eql-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        when (not (eql item element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-not-eql-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (not (eql item (funcall key element)))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-test-identity
    (item list start test)
  (loop with value = nil
        for element in (nthcdr start list)
        when (funcall test item element)
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-test-key
    (item list start test key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (funcall test item (funcall key element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-test-not-identity
    (item list start test)
  (loop with value = nil
        for element in (nthcdr start list)
        when (not (funcall test item element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-unbounded-test-not-key
    (item list start test key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (not (funcall test item (funcall key element)))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-eq-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (eq item element)
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-eq-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (eq item (funcall key element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-not-eq-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (not (eq item element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-not-eq-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (not (eq item (funcall key item)))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-eql-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (eql item element)
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-eql-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (eql item (funcall key element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-not-eql-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (not (eql item element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-not-eql-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (not (eql item (funcall key element)))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-test-identity
    (item list start end test)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (funcall test item element)
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-test-key
    (item list start end test key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (funcall test item (funcall key element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-test-not-identity
    (item list start end test)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (not (funcall test item element))
          do (setf value element)
        finally (return value)))

(defun find-list-from-end-bounded-test-not-key
    (item list start end test key)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (not (funcall test item (funcall key element)))
          do (setf value element)
        finally (return value)))

(defun find-vector-from-start-eq-identity
    (item vector start end)
  (loop for index from start below end
        when (eq item (aref vector index))
          return (aref vector index)))

(defun find-vector-from-start-eq-key
    (item vector start end key)
  (loop for index from start below end
        when (eq item (funcall key (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-start-not-eq-identity
    (item vector start end)
  (loop for index from start below end
        when (not (eq item (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-start-not-eq-key
    (item vector start end key)
  (loop for index from start below end
        when (not (eq item (funcall key (aref vector index))))
          return (aref vector index)))

(defun find-vector-from-start-eql-identity
    (item vector start end)
  (loop for index from start below end
        when (eql item (aref vector index))
          return (aref vector index)))

(defun find-vector-from-start-eql-key
    (item vector start end key)
  (loop for index from start below end
        when (eql item (funcall key (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-start-not-eql-identity
    (item vector start end)
  (loop for index from start below end
        when (not (eql item (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-start-not-eql-key
    (item vector start end key)
  (loop for index from start below end
        when (not (eql item (funcall key (aref vector index))))
          return (aref vector index)))

(defun find-vector-from-start-test-identity
    (item vector start end test)
  (loop for index from start below end
        when (funcall test item (aref vector index))
          return (aref vector index)))

(defun find-vector-from-start-test-key
    (item vector start end test key)
  (loop for index from start below end
        when (funcall test item (funcall key (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-start-test-not-identity
    (item vector start end test)
  (loop for index from start below end
        when (not (funcall test item (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-start-test-not-key
    (item vector start end test key)
  (loop for index from start below end
        when (not (funcall test item (funcall key (aref vector index))))
          return (aref vector index)))

(defun find-vector-from-end-eq-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (eq item (aref vector index))
          return (aref vector index)))

(defun find-vector-from-end-eq-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (eq item (funcall key (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-end-not-eq-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (not (eq item (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-end-not-eq-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (not (eq item (funcall key (aref vector index))))
          return (aref vector index)))

(defun find-vector-from-end-eql-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (eql item (aref vector index))
          return (aref vector index)))

(defun find-vector-from-end-eql-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (eql item (funcall key (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-end-not-eql-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (not (eql item (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-end-not-eql-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (not (eql item (funcall key (aref vector index))))
          return (aref vector index)))

(defun find-vector-from-end-test-identity
    (item vector start end test)
  (loop for index downfrom (1- end) to start
        when (funcall test item (aref vector index))
          return (aref vector index)))

(defun find-vector-from-end-test-key
    (item vector start end test key)
  (loop for index downfrom (1- end) to start
        when (funcall test item (funcall key (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-end-test-not-identity
    (item vector start end test)
  (loop for index downfrom (1- end) to start
        when (not (funcall test item (aref vector index)))
          return (aref vector index)))

(defun find-vector-from-end-test-not-key
    (item vector start end test key)
  (loop for index downfrom (1- end) to start
        when (not (funcall test item (funcall key (aref vector index))))
          return (aref vector index)))

(defun find-from-start-unbounded-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-start-eq-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-start-unbounded-eq-identity
        item sequence start))))

(defun find-from-start-unbounded-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-start-eq-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-start-unbounded-eq-key
        item sequence start key))))

(defun find-from-start-unbounded-not-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eq-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-start-unbounded-not-eq-identity
        item sequence start))))

(defun find-from-start-unbounded-not-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eq-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-start-unbounded-not-eq-key
        item sequence start key))))

(defun find-from-start-unbounded-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-start-eql-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-start-unbounded-eql-identity
        item sequence start))))

(defun find-from-start-unbounded-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-start-eql-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-start-unbounded-eql-key
        item sequence start key))))

(defun find-from-start-unbounded-not-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eql-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-start-unbounded-not-eql-identity
        item sequence start))))

(defun find-from-start-unbounded-not-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eql-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-start-unbounded-not-eql-key
        item sequence start key))))

(defun find-from-start-unbounded-test-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-identity
        item sequence start (length sequence) test))
    (list
       (find-list-from-start-unbounded-test-identity
        item sequence start test))))

(defun find-from-start-unbounded-test-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-key
        item sequence start (length sequence) test key))
    (list
       (find-list-from-start-unbounded-test-key
        item sequence start test key))))

(defun find-from-start-unbounded-test-not-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-not-identity
        item sequence start (length sequence) test))
    (list
       (find-list-from-start-unbounded-test-not-identity
        item sequence start test))))

(defun find-from-start-unbounded-test-not-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-not-key
        item sequence start (length sequence) test key))
    (list
       (find-list-from-start-unbounded-test-not-key
        item sequence start test key))))

(defun find-from-start-bounded-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-start-eq-identity
        item sequence start end))
    (list
       (find-list-from-start-bounded-eq-identity
        item sequence start end))))

(defun find-from-start-bounded-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-start-eq-key
        item sequence start end key))
    (list
       (find-list-from-start-bounded-eq-key
        item sequence start end key))))

(defun find-from-start-bounded-not-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eq-identity
        item sequence start end))
    (list
       (find-list-from-start-bounded-not-eq-identity
        item sequence start end))))

(defun find-from-start-bounded-not-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eq-key
        item sequence start end key))
    (list
       (find-list-from-start-bounded-not-eq-key
        item sequence start end key))))

(defun find-from-start-bounded-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-start-eql-identity
        item sequence start end))
    (list
       (find-list-from-start-bounded-eql-identity
        item sequence start end))))

(defun find-from-start-bounded-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-start-eql-key
        item sequence start end key))
    (list
       (find-list-from-start-bounded-eql-key
        item sequence start end key))))

(defun find-from-start-bounded-not-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eql-identity
        item sequence start end))
    (list
       (find-list-from-start-bounded-not-eql-identity
        item sequence start end))))

(defun find-from-start-bounded-not-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-start-not-eql-key
        item sequence start end key))
    (list
       (find-list-from-start-bounded-not-eql-key
        item sequence start end key))))

(defun find-from-start-bounded-test-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-identity
        item sequence start end test))
    (list
       (find-list-from-start-bounded-test-identity
        item sequence start end test))))

(defun find-from-start-bounded-test-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-key
        item sequence start end test key))
    (list
       (find-list-from-start-bounded-test-key
        item sequence start end test key))))

(defun find-from-start-bounded-test-not-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-not-identity
        item sequence start end test))
    (list
       (find-list-from-start-bounded-test-not-identity
        item sequence start end test))))

(defun find-from-start-bounded-test-not-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (find-vector-from-start-test-not-key
        item sequence start end test key))
    (list
       (find-list-from-start-bounded-test-not-key
        item sequence start end test key))))

(defun find-from-end-unbounded-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-end-eq-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-end-unbounded-eq-identity
        item sequence start))))

(defun find-from-end-unbounded-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-end-eq-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-end-unbounded-eq-key
        item sequence start key))))

(defun find-from-end-unbounded-not-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eq-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-end-unbounded-not-eq-identity
        item sequence start))))

(defun find-from-end-unbounded-not-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eq-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-end-unbounded-not-eq-key
        item sequence start key))))

(defun find-from-end-unbounded-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-end-eql-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-end-unbounded-eql-identity
        item sequence start))))

(defun find-from-end-unbounded-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-end-eql-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-end-unbounded-eql-key
        item sequence start key))))

(defun find-from-end-unbounded-not-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eql-identity
        item sequence start (length sequence)))
    (list
       (find-list-from-end-unbounded-not-eql-identity
        item sequence start))))

(defun find-from-end-unbounded-not-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eql-key
        item sequence start (length sequence) key))
    (list
       (find-list-from-end-unbounded-not-eql-key
        item sequence start key))))

(defun find-from-end-unbounded-test-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-identity
        item sequence start (length sequence) test))
    (list
       (find-list-from-end-unbounded-test-identity
        item sequence start test))))

(defun find-from-end-unbounded-test-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-key
        item sequence start (length sequence) test key))
    (list
       (find-list-from-end-unbounded-test-key
        item sequence start test key))))

(defun find-from-end-unbounded-test-not-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-not-identity
        item sequence start (length sequence) test))
    (list
       (find-list-from-end-unbounded-test-not-identity
        item sequence start test))))

(defun find-from-end-unbounded-test-not-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-not-key
        item sequence start (length sequence) test key))
    (list
       (find-list-from-end-unbounded-test-not-key
        item sequence start test key))))

(defun find-from-end-bounded-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-end-eq-identity
        item sequence start end))
    (list
       (find-list-from-end-bounded-eq-identity
        item sequence start end))))

(defun find-from-end-bounded-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-end-eq-key
        item sequence start end key))
    (list
       (find-list-from-end-bounded-eq-key
        item sequence start end key))))

(defun find-from-end-bounded-not-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eq-identity
        item sequence start end))
    (list
       (find-list-from-end-bounded-not-eq-identity
        item sequence start end))))

(defun find-from-end-bounded-not-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eq-key
        item sequence start end key))
    (list
       (find-list-from-end-bounded-not-eq-key
        item sequence start end key))))

(defun find-from-end-bounded-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-end-eql-identity
        item sequence start end))
    (list
       (find-list-from-end-bounded-eql-identity
        item sequence start end))))

(defun find-from-end-bounded-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-end-eql-key
        item sequence start end key))
    (list
       (find-list-from-end-bounded-eql-key
        item sequence start end key))))

(defun find-from-end-bounded-not-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eql-identity
        item sequence start end))
    (list
       (find-list-from-end-bounded-not-eql-identity
        item sequence start end))))

(defun find-from-end-bounded-not-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (find-vector-from-end-not-eql-key
        item sequence start end key))
    (list
       (find-list-from-end-bounded-not-eql-key
        item sequence start end key))))

(defun find-from-end-bounded-test-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-identity
        item sequence start end test))
    (list
       (find-list-from-end-bounded-test-identity
        item sequence start end test))))

(defun find-from-end-bounded-test-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-key
        item sequence start end test key))
    (list
       (find-list-from-end-bounded-test-key
        item sequence start end test key))))

(defun find-from-end-bounded-test-not-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-not-identity
        item sequence start end test))
    (list
       (find-list-from-end-bounded-test-not-identity
        item sequence start end test))))

(defun find-from-end-bounded-test-not-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (find-vector-from-end-test-not-key
        item sequence start end test key))
    (list
       (find-list-from-end-bounded-test-not-key
        item sequence start end test key))))

(defun find (item sequence
             &key
             from-end
             test
             test-not
             (start 0)
             end
             key)
  ;; FIXME do this better
  (assert (not (minusp start)))
  (assert (or (null test) (null test-not)))
  (if from-end
      (if key
          (if end
              (if test
                  (if (eq test #'eql)
                      (find-from-end-bounded-eql-key
                       item sequence start end key)
                      (if (eq test #'eq)
                          (find-from-end-bounded-eq-key
                           item sequence start end key)
                          (find-from-end-bounded-test-key
                           item sequence start end test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-end-bounded-not-eql-key
                           item sequence start end key)
                          (if (eq test-not #'eq)
                              (find-from-end-bounded-not-eq-key
                               item sequence start end key)
                              (find-from-end-bounded-test-not-key
                               item sequence start end test-not key)))
                      (find-from-end-bounded-eql-key
                       item sequence start end key)))
              (if test
                  (if (eq test #'eql)
                      (find-from-end-unbounded-eql-key
                       item sequence start key)
                      (if (eq test #'eq)
                          (find-from-end-unbounded-eq-key
                           item sequence start key)
                          (find-from-end-unbounded-test-key
                           item sequence start test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-end-unbounded-not-eql-key
                           item sequence start key)
                          (if (eq test-not #'eq)
                              (find-from-end-unbounded-not-eq-key
                               item sequence start key)
                              (find-from-end-unbounded-test-not-key
                               item sequence start test-not key)))
                      (find-from-end-unbounded-eql-key
                       item sequence start key))))
          (if end
              (if test
                  (if (eq test #'eql)
                      (find-from-end-bounded-eql-identity
                       item sequence start end)
                      (if (eq test #'eq)
                          (find-from-end-bounded-eq-identity
                           item sequence start end)
                          (find-from-end-bounded-test-identity
                           item sequence start end test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-end-bounded-not-eql-identity
                           item sequence start end)
                          (if (eq test-not #'eq)
                              (find-from-end-bounded-not-eq-identity
                               item sequence start end)
                              (find-from-end-bounded-test-not-identity
                               item sequence start end test)))
                      (find-from-end-bounded-eql-identity
                       item sequence start end)))
              (if test
                  (if (eq test #'eql)
                      (find-from-end-unbounded-eql-identity
                       item sequence start)
                      (if (eq test #'eq)
                          (find-from-end-unbounded-eq-identity
                           item sequence start)
                          (find-from-end-unbounded-test-identity
                           item sequence start test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-end-unbounded-not-eql-identity
                           item sequence start)
                          (if (eq test-not #'eq)
                              (find-from-end-unbounded-not-eq-identity
                               item sequence start)
                              (find-from-end-unbounded-test-not-identity
                               item sequence start test)))
                      (find-from-end-unbounded-eql-identity
                       item sequence start)))))
      (if key
          (if end
              (if test
                  (if (eq test #'eql)
                      (find-from-start-bounded-eql-key
                       item sequence start end key)
                      (if (eq test #'eq)
                          (find-from-start-bounded-eq-key
                           item sequence start end key)
                          (find-from-start-bounded-test-key
                           item sequence start end test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-start-bounded-not-eql-key
                           item sequence start end key)
                          (if (eq test-not #'eq)
                              (find-from-start-bounded-not-eq-key
                               item sequence start end key)
                              (find-from-start-bounded-test-not-key
                               item sequence start end test-not key)))
                      (find-from-start-bounded-eql-key
                       item sequence start end key)))
              (if test
                  (if (eq test #'eql)
                      (find-from-start-unbounded-eql-key
                       item sequence start key)
                      (if (eq test #'eq)
                          (find-from-start-unbounded-eq-key
                           item sequence start key)
                          (find-from-start-unbounded-test-key
                           item sequence start test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-start-unbounded-not-eql-key
                           item sequence start key)
                          (if (eq test-not #'eq)
                              (find-from-start-unbounded-not-eq-key
                               item sequence start key)
                              (find-from-start-unbounded-test-not-key
                               item sequence start test-not key)))
                      (find-from-start-unbounded-eql-key
                       item sequence start key))))
          (if end
              (if test
                  (if (eq test #'eql)
                      (find-from-start-bounded-eql-identity
                       item sequence start end)
                      (if (eq test #'eq)
                          (find-from-start-bounded-eq-identity
                           item sequence start end)
                          (find-from-start-bounded-test-identity
                           item sequence start end test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-start-bounded-not-eql-identity
                           item sequence start end)
                          (if (eq test-not #'eq)
                              (find-from-start-bounded-not-eq-identity
                               item sequence start end)
                              (find-from-start-bounded-test-not-identity
                               item sequence start end test)))
                      (find-from-start-bounded-eql-identity
                       item sequence start end)))
              (if test
                  (if (eq test #'eql)
                      (find-from-start-unbounded-eql-identity
                       item sequence start)
                      (if (eq test #'eq)
                          (find-from-start-unbounded-eq-identity
                           item sequence start)
                          (find-from-start-unbounded-test-identity
                           item sequence start test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (find-from-start-unbounded-not-eql-identity
                           item sequence start)
                          (if (eq test-not #'eq)
                              (find-from-start-unbounded-not-eq-identity
                               item sequence start)
                              (find-from-start-unbounded-test-not-identity
                               item sequence start test)))
                      (find-from-start-unbounded-eql-identity
                       item sequence start)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function length

(defun length (sequence)
  (etypecase sequence
    (vector
       (if (array-has-fill-pointer-p sequence)
           (fill-pointer sequence)
           (array-dimension sequence 0)))
    (list
       (loop while (not (endp sequence))
             count t
             do (pop sequence)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function reduce

(defun reduce-list-from-start-unbounded-identity-no-initial
    (function list start)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) (funcall function))
          ((null (cdr remaining)) (car remaining))
          (t (loop with value = (car remaining)
                   for element in (cdr remaining)
                   do (setf value (funcall function value element))
                   finally (return value))))))

(defun reduce-list-from-start-unbounded-identity-initial
    (function list start initial)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) initial)
          (t (loop with value = initial
                   for element in remaining
                   do (setf value (funcall function value element))
                   finally (return value))))))

(defun reduce-list-from-start-unbounded-key-no-initial
    (function list start key)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) (funcall function))
          ((null (cdr remaining)) (car remaining))
          (t (loop with value = (funcall key (car remaining))
                   for element in (cdr remaining)
                   do (setf value (funcall function
                                           value
                                           (funcall key element)))
                   finally (return value))))))

(defun reduce-list-from-start-unbounded-key-initial
    (function list start key initial)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) initial)
          (t (loop with value = initial
                   for element in remaining
                   do (setf value (funcall function
                                           value
                                           (funcall key element)))
                   finally (return value))))))


(defun reduce-list-from-start-bounded-identity-no-initial
    (function list start end)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) (funcall function))
          ((null (cdr remaining)) (car remaining))
          (t (loop with value = (car remaining)
                   for element in (cdr remaining)
                   repeat (- end start 1)
                   do (setf value (funcall function value element))
                   finally (return value))))))

(defun reduce-list-from-start-bounded-identity-initial
    (function list start end initial)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) initial)
          (t (loop with value = initial
                   for element in remaining
                   repeat (- end start)
                   do (setf value (funcall function value element))
                   finally (return value))))))

(defun reduce-list-from-start-bounded-key-no-initial
    (function list start end key)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) (funcall function))
          ((null (cdr remaining)) (car remaining))
          (t (loop with value = (funcall key (car remaining))
                   for element in (cdr remaining)
                   repeat (- end start 1)
                   do (setf value (funcall function
                                           value
                                           (funcall key element)))
                   finally (return value))))))

(defun reduce-list-from-start-bounded-key-initial
    (function list start end key initial)
  (let ((remaining (nthcdr start list)))
    (cond ((null remaining) initial)
          (t (loop with value = initial
                   for element in remaining
                   repeat (- end start)
                   do (setf value (funcall function
                                           value
                                           (funcall key element)))
                   finally (return value))))))

(defun reduce-list-from-end-unbounded-identity-no-initial
    (function list start)
  (reduce-list-from-start-unbounded-identity-no-initial
   function (reverse (nthcdr start list)) 0))

(defun reduce-list-from-end-unbounded-identity-initial
    (function list start initial)
  (reduce-list-from-start-unbounded-identity-initial
   function (reverse (nthcdr start list)) 0 initial))

(defun reduce-list-from-end-unbounded-key-no-initial
    (function list start key)
  (reduce-list-from-start-unbounded-key-no-initial
   function (reverse (nthcdr start list)) 0 key))

(defun reduce-list-from-end-unbounded-key-initial
    (function list start key initial)
  (reduce-list-from-start-unbounded-key-initial
   function (reverse (nthcdr start list)) 0 key initial))

(defun reduce-list-from-end-bounded-identity-no-initial
    (function list start end)
  (reduce-list-from-start-unbounded-identity-no-initial
   function (nreverse (subseq list start end)) 0))

(defun reduce-list-from-end-bounded-identity-initial
    (function list start end initial)
  (reduce-list-from-start-unbounded-identity-initial
   function (nreverse (subseq list start end)) 0 initial))

(defun reduce-list-from-end-bounded-key-no-initial
    (function list start end key)
  (reduce-list-from-start-unbounded-key-no-initial
   function (nreverse (subseq list start end)) 0 key))

(defun reduce-list-from-end-bounded-key-initial
    (function list start end key initial)
  (reduce-list-from-start-unbounded-key-initial
   function (nreverse (subseq list start end)) 0 key initial))

(defun reduce-vector-from-start-identity-no-initial
    (function vector start end)
  (cond ((<= end start) (funcall function))
        ((= 1 (- end start)) (aref vector start))
        (t (loop with value = (aref vector start)
                 for index from (1+ start) below end
                 do (setf value (funcall function
                                         value
                                         (aref vector index)))
                 finally (return value)))))

(defun reduce-vector-from-start-identity-initial
    (function vector start end initial)
  (cond ((<= end start) initial)
        (t (loop with value = initial
                 for index from start below end
                 do (setf value (funcall function
                                         value
                                         (aref vector index)))
                 finally (return value)))))

(defun reduce-vector-from-start-key-no-initial
    (function vector start end key)
  (cond ((<= end start) (funcall function))
        ((= 1 (- end start)) (aref vector start))
        (t (loop with value = (funcall key (aref vector start))
                 for index from (1+ start) below end
                 do (setf value (funcall function
                                         value
                                         (funcall key (aref vector index))))
                 finally (return value)))))


(defun reduce-vector-from-start-key-initial
    (function vector start end key initial)
  (cond ((<= end start) initial)
        (t (loop with value = initial
                 for index from start below end
                 do (setf value (funcall function
                                         value
                                         (funcall key (aref vector index))))
                 finally (return value)))))

(defun reduce-vector-from-end-identity-no-initial
    (function vector start end)
  (cond ((<= end start) (funcall function))
        ((= 1 (- end start)) (aref vector start))
        (t (loop with value = (aref vector (1- start))
                 for index downfrom (- end 2) to start
                 do (setf value (funcall function
                                         value
                                         (aref vector index)))
                 finally (return value)))))

(defun reduce-vector-from-end-identity-initial
    (function vector start end initial)
  (cond ((<= end start) initial)
        (t (loop with value = initial
                 for index downfrom (1- end) to start
                 do (setf value (funcall function
                                         value
                                         (aref vector index)))
                 finally (return value)))))

(defun reduce-vector-from-end-key-no-initial
    (function vector start end key)
  (cond ((<= end start) (funcall function))
        ((= 1 (- end start)) (aref vector start))
        (t (loop with value = (funcall key (aref vector (1- end)))
                 for index downfrom (- end 2) to start
                 do (setf value (funcall function
                                         value
                                         (funcall key (aref vector index))))
                 finally (return value)))))


(defun reduce-vector-from-end-key-initial
    (function vector start end key initial)
  (cond ((<= end start) initial)
        (t (loop with value = initial
                 for index downfrom (1- end) to start
                 do (setf value (funcall function
                                         value
                                         (funcall key (aref vector index))))
                 finally (return value)))))

(defun reduce-from-start-unbounded-identity-no-initial
    (function sequence start)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-identity-no-initial
        function sequence start (length sequence)))
    (list 
       (reduce-list-from-start-unbounded-identity-no-initial
        function sequence start))))
       
(defun reduce-from-start-unbounded-identity-initial
    (function sequence start initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-identity-initial
        function sequence start (length sequence) initial))
    (list 
       (reduce-list-from-start-unbounded-identity-initial
        function sequence start initial))))

(defun reduce-from-start-unbounded-key-no-initial
    (function sequence start key)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-key-no-initial
        function sequence start (length sequence) key))
    (list 
       (reduce-list-from-start-unbounded-key-no-initial
        function sequence start key))))
       
(defun reduce-from-start-unbounded-key-initial
    (function sequence start key initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-key-initial
        function sequence start (length sequence) key initial))
    (list 
       (reduce-list-from-start-unbounded-key-initial
        function sequence start key initial))))

(defun reduce-from-start-bounded-identity-no-initial
    (function sequence start end)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-identity-no-initial
        function sequence start end))
    (list 
       (reduce-list-from-start-bounded-identity-no-initial
        function sequence start end))))
       
(defun reduce-from-start-bounded-identity-initial
    (function sequence start end initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-identity-initial
        function sequence start end initial))
    (list 
       (reduce-list-from-start-bounded-identity-initial
        function sequence start end initial))))

(defun reduce-from-start-bounded-key-no-initial
    (function sequence start end key)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-key-no-initial
        function sequence start end key))
    (list 
       (reduce-list-from-start-bounded-key-no-initial
        function sequence start end key))))
       
(defun reduce-from-start-bounded-key-initial
    (function sequence start end key initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-start-key-initial
        function sequence start end key initial))
    (list 
       (reduce-list-from-start-bounded-key-initial
        function sequence start end key initial))))

(defun reduce-from-end-unbounded-identity-no-initial
    (function sequence start)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-identity-no-initial
        function sequence start (length sequence)))
    (list 
       (reduce-list-from-end-unbounded-identity-no-initial
        function sequence start))))
       
(defun reduce-from-end-unbounded-identity-initial
    (function sequence start initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-identity-initial
        function sequence start (length sequence) initial))
    (list 
       (reduce-list-from-end-unbounded-identity-initial
        function sequence start initial))))

(defun reduce-from-end-unbounded-key-no-initial
    (function sequence start key)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-key-no-initial
        function sequence start (length sequence) key))
    (list 
       (reduce-list-from-end-unbounded-key-no-initial
        function sequence start key))))
       
(defun reduce-from-end-unbounded-key-initial
    (function sequence start key initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-key-initial
        function sequence start (length sequence) key initial))
    (list 
       (reduce-list-from-end-unbounded-key-initial
        function sequence start key initial))))

(defun reduce-from-end-bounded-identity-no-initial
    (function sequence start end)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-identity-no-initial
        function sequence start end))
    (list 
       (reduce-list-from-end-bounded-identity-no-initial
        function sequence start end))))
       
(defun reduce-from-end-bounded-identity-initial
    (function sequence start end initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-identity-initial
        function sequence start end initial))
    (list 
       (reduce-list-from-end-bounded-identity-initial
        function sequence start end initial))))

(defun reduce-from-end-bounded-key-no-initial
    (function sequence start end key)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-key-no-initial
        function sequence start end key))
    (list 
       (reduce-list-from-end-bounded-key-no-initial
        function sequence start end key))))
       
(defun reduce-from-end-bounded-key-initial
    (function sequence start end key initial)
  (etypecase sequence
    (vector
       (reduce-vector-from-end-key-initial
        function sequence start end key initial))
    (list 
       (reduce-list-from-end-bounded-key-initial
        function sequence start end key initial))))

(defun reduce (function sequence
               &key
               key
               from-end
               (start 0)
               end
               (initial-value nil initial-value-p))
    (if from-end
        (if key
          (if end
              (if initial-value-p
                  (reduce-from-end-bounded-key-initial
                   function sequence start end key initial-value)
                  (reduce-from-end-bounded-key-no-initial
                   function sequence start end key))
              (if initial-value-p
                  (reduce-from-end-unbounded-key-initial
                   function sequence start key initial-value)
                  (reduce-from-end-unbounded-key-no-initial
                   function sequence start key)))
          (if end
              (if initial-value-p
                  (reduce-from-end-bounded-identity-initial
                   function sequence start end initial-value)
                  (reduce-from-end-bounded-identity-no-initial
                   function sequence start end))
              (if initial-value-p
                  (reduce-from-end-unbounded-identity-initial
                   function sequence start initial-value)
                  (reduce-from-end-unbounded-identity-no-initial
                   function sequence start))))
        (if key
          (if end
              (if initial-value-p
                  (reduce-from-start-bounded-key-initial
                   function sequence start end key initial-value)
                  (reduce-from-start-bounded-key-no-initial
                   function sequence start end key))
              (if initial-value-p
                  (reduce-from-start-unbounded-key-initial
                   function sequence start key initial-value)
                  (reduce-from-start-unbounded-key-no-initial
                   function sequence start key)))
          (if end
              (if initial-value-p
                  (reduce-from-start-bounded-identity-initial
                   function sequence start end initial-value)
                  (reduce-from-start-bounded-identity-no-initial
                   function sequence start end))
              (if initial-value-p
                  (reduce-from-start-unbounded-identity-initial
                   function sequence start initial-value)
                  (reduce-from-start-unbounded-identity-no-initial
                   function sequence start))))))
