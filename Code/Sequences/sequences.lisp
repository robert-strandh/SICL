(in-package #:sicl-sequences)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tools for writing compiler macros.

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
;;; Conditions

(define-condition invalid-bounding-index (error)
  ((%sequence-length :initarg :sequence-length :reader sequence-length)))

(define-condition invalid-start-index (invalid-bounding-index)
  ((%start :initarg :start :reader start)))

(define-condition invalid-end-index (invalid-bounding-index)
  ((%end :initarg :end :reader end)))

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
;;; Function position

(defun position-list-from-start-unbounded-eq-identity
    (item list start)
  (loop for element in (nthcdr start list)
        for index from start
        when (eq item element)
          return index))  

(defun position-list-from-start-unbounded-eq-key
    (item list start key)
  (loop for element in (nthcdr start list)
        for index from start
        when (eq item (funcall key element))
          return index))  

(defun position-list-from-start-unbounded-not-eq-identity
    (item list start)
  (loop for element in (nthcdr start list)
        for index from start
        when (not (eq item element))
          return index))  

(defun position-list-from-start-unbounded-not-eq-key
    (item list start key)
  (loop for element in (nthcdr start list)
        for index from start
        when (not (eq item (funcall key element)))
          return index))  

(defun position-list-from-start-unbounded-eql-identity
    (item list start)
  (loop for element in (nthcdr start list)
        for index from start
        when (eql item element)
          return index))  

(defun position-list-from-start-unbounded-eql-key
    (item list start key)
  (loop for element in (nthcdr start list)
        for index from start
        when (eql item (funcall key element))
          return index))  

(defun position-list-from-start-unbounded-not-eql-identity
    (item list start)
  (loop for element in (nthcdr start list)
        for index from start
        when (not (eql item element))
          return index))  

(defun position-list-from-start-unbounded-not-eql-key
    (item list start key)
  (loop for element in (nthcdr start list)
        for index from start
        when (not (eql item (funcall key element)))
          return index))  

(defun position-list-from-start-unbounded-test-identity
    (item list start test)
  (loop for element in (nthcdr start list)
        for index from start
        when (funcall test item element)
          return index))  

(defun position-list-from-start-unbounded-test-key
    (item list start test key)
  (loop for element in (nthcdr start list)
        for index from start
        when (funcall test item (funcall key element))
          return index))

(defun position-list-from-start-unbounded-test-not-identity
    (item list start test)
  (loop for element in (nthcdr start list)
        for index from start
        when (not (funcall test item element))
          return index))

(defun position-list-from-start-unbounded-test-not-key
    (item list start test key)
  (loop for element in (nthcdr start list)
        for index from start
        when (not (funcall test item (funcall key element)))
          return index))

(defun position-list-from-start-bounded-eq-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (eq item element)
          return index))  

(defun position-list-from-start-bounded-eq-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (eq item (funcall key element))
          return index))  

(defun position-list-from-start-bounded-not-eq-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (not (eq item element))
          return index))  

(defun position-list-from-start-bounded-not-eq-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (not (eq item (funcall key element)))
          return index))  

(defun position-list-from-start-bounded-eql-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (eql item element)
          return index))  

(defun position-list-from-start-bounded-eql-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (eql item (funcall key element))
          return index))  

(defun position-list-from-start-bounded-not-eql-identity
    (item list start end)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (not (eql item element))
          return index))  

(defun position-list-from-start-bounded-not-eql-key
    (item list start end key)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (not (eql item (funcall key element)))
          return index))  

(defun position-list-from-start-bounded-test-identity
    (item list start end test)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (funcall test item element)
          return index))  

(defun position-list-from-start-bounded-test-key
    (item list start end test key)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (funcall test item (funcall key element))
          return index))

(defun position-list-from-start-bounded-test-not-identity
    (item list start end test)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (not (funcall test item element))
          return index))

(defun position-list-from-start-bounded-test-not-key
    (item list start end test key)
  (loop for element in (nthcdr start list)
        for index from start below end
        when (not (funcall test item (funcall key element)))
          return index))

(defun position-list-from-end-unbounded-eq-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (eq item element)
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-eq-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (eq item (funcall key element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-not-eq-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (not (eq item element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-not-eq-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (not (eq item (funcall key element)))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-eql-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (eql item element)
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-eql-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (eql item (funcall key element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-not-eql-identity
    (item list start)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (not (eql item element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-not-eql-key
    (item list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (not (eql item (funcall key element)))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-test-identity
    (item list start test)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (funcall test item element)
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-test-key
    (item list start test key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (funcall test item (funcall key element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-test-not-identity
    (item list start test)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (not (funcall test item element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-unbounded-test-not-key
    (item list start test key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start
        when (not (funcall test item (funcall key element)))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-eq-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (eq item element)
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-eq-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (eq item (funcall key element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-not-eq-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (not (eq item element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-not-eq-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (not (eq item (funcall key item)))
          do (setf value element)
        finally (return value)))

(defun position-list-from-end-bounded-eql-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (eql item element)
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-eql-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (eql item (funcall key element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-not-eql-identity
    (item list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (not (eql item element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-not-eql-key
    (item list start end key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (not (eql item (funcall key element)))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-test-identity
    (item list start end test)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (funcall test item element)
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-test-key
    (item list start end test key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (funcall test item (funcall key element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-test-not-identity
    (item list start end test)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (not (funcall test item element))
          do (setf value index)
        finally (return value)))

(defun position-list-from-end-bounded-test-not-key
    (item list start end test key)
  (loop with value = nil
        for element in (nthcdr start list)
        for index from start below end
        when (not (funcall test item (funcall key element)))
          do (setf value index)
        finally (return value)))

(defun position-vector-from-start-eq-identity
    (item vector start end)
  (loop for index from start below end
        when (eq item (aref vector index))
          return index))

(defun position-vector-from-start-eq-key
    (item vector start end key)
  (loop for index from start below end
        when (eq item (funcall key (aref vector index)))
          return index))

(defun position-vector-from-start-not-eq-identity
    (item vector start end)
  (loop for index from start below end
        when (not (eq item (aref vector index)))
          return index))

(defun position-vector-from-start-not-eq-key
    (item vector start end key)
  (loop for index from start below end
        when (not (eq item (funcall key (aref vector index))))
          return index))

(defun position-vector-from-start-eql-identity
    (item vector start end)
  (loop for index from start below end
        when (eql item (aref vector index))
          return index))

(defun position-vector-from-start-eql-key
    (item vector start end key)
  (loop for index from start below end
        when (eql item (funcall key (aref vector index)))
          return index))

(defun position-vector-from-start-not-eql-identity
    (item vector start end)
  (loop for index from start below end
        when (not (eql item (aref vector index)))
          return index))

(defun position-vector-from-start-not-eql-key
    (item vector start end key)
  (loop for index from start below end
        when (not (eql item (funcall key (aref vector index))))
          return index))

(defun position-vector-from-start-test-identity
    (item vector start end test)
  (loop for index from start below end
        when (funcall test item (aref vector index))
          return index))

(defun position-vector-from-start-test-key
    (item vector start end test key)
  (loop for index from start below end
        when (funcall test item (funcall key (aref vector index)))
          return index))

(defun position-vector-from-start-test-not-identity
    (item vector start end test)
  (loop for index from start below end
        when (not (funcall test item (aref vector index)))
          return index))

(defun position-vector-from-start-test-not-key
    (item vector start end test key)
  (loop for index from start below end
        when (not (funcall test item (funcall key (aref vector index))))
          return index))

(defun position-vector-from-end-eq-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (eq item (aref vector index))
          return index))

(defun position-vector-from-end-eq-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (eq item (funcall key (aref vector index)))
          return index))

(defun position-vector-from-end-not-eq-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (not (eq item (aref vector index)))
          return index))

(defun position-vector-from-end-not-eq-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (not (eq item (funcall key (aref vector index))))
          return index))

(defun position-vector-from-end-eql-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (eql item (aref vector index))
          return index))

(defun position-vector-from-end-eql-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (eql item (funcall key (aref vector index)))
          return index))

(defun position-vector-from-end-not-eql-identity
    (item vector start end)
  (loop for index downfrom (1- end) to start
        when (not (eql item (aref vector index)))
          return index))

(defun position-vector-from-end-not-eql-key
    (item vector start end key)
  (loop for index downfrom (1- end) to start
        when (not (eql item (funcall key (aref vector index))))
          return index))

(defun position-vector-from-end-test-identity
    (item vector start end test)
  (loop for index downfrom (1- end) to start
        when (funcall test item (aref vector index))
          return index))

(defun position-vector-from-end-test-key
    (item vector start end test key)
  (loop for index downfrom (1- end) to start
        when (funcall test item (funcall key (aref vector index)))
          return index))

(defun position-vector-from-end-test-not-identity
    (item vector start end test)
  (loop for index downfrom (1- end) to start
        when (not (funcall test item (aref vector index)))
          return index))

(defun position-vector-from-end-test-not-key
    (item vector start end test key)
  (loop for index downfrom (1- end) to start
        when (not (funcall test item (funcall key (aref vector index))))
          return index))

(defun position-from-start-unbounded-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-start-eq-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-start-unbounded-eq-identity
        item sequence start))))

(defun position-from-start-unbounded-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-start-eq-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-start-unbounded-eq-key
        item sequence start key))))

(defun position-from-start-unbounded-not-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eq-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-start-unbounded-not-eq-identity
        item sequence start))))

(defun position-from-start-unbounded-not-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eq-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-start-unbounded-not-eq-key
        item sequence start key))))

(defun position-from-start-unbounded-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-start-eql-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-start-unbounded-eql-identity
        item sequence start))))

(defun position-from-start-unbounded-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-start-eql-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-start-unbounded-eql-key
        item sequence start key))))

(defun position-from-start-unbounded-not-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eql-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-start-unbounded-not-eql-identity
        item sequence start))))

(defun position-from-start-unbounded-not-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eql-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-start-unbounded-not-eql-key
        item sequence start key))))

(defun position-from-start-unbounded-test-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-identity
        item sequence start (length sequence) test))
    (list
       (position-list-from-start-unbounded-test-identity
        item sequence start test))))

(defun position-from-start-unbounded-test-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-key
        item sequence start (length sequence) test key))
    (list
       (position-list-from-start-unbounded-test-key
        item sequence start test key))))

(defun position-from-start-unbounded-test-not-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-not-identity
        item sequence start (length sequence) test))
    (list
       (position-list-from-start-unbounded-test-not-identity
        item sequence start test))))

(defun position-from-start-unbounded-test-not-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-not-key
        item sequence start (length sequence) test key))
    (list
       (position-list-from-start-unbounded-test-not-key
        item sequence start test key))))

(defun position-from-start-bounded-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-start-eq-identity
        item sequence start end))
    (list
       (position-list-from-start-bounded-eq-identity
        item sequence start end))))

(defun position-from-start-bounded-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-start-eq-key
        item sequence start end key))
    (list
       (position-list-from-start-bounded-eq-key
        item sequence start end key))))

(defun position-from-start-bounded-not-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eq-identity
        item sequence start end))
    (list
       (position-list-from-start-bounded-not-eq-identity
        item sequence start end))))

(defun position-from-start-bounded-not-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eq-key
        item sequence start end key))
    (list
       (position-list-from-start-bounded-not-eq-key
        item sequence start end key))))

(defun position-from-start-bounded-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-start-eql-identity
        item sequence start end))
    (list
       (position-list-from-start-bounded-eql-identity
        item sequence start end))))

(defun position-from-start-bounded-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-start-eql-key
        item sequence start end key))
    (list
       (position-list-from-start-bounded-eql-key
        item sequence start end key))))

(defun position-from-start-bounded-not-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eql-identity
        item sequence start end))
    (list
       (position-list-from-start-bounded-not-eql-identity
        item sequence start end))))

(defun position-from-start-bounded-not-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-start-not-eql-key
        item sequence start end key))
    (list
       (position-list-from-start-bounded-not-eql-key
        item sequence start end key))))

(defun position-from-start-bounded-test-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-identity
        item sequence start end test))
    (list
       (position-list-from-start-bounded-test-identity
        item sequence start end test))))

(defun position-from-start-bounded-test-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-key
        item sequence start end test key))
    (list
       (position-list-from-start-bounded-test-key
        item sequence start end test key))))

(defun position-from-start-bounded-test-not-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-not-identity
        item sequence start end test))
    (list
       (position-list-from-start-bounded-test-not-identity
        item sequence start end test))))

(defun position-from-start-bounded-test-not-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (position-vector-from-start-test-not-key
        item sequence start end test key))
    (list
       (position-list-from-start-bounded-test-not-key
        item sequence start end test key))))

(defun position-from-end-unbounded-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-end-eq-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-end-unbounded-eq-identity
        item sequence start))))

(defun position-from-end-unbounded-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-end-eq-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-end-unbounded-eq-key
        item sequence start key))))

(defun position-from-end-unbounded-not-eq-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eq-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-end-unbounded-not-eq-identity
        item sequence start))))

(defun position-from-end-unbounded-not-eq-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eq-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-end-unbounded-not-eq-key
        item sequence start key))))

(defun position-from-end-unbounded-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-end-eql-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-end-unbounded-eql-identity
        item sequence start))))

(defun position-from-end-unbounded-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-end-eql-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-end-unbounded-eql-key
        item sequence start key))))

(defun position-from-end-unbounded-not-eql-identity
    (item sequence start)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eql-identity
        item sequence start (length sequence)))
    (list
       (position-list-from-end-unbounded-not-eql-identity
        item sequence start))))

(defun position-from-end-unbounded-not-eql-key
    (item sequence start key)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eql-key
        item sequence start (length sequence) key))
    (list
       (position-list-from-end-unbounded-not-eql-key
        item sequence start key))))

(defun position-from-end-unbounded-test-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-identity
        item sequence start (length sequence) test))
    (list
       (position-list-from-end-unbounded-test-identity
        item sequence start test))))

(defun position-from-end-unbounded-test-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-key
        item sequence start (length sequence) test key))
    (list
       (position-list-from-end-unbounded-test-key
        item sequence start test key))))

(defun position-from-end-unbounded-test-not-identity
    (item sequence start test)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-not-identity
        item sequence start (length sequence) test))
    (list
       (position-list-from-end-unbounded-test-not-identity
        item sequence start test))))

(defun position-from-end-unbounded-test-not-key
    (item sequence start test key)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-not-key
        item sequence start (length sequence) test key))
    (list
       (position-list-from-end-unbounded-test-not-key
        item sequence start test key))))

(defun position-from-end-bounded-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-end-eq-identity
        item sequence start end))
    (list
       (position-list-from-end-bounded-eq-identity
        item sequence start end))))

(defun position-from-end-bounded-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-end-eq-key
        item sequence start end key))
    (list
       (position-list-from-end-bounded-eq-key
        item sequence start end key))))

(defun position-from-end-bounded-not-eq-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eq-identity
        item sequence start end))
    (list
       (position-list-from-end-bounded-not-eq-identity
        item sequence start end))))

(defun position-from-end-bounded-not-eq-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eq-key
        item sequence start end key))
    (list
       (position-list-from-end-bounded-not-eq-key
        item sequence start end key))))

(defun position-from-end-bounded-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-end-eql-identity
        item sequence start end))
    (list
       (position-list-from-end-bounded-eql-identity
        item sequence start end))))

(defun position-from-end-bounded-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-end-eql-key
        item sequence start end key))
    (list
       (position-list-from-end-bounded-eql-key
        item sequence start end key))))

(defun position-from-end-bounded-not-eql-identity
    (item sequence start end)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eql-identity
        item sequence start end))
    (list
       (position-list-from-end-bounded-not-eql-identity
        item sequence start end))))

(defun position-from-end-bounded-not-eql-key
    (item sequence start end key)
  (etypecase sequence
    (vector
       (position-vector-from-end-not-eql-key
        item sequence start end key))
    (list
       (position-list-from-end-bounded-not-eql-key
        item sequence start end key))))

(defun position-from-end-bounded-test-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-identity
        item sequence start end test))
    (list
       (position-list-from-end-bounded-test-identity
        item sequence start end test))))

(defun position-from-end-bounded-test-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-key
        item sequence start end test key))
    (list
       (position-list-from-end-bounded-test-key
        item sequence start end test key))))

(defun position-from-end-bounded-test-not-identity
    (item sequence start end test)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-not-identity
        item sequence start end test))
    (list
       (position-list-from-end-bounded-test-not-identity
        item sequence start end test))))

(defun position-from-end-bounded-test-not-key
    (item sequence start end test key)
  (etypecase sequence
    (vector
       (position-vector-from-end-test-not-key
        item sequence start end test key))
    (list
       (position-list-from-end-bounded-test-not-key
        item sequence start end test key))))

(defun position (item sequence
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
                      (position-from-end-bounded-eql-key
                       item sequence start end key)
                      (if (eq test #'eq)
                          (position-from-end-bounded-eq-key
                           item sequence start end key)
                          (position-from-end-bounded-test-key
                           item sequence start end test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-end-bounded-not-eql-key
                           item sequence start end key)
                          (if (eq test-not #'eq)
                              (position-from-end-bounded-not-eq-key
                               item sequence start end key)
                              (position-from-end-bounded-test-not-key
                               item sequence start end test-not key)))
                      (position-from-end-bounded-eql-key
                       item sequence start end key)))
              (if test
                  (if (eq test #'eql)
                      (position-from-end-unbounded-eql-key
                       item sequence start key)
                      (if (eq test #'eq)
                          (position-from-end-unbounded-eq-key
                           item sequence start key)
                          (position-from-end-unbounded-test-key
                           item sequence start test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-end-unbounded-not-eql-key
                           item sequence start key)
                          (if (eq test-not #'eq)
                              (position-from-end-unbounded-not-eq-key
                               item sequence start key)
                              (position-from-end-unbounded-test-not-key
                               item sequence start test-not key)))
                      (position-from-end-unbounded-eql-key
                       item sequence start key))))
          (if end
              (if test
                  (if (eq test #'eql)
                      (position-from-end-bounded-eql-identity
                       item sequence start end)
                      (if (eq test #'eq)
                          (position-from-end-bounded-eq-identity
                           item sequence start end)
                          (position-from-end-bounded-test-identity
                           item sequence start end test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-end-bounded-not-eql-identity
                           item sequence start end)
                          (if (eq test-not #'eq)
                              (position-from-end-bounded-not-eq-identity
                               item sequence start end)
                              (position-from-end-bounded-test-not-identity
                               item sequence start end test)))
                      (position-from-end-bounded-eql-identity
                       item sequence start end)))
              (if test
                  (if (eq test #'eql)
                      (position-from-end-unbounded-eql-identity
                       item sequence start)
                      (if (eq test #'eq)
                          (position-from-end-unbounded-eq-identity
                           item sequence start)
                          (position-from-end-unbounded-test-identity
                           item sequence start test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-end-unbounded-not-eql-identity
                           item sequence start)
                          (if (eq test-not #'eq)
                              (position-from-end-unbounded-not-eq-identity
                               item sequence start)
                              (position-from-end-unbounded-test-not-identity
                               item sequence start test)))
                      (position-from-end-unbounded-eql-identity
                       item sequence start)))))
      (if key
          (if end
              (if test
                  (if (eq test #'eql)
                      (position-from-start-bounded-eql-key
                       item sequence start end key)
                      (if (eq test #'eq)
                          (position-from-start-bounded-eq-key
                           item sequence start end key)
                          (position-from-start-bounded-test-key
                           item sequence start end test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-start-bounded-not-eql-key
                           item sequence start end key)
                          (if (eq test-not #'eq)
                              (position-from-start-bounded-not-eq-key
                               item sequence start end key)
                              (position-from-start-bounded-test-not-key
                               item sequence start end test-not key)))
                      (position-from-start-bounded-eql-key
                       item sequence start end key)))
              (if test
                  (if (eq test #'eql)
                      (position-from-start-unbounded-eql-key
                       item sequence start key)
                      (if (eq test #'eq)
                          (position-from-start-unbounded-eq-key
                           item sequence start key)
                          (position-from-start-unbounded-test-key
                           item sequence start test key)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-start-unbounded-not-eql-key
                           item sequence start key)
                          (if (eq test-not #'eq)
                              (position-from-start-unbounded-not-eq-key
                               item sequence start key)
                              (position-from-start-unbounded-test-not-key
                               item sequence start test-not key)))
                      (position-from-start-unbounded-eql-key
                       item sequence start key))))
          (if end
              (if test
                  (if (eq test #'eql)
                      (position-from-start-bounded-eql-identity
                       item sequence start end)
                      (if (eq test #'eq)
                          (position-from-start-bounded-eq-identity
                           item sequence start end)
                          (position-from-start-bounded-test-identity
                           item sequence start end test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-start-bounded-not-eql-identity
                           item sequence start end)
                          (if (eq test-not #'eq)
                              (position-from-start-bounded-not-eq-identity
                               item sequence start end)
                              (position-from-start-bounded-test-not-identity
                               item sequence start end test)))
                      (position-from-start-bounded-eql-identity
                       item sequence start end)))
              (if test
                  (if (eq test #'eql)
                      (position-from-start-unbounded-eql-identity
                       item sequence start)
                      (if (eq test #'eq)
                          (position-from-start-unbounded-eq-identity
                           item sequence start)
                          (position-from-start-unbounded-test-identity
                           item sequence start test)))
                  (if test-not
                      (if (eq test-not #'eql)
                          (position-from-start-unbounded-not-eql-identity
                           item sequence start)
                          (if (eq test-not #'eq)
                              (position-from-start-unbounded-not-eq-identity
                               item sequence start)
                              (position-from-start-unbounded-test-not-identity
                               item sequence start test)))
                      (position-from-start-unbounded-eql-identity
                       item sequence start)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function position-if

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
;;; as we position an element that satisfies the test.  When 
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
(defun position-if-list-from-start-unbounded-identity
    (predicate list start)
  (loop for element in (nthcdr start list)
        when (funcall predicate element)
          return element))

;;; Version on lists, from start, no end, key
(defun position-if-list-from-start-unbounded-key
    (predicate list start key)
  (loop for element in (nthcdr start list)
        when (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from start, end, no key
(defun position-if-list-from-start-bounded-identity
    (predicate list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (funcall predicate element)
          return element))

;;; Version on lists, from start, end, key
(defun position-if-list-from-start-bounded-key
    (predicate list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        when (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from end, no end, no key
(defun position-if-list-from-end-unbounded-identity
    (predicate list start)
  (loop with value = nil
        for element in (nthcdr start list)
        when (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, no end, key
(defun position-if-list-from-end-unbounded-key
    (predicate list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        when (funcall predicate (funcall key element))
          do (setf value element)
        finally (return value)))
  
;;; Version on lists, from end, end, no key
(defun position-if-list-from-end-bounded-identity
    (predicate list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        when (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, end, key
(defun position-if-list-from-end-bounded-key
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
(defun position-if-vector-from-start-identity
    (predicate vector start end)
  (loop for index from start below (min end (length vector))
        when (funcall predicate (aref vector index))
          return index))

;;; Version on vectors, from start, key
(defun position-if-vector-from-start-key
    (predicate vector start end key)
  (loop for index from start below (min end (length vector))
        when (funcall predicate (funcall key (aref vector index)))
          return index))

;;; Version on vectors, from end, no key
(defun position-if-vector-from-end-identity
    (predicate vector start end)
  (loop for index downfrom (1- (min end (length vector))) to start
        when (funcall predicate (aref vector index))
          return index))

;;; Version on vectors, from end, key
(defun position-if-vector-from-end-key
    (predicate vector start end key)
  (loop for index downfrom (1- (min end (length vector))) to start
        when (funcall predicate (funcall key (aref vector index)))
          return index))

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
(defun position-if-from-start-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (position-if-vector-from-start-identity
        predicate sequence start (length sequence)))
    (list
       (position-if-list-from-start-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from start, no end, key
(defun position-if-from-start-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (position-if-vector-from-start-key
        predicate sequence start (length sequence) key))
    (list
       (position-if-list-from-start-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from start, end, no key
(defun position-if-from-start-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (position-if-vector-from-start-identity
        predicate sequence start end))
    (list
       (position-if-list-from-start-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from start, end, key
(defun position-if-from-start-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (position-if-vector-from-start-key
        predicate sequence start end key))
    (list
       (position-if-list-from-start-bounded-key
        predicate sequence start end key))))

;;; Version on any sequence, from end, no end, no key
(defun position-if-from-end-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (position-if-vector-from-end-identity
        predicate sequence start (length sequence)))
    (list
       (position-if-list-from-end-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from end, no end, key
(defun position-if-from-end-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (position-if-vector-from-end-key
        predicate sequence start (length sequence) key))
    (list
       (position-if-list-from-end-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from end, end, no key
(defun position-if-from-end-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (position-if-vector-from-end-identity
        predicate sequence start end))
    (list
       (position-if-list-from-end-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from end, end, key
(defun position-if-from-end-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (position-if-vector-from-end-key
        predicate sequence start end key))
    (list
       (position-if-list-from-end-bounded-key
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
(defun position-if (predicate sequence
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
              (position-if-from-end-bounded-key
               predicate sequence start end key)
              (position-if-from-end-unbounded-key
               predicate sequence start key))
          (if end
              (position-if-from-end-bounded-identity
               predicate sequence start end)
              (position-if-from-end-unbounded-identity
               predicate sequence start)))
      (if key
          (if end
              (position-if-from-start-bounded-key
               predicate sequence start end key)
              (position-if-from-start-unbounded-key
               predicate sequence start key))
          (if end
              (position-if-from-start-bounded-identity
               predicate sequence start end)
              (position-if-from-start-unbounded-identity
               predicate sequence start)))))

(define-compiler-macro position-if (&whole form &rest args)
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
                                `(position-if-from-end-bounded-key
                                  predicate sequence start end key)
                                `(if from-end
                                     (position-if-from-end-bounded-key
                                      predicate sequence start end key)
                                     (position-if-from-start-bounded-key
                                      predicate sequence start end key)))
                            `(position-if-from-start-bounded-key
                              predicate sequence start end key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(position-if-from-end-bounded-identity
                                  predicate sequence start end)
                                `(if from-end
                                     (position-if-from-end-bounded-identity
                                      predicate sequence start end)
                                     (position-if-from-start-bounded-identity
                                      predicate sequence start end)))
                            `(position-if-from-start-bounded-identity
                              predicate sequence start end)))
                    (if (and keyp (not (null key)))
                        (if from-end-p
                            (if (eq from-end t)
                                `(position-if-from-end-unbounded-key
                                  predicate sequence start key)
                                `(if from-end
                                     (position-if-from-end-unbounded-key
                                      predicate sequence start key)
                                     (position-if-from-start-unbounded-key
                                      predicate sequence start key)))
                            `(position-if-from-start-unbounded-key
                              predicate sequence start key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(position-if-from-end-unbounded-identity
                                  predicate sequence start)
                                `(if from-end
                                     (position-if-from-end-unbounded-identity
                                      predicate sequence start)
                                     (position-if-from-start-unbounded-identity
                                      predicate sequence start)))
                            `(position-if-from-start-unbounded-identity
                              predicate sequence start))))))))
    (error () form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function position-if-not

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
;;; as we position an element that satisfies the test.  When 
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
(defun position-if-not-list-from-start-unbounded-identity
    (predicate list start)
  (loop for element in (nthcdr start list)
        unless (funcall predicate element)
          return element))

;;; Version on lists, from start, no end, key
(defun position-if-not-list-from-start-unbounded-key
    (predicate list start key)
  (loop for element in (nthcdr start list)
        unless (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from start, end, no key
(defun position-if-not-list-from-start-bounded-identity
    (predicate list start end)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        unless (funcall predicate element)
          return element))

;;; Version on lists, from start, end, key
(defun position-if-not-list-from-start-bounded-key
    (predicate list start end key)
  (loop for element in (nthcdr start list)
        repeat (- end start)
        unless (funcall predicate (funcall key element))
          return element))

;;; Version on lists, from end, no end, no key
(defun position-if-not-list-from-end-unbounded-identity
    (predicate list start)
  (loop with value = nil
        for element in (nthcdr start list)
        unless (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, no end, key
(defun position-if-not-list-from-end-unbounded-key
    (predicate list start key)
  (loop with value = nil
        for element in (nthcdr start list)
        unless (funcall predicate (funcall key element))
          do (setf value element)
        finally (return value)))
  
;;; Version on lists, from end, end, no key
(defun position-if-not-list-from-end-bounded-identity
    (predicate list start end)
  (loop with value = nil
        for element in (nthcdr start list)
        repeat (- end start)
        unless (funcall predicate element)
          do (setf value element)
        finally (return value)))

;;; Version on lists, from end, end, key
(defun position-if-not-list-from-end-bounded-key
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
(defun position-if-not-vector-from-start-identity
    (predicate vector start end)
  (loop for index from start below (min end (length vector))
        unless (funcall predicate (aref vector index))
          return index))

;;; Version on vectors, from start, key
(defun position-if-not-vector-from-start-key
    (predicate vector start end key)
  (loop for index from start below (min end (length vector))
        unless (funcall predicate (funcall key (aref vector index)))
          return index))

;;; Version on vectors, from end, no key
(defun position-if-not-vector-from-end-identity
    (predicate vector start end)
  (loop for index downfrom (1- (min end (length vector))) to start
        unless (funcall predicate (aref vector index))
          return index))

;;; Version on vectors, from end, key
(defun position-if-not-vector-from-end-key
    (predicate vector start end key)
  (loop for index downfrom (1- (min end (length vector))) to start
        unless (funcall predicate (funcall key (aref vector index)))
          return index))

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
(defun position-if-not-from-start-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-start-identity
        predicate sequence start (length sequence)))
    (list
       (position-if-not-list-from-start-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from start, no end, key
(defun position-if-not-from-start-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-start-key
        predicate sequence start (length sequence) key))
    (list
       (position-if-not-list-from-start-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from start, end, no key
(defun position-if-not-from-start-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-start-identity
        predicate sequence start end))
    (list
       (position-if-not-list-from-start-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from start, end, key
(defun position-if-not-from-start-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-start-key
        predicate sequence start end key))
    (list
       (position-if-not-list-from-start-bounded-key
        predicate sequence start end key))))

;;; Version on any sequence, from end, no end, no key
(defun position-if-not-from-end-unbounded-identity (predicate sequence start)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-end-identity
        predicate sequence start (length sequence)))
    (list
       (position-if-not-list-from-end-unbounded-identity
        predicate sequence start))))

;;; Version on any sequence, from end, no end, key
(defun position-if-not-from-end-unbounded-key (predicate sequence start key)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-end-key
        predicate sequence start (length sequence) key))
    (list
       (position-if-not-list-from-end-unbounded-key
        predicate sequence start key))))

;;; Version on any sequence, from end, end, no key
(defun position-if-not-from-end-bounded-identity (predicate sequence start end)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-end-identity
        predicate sequence start end))
    (list
       (position-if-not-list-from-end-bounded-identity
        predicate sequence start end))))

;;; Version on any sequence, from end, end, key
(defun position-if-not-from-end-bounded-key (predicate sequence start end key)
  (etypecase sequence
    (vector
       (position-if-not-vector-from-end-key
        predicate sequence start end key))
    (list
       (position-if-not-list-from-end-bounded-key
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
(defun position-if-not (predicate sequence
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
              (position-if-not-from-end-bounded-key
               predicate sequence start end key)
              (position-if-not-from-end-unbounded-key
               predicate sequence start key))
          (if end
              (position-if-not-from-end-bounded-identity
               predicate sequence start end)
              (position-if-not-from-end-unbounded-identity
               predicate sequence start)))
      (if key
          (if end
              (position-if-not-from-start-bounded-key
               predicate sequence start end key)
              (position-if-not-from-start-unbounded-key
               predicate sequence start key))
          (if end
              (position-if-not-from-start-bounded-identity
               predicate sequence start end)
              (position-if-not-from-start-unbounded-identity
               predicate sequence start)))))

(define-compiler-macro position-if-not (&whole form &rest args)
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
                                `(position-if-not-from-end-bounded-key
                                  predicate sequence start end key)
                                `(if from-end
                                     (position-if-not-from-end-bounded-key
                                      predicate sequence start end key)
                                     (position-if-not-from-start-bounded-key
                                      predicate sequence start end key)))
                            `(position-if-not-from-start-bounded-key
                              predicate sequence start end key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(position-if-not-from-end-bounded-identity
                                  predicate sequence start end)
                                `(if from-end
                                     (position-if-not-from-end-bounded-identity
                                      predicate sequence start end)
                                     (position-if-not-from-start-bounded-identity
                                      predicate sequence start end)))
                            `(position-if-not-from-start-bounded-identity
                              predicate sequence start end)))
                    (if (and keyp (not (null key)))
                        (if from-end-p
                            (if (eq from-end t)
                                `(position-if-not-from-end-unbounded-key
                                  predicate sequence start key)
                                `(if from-end
                                     (position-if-not-from-end-unbounded-key
                                      predicate sequence start key)
                                     (position-if-not-from-start-unbounded-key
                                      predicate sequence start key)))
                            `(position-if-not-from-start-unbounded-key
                              predicate sequence start key))
                        (if from-end-p
                            (if (eq from-end t)
                                `(position-if-not-from-end-unbounded-identity
                                  predicate sequence start)
                                `(if from-end
                                     (position-if-not-from-end-unbounded-identity
                                      predicate sequence start)
                                     (position-if-not-from-start-unbounded-identity
                                      predicate sequence start)))
                            `(position-if-not-from-start-unbounded-identity
                              predicate sequence start))))))))
    (error () form)))

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
;;; Accessor subseq

(defun subseq (sequence start &optional end)
  (if (listp sequence)
      (let ((list sequence))
	(loop repeat start
	      do (setf list (cdr list)))
	(if end
	    (let* ((end-start (- end start))
		   (result (loop for element in list
				 until (zerop end-start)
				 collect element
				 do (decf end-start))))
	      (if (plusp end-start)
		  (error 'invalid-end-index
			 :datum end
			 :expected-type `(integer 0 ,(- end end-start))
			 :in-sequence sequence)
		  result))
	    (loop for element in list
		  collect element)))
      (progn (when (null end)
	       (setf end (length sequence)))
	     (when (> end (length sequence))
	       (error  'invalid-end-index
		       :datum end
		       :expected-type `(integer 0 ,(length sequence))
		       :in-sequence sequence))
	     (let ((result (make-array (- end start)
				       :element-type (array-element-type sequence))))
	       (loop for i from start below end
		     do (setf (aref result (- i start))
			      (aref sequence i)))
	       result))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function fill

(defun fill-list-unbounded
    (list item start)
  (loop for sublist on (nthcdr start list)
        do (setf (car sublist) item))
  list)

(defun fill-list-bounded
    (list item start end)
  (loop for sublist on (nthcdr start list)
        repeat (- end start)
        do (setf (car sublist) item))
  list)

(defun fill-vector
    (vector item start end)
  (loop for index from start below end
        do (setf (aref vector index) item))
  vector)

(defun fill (sequence item
             &key
             (start 0)
             end)
  (etypecase sequence
    (vector
       (if end
           (fill-vector sequence item start end)
           (fill-vector sequence item start (length sequence))))
    (list
       (if end
           (fill-list-bounded sequence item start end)
           (fill-list-unbounded sequence item start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function remove

;;; It is not worth the effort to specialize for a start value of 0
;;; because that only implies a single test at the beginning of the
;;; function, but it is worth specializing for an end value of nil
;;; when the sequence is a list.

;;; For lists, the technique is to allocate a single sentinel cons
;;; cell that acts as a queue.  Then we fill up the end of the queue
;;; with elements of the list that should be kept.  Finally we return
;;; the cdr of the initial cons cell.  This technique avoids some
;;; special cases at the cost of allocating another cons cell. 

(defun |remove seq-type=list test=eql end=nil count=nil key=identity|
    (item list start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (eql item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=eq end=nil count=nil key=identity|
    (item list start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (eq item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=eql end=nil count=nil key=other|
    (item list start key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (eql item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=eq end=nil count=nil key=other|
    (item list start key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (eq item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=eql end=other count=nil key=identity|
    (item list start end)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eql item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=eq end=other count=nil key=identity|
    (item list start end)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eq item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=eql end=other count=nil key=other|
    (item list start end key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eql item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=eq end=other count=nil key=other|
    (item list start end key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eq item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=other end=nil count=nil key=identity|
    (item list test start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (funcall test item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=other end=nil count=nil key=other|
    (item list test start key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (funcall test item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=other end=other count=nil key=identity|
    (item list test start end)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test=other end=other count=nil key=other|
    (item list test start end key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test-not=other end=nil count=nil key=identity|
    (item list test-not start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  when (funcall test-not item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test-not=other end=nil count=nil key=other|
    (item list test-not start key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  when (funcall test-not item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test-not=other end=other count=nil key=identity|
    (item list test-not start end)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list test-not=other end=other count=nil key=other|
    (item list test-not start end key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eql end=nil count=other key=identity|
    (item list start count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (eql item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eq end=nil count=other key=identity|
    (item list start count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (eq item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eql end=nil count=other key=other|
    (item list start count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (eql item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eq end=nil count=other key=other|
    (item list start count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (eq item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eql end=other count=other key=identity|
    (item list start end count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eql item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eq end=other count=other key=identity|
    (item list start end count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eq item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eql end=other count=other key=other|
    (item list start end count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eql item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=eq end=other count=other key=other|
    (item list start end count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (eq item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=other end=nil count=other key=identity|
    (item list test count start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (funcall test item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=other end=nil count=other key=other|
    (item list test start count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (funcall test item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=other end=other count=other key=identity|
    (item list test start end count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test=other end=other count=other key=other|
    (item list test start end count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test-not=other end=nil count=other key=identity|
    (item list test-not start count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  when (funcall test-not item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test-not=other end=nil count=other key=other|
    (item list test-not start count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  when (funcall test-not item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test-not=other end=other count=other key=identity|
    (item list test-not start end count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not item element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=nil test-not=other end=other count=other key=other|
    (item list test-not start end count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not item (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove seq-type=list from-end=t test=eql end=nil count=other key=identity|
    (item list start count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eql (car reversed-prefix) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=eq end=nil count=other key=identity|
    (item list start count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eq (car reversed-prefix) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=eql end=nil count=other key=other|
    (item list start count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eql (funcall key (car reversed-prefix)) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=eq end=nil count=other key=other|
    (item list start count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eq (funcall key (car reversed-prefix)) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=eql end=other count=other key=identity|
    (item list start end count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eql (car reversed-prefix) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=eq end=other count=other key=identity|
    (item list start end count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eq (car reversed-prefix) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=eql end=other count=other key=other|
    (item list start end count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eql (funcall key (car reversed-prefix)) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=eq end=other count=other key=other|
    (item list start end count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (eq (funcall key (car reversed-prefix)) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=other end=nil count=other key=identity|
    (item list test start count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test item (car reversed-prefix))
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=other end=nil count=other key=other|
    (item list test start count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test (funcall key (car reversed-prefix)) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=other end=other count=other key=identity|
    (item list test start end count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test item (car reversed-prefix))
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test=other end=other count=other key=other|
    (item list test start end count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test (funcall key (car reversed-prefix)) item)
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test-not=other end=nil count=other key=identity|
    (item list test-not start count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (car reversed-prefix) item)
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test-not=other end=nil count=other key=other|
    (item list test-not start count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (funcall key (car reversed-prefix)) item)
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test-not=other end=other count=other key=identity|
    (item list test-not start end count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (car reversed-prefix) item)
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

(defun |remove seq-type=list from-end=t test-not=other end=other count=other key=other|
    (item list test-not start end count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (funcall key (car reversed-prefix)) item)
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

;;; Helper function. 
;;; FIXME: try to explain what it does!
(defun copy-result-general (original-vector start end bit-vector count)
  (declare (type simple-bit-vector bit-vector)
	   (type fixnum start end count))
  (if (= count (- end start))
      original-vector
      (let* ((length (length original-vector))
	     (result (make-array (+ start (- length end) count)
				 :element-type (array-element-type original-vector))))
	;; Copy the prefix
	(loop for i from 0 below start
	      do (setf (svref result i) (aref original-vector i)))
	;; Copy elements marked by the bitmap
	(loop with result-index = start
	      for source-index from start below end
	      when (= 1 (bit bit-vector (- source-index start)))
		do (setf (svref result result-index)
			 (aref original-vector source-index))
		   (incf result-index))
	;; Copy the suffix
	(loop for source-index from end below length
	      for result-index from (+ start count)
	      do (setf (svref result result-index)
		       (aref original-vector source-index)))
	result)))

(defun |remove seq-type=general-vector test=eql count=nil key=identity|
    (item vector start end)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eql item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector test=eq count=nil key=identity|
    (item vector start end)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eq item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector test=eql count=nil key=other|
    (item vector start end key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eql item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector test=eq count=nil key=other|
    (item vector start end key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eq item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector test=other count=nil key=identity|
    (item vector test start end)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector test=other count=nil key=other|
    (item vector test start end key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector test-not=other count=nil key=identity|
    (item vector test-not start end)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector test-not=other count=nil key=other|
    (item vector test-not start end key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
    (item vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eql item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test=eq count=other key=identity|
    (item vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eq item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test=eql count=other key=other|
    (item vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eql item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test=eq count=other key=other|
    (item vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eq item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test=other count=other key=identity|
    (item vector test start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test=other count=other key=other|
    (item vector test start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test-not=other count=other key=identity|
    (item vector test-not start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=nil test-not=other count=other key=other|
    (item vector test-not start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test=eql count=other key=identity|
    (item vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eql item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test=eq count=other key=identity|
    (item vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eq item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test=eql count=other key=other|
    (item vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eql item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test=eq count=other key=other|
    (item vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eq item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test=other count=other key=identity|
    (item vector test start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test=other count=other key=other|
    (item vector test start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test-not=other count=other key=identity|
    (item vector test-not start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not item (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove seq-type=general-vector from-end=t test-not=other count=other key=other|
    (item vector test-not start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not item (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

;;; For vectors, the technique used is to allocate a bitvector that
;;; has the length of the interval in which elements should be
;;; removed, i.e. end - start.  Elements to keep are then marked with
;;; a 1 in that bitvector, and at the same time, we count the number
;;; of 1s.  Finally, we allocate a vector of the correct size, copy
;;; the prefix of the original vector (before start), the elements of
;;; the original vector marked by a 1 in the bitvector in the interval
;;; between start and end, and the suffix of the original vector
;;; (after end).  This technique has the disadvantage that elements of
;;; the original vector in the interval between start and end have to
;;; be accessed twice; once in order to apply the test to see whether
;;; to mark them in the bitvector, and once more to move them from the
;;; original vector to the result vector.  And of course, the
;;; bitvector has to be manipulated as well.  For very quick
;;; combinations of tests and keys, for instance eq and identity, it
;;; may be faster to apply the test twice; once by going through the
;;; original vector and just counting the number of elements to keep,
;;; and then once more in order to move from the original to the
;;; resulting vector.  That method would save the bitvector
;;; manipulation, but it would access *all* of the elements in the the
;;; interval between start and end twice, not only those that are to
;;; be kept.

;;; Helper function. 
;;; FIXME: try to explain what it does!
(defun copy-result-simple (original-vector start end bit-vector count)
  (declare (type simple-vector original-vector)
	   (type simple-bit-vector bit-vector)
	   (type fixnum start end count))
  (if (= count (- end start))
      original-vector
      (let* ((length (length original-vector))
	     (result (make-array (+ start (- length end) count)
				 :element-type (array-element-type original-vector))))
	;; Copy the prefix
	(loop for i from 0 below start
	      do (setf (svref result i) (svref original-vector i)))
	;; Copy elements marked by the bitmap
	(loop with result-index = start
	      for source-index from start below end
	      when (= 1 (bit bit-vector (- source-index start)))
		do (setf (svref result result-index)
			 (svref original-vector source-index))
		   (incf result-index))
	;; Copy the suffix
	(loop for source-index from end below length
	      for result-index from (+ start count)
	      do (setf (svref result result-index)
		       (svref original-vector source-index)))
	result)))

(defun |remove seq-type=simple-vector test=eql count=nil key=identity|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eql item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector test=eq count=nil key=identity|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eq item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector test=eql count=nil key=other|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eql item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector test=eq count=nil key=other|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eq item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector test=other count=nil key=identity|
    (item vector test start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector test=other count=nil key=other|
    (item vector test start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector test-not=other count=nil key=identity|
    (item vector test-not start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector test-not=other count=nil key=other|
    (item vector test-not start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
    (item vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eql item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test=eq count=other key=identity|
    (item vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eq item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
    (item vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eql item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test=eq count=other key=other|
    (item vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eq item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test=other count=other key=identity|
    (item vector test start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test=other count=other key=other|
    (item vector test start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test-not=other count=other key=identity|
    (item vector test-not start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=nil test-not=other count=other key=other|
    (item vector test-not start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
    (item vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eql item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test=eq count=other key=identity|
    (item vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eq item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test=eql count=other key=other|
    (item vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eql item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test=eq count=other key=other|
    (item vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eq item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test=other count=other key=identity|
    (item vector test start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test=other count=other key=other|
    (item vector test start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test-not=other count=other key=identity|
    (item vector test-not start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not item (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-vector from-end=t test-not=other count=other key=other|
    (item vector test-not start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not item (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

;;; Helper function. 
;;; FIXME: try to explain what it does!
(defun copy-result-simple-string (original-vector start end bit-vector count)
  (declare (type simple-string original-vector)
	   (type simple-bit-vector bit-vector)
	   (type fixnum start end count))
  (if (= count (- end start))
      original-vector
      (let* ((length (length original-vector))
	     (result (make-array (+ start (- length end) count)
				 :element-type (array-element-type original-vector))))
	;; Copy the prefix
	(loop for i from 0 below start
	      do (setf (svref result i) (schar original-vector i)))
	;; Copy elements marked by the bitmap
	(loop with result-index = start
	      for source-index from start below end
	      when (= 1 (bit bit-vector (- source-index start)))
		do (setf (svref result result-index)
			 (schar original-vector source-index))
		   (incf result-index))
	;; Copy the suffix
	(loop for source-index from end below length
	      for result-index from (+ start count)
	      do (setf (svref result result-index)
		       (schar original-vector source-index)))
	result)))

(defun |remove seq-type=simple-string test=eql count=nil key=identity|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eql item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string test=eq count=nil key=identity|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eq item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string test=eql count=nil key=other|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eql item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string test=eq count=nil key=other|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (eq item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string test=other count=nil key=identity|
    (item vector test start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string test=other count=nil key=other|
    (item vector test start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string test-not=other count=nil key=identity|
    (item vector test-not start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string test-not=other count=nil key=other|
    (item vector test-not start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
    (item vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eql item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test=eq count=other key=identity|
    (item vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eq item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test=eql count=other key=other|
    (item vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eql item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test=eq count=other key=other|
    (item vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (eq item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test=other count=other key=identity|
    (item vector test start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test=other count=other key=other|
    (item vector test start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test-not=other count=other key=identity|
    (item vector test-not start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=nil test-not=other count=other key=other|
    (item vector test-not start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test=eql count=other key=identity|
    (item vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eql item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test=eq count=other key=identity|
    (item vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eq item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test=eql count=other key=other|
    (item vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eql item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test=eq count=other key=other|
    (item vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (eq item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test=other count=other key=identity|
    (item vector test start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test=other count=other key=other|
    (item vector test start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test-not=other count=other key=identity|
    (item vector test-not start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not item (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove seq-type=simple-string from-end=t test-not=other count=other key=other|
    (item vector test-not start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not item (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun remove (item sequence &key from-end test test-not (start 0) end count key)
  (assert (or (null test) (null test-not)))
  ;; FIXME test if it is a sequence at all.
  (if (listp sequence)
      ;; seq-type=list
      (if from-end
	  ;; seq-type=list from-end=t
	  (if test
	      ;; seq-type=list from-end=t test=?
	      (if (or (eq test 'eq) (eq test #'eq))
		  ;; seq-type=list from-end=t test=eq
		  (if end
		      ;; seq-type=list from-end=t test=eq end=other
		      (if count
			  ;; seq-type=list from-end=t test=eq end=other count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eq end=other count=other key=other
			      (|remove seq-type=list from-end=t test=eq end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=t test=eq end=other count=other key=identity
			      (|remove seq-type=list from-end=t test=eq end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=t test=eq end=other count=nil
			  (if key
			      ;;       seq-type=list test=eq end=other count=nil key=other
			      (|remove seq-type=list test=eq end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eq end=other count=nil key=identity
			      (|remove seq-type=list test=eq end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=t test=eq end=nil
		      (if count
			  ;; seq-type=list from-end=t test=eq end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eq end=nil count=other key=other
			      (|remove seq-type=list from-end=t test=eq end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=t test=eq end=nil count=other key=identity
			      (|remove seq-type=list from-end=t test=eq end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=t test=eq end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eq end=nil count=nil key=other
			      (|remove seq-type=list test=eq end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eq end=nil count=nil key=identity
			      (|remove seq-type=list test=eq end=nil count=nil key=identity|
			       item sequence start))))
		  (if (or (eq test 'eql) (eq test #'eql))
		      ;; seq-type=list from-end=t test=eql
		      (if end
			  ;; seq-type=list from-end=t test=eql end=other
			  (if count
			      ;; seq-type=list from-end=t test=eql end=other count=other
			      (if key
				  ;;       seq-type=list from-end=t test=eql end=other count=other key=other
				  (|remove seq-type=list from-end=t test=eql end=other count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=list from-end=t test=eql end=other count=other key=identity
				  (|remove seq-type=list from-end=t test=eql end=other count=other key=identity|
				   item sequence start end count))
			      ;; seq-type=list from-end=t test=eql end=other count=nil
			      (if key
				  ;;       seq-type=list test=eql end=other count=nil key=other
				  (|remove seq-type=list test=eql end=other count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=list test=eql end=other count=nil key=identity
				  (|remove seq-type=list test=eql end=other count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=list from-end=t test=eql end=nil
			  (if count
			      ;; seq-type=list from-end=t test=eql end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=t test=eql end=nil count=other key=other
				  (|remove seq-type=list from-end=t test=eql end=nil count=other key=other|
				   item sequence start count key)
				  ;;       seq-type=list from-end=t test=eql end=nil count=other key=identity
				  (|remove seq-type=list from-end=t test=eql end=nil count=other key=identity|
				   item sequence start count))
			      ;; seq-type=list from-end=t test=eql end=nil count=nil
			      (if key
				  ;;       seq-type=list test=eql end=nil count=nil key=other
				  (|remove seq-type=list test=eql end=nil count=nil key=other|
				   item sequence start key)
				  ;;       seq-type=list test=eql end=nil count=nil key=identity
				  (|remove seq-type=list test=eql end=nil count=nil key=identity|
				   item sequence start))))
		      ;; seq-type=list from-end=t test=other
		      (if end
			  ;; seq-type=list from-end=t test=other end=other
			  (if count
			      ;; seq-type=list from-end=t test=other end=other count=other
			      (if key
				  ;;       seq-type=list from-end=t test=other end=other count=other key=other
				  (|remove seq-type=list from-end=t test=other end=other count=other key=other|
				   item sequence test start end count key)
				  ;;       seq-type=list from-end=t test=other end=other count=other key=identity
				  (|remove seq-type=list from-end=t test=other end=other count=other key=identity|
				   item sequence test start end count))
			      ;; seq-type=list from-end=t test=other end=other count=nil
			      (if key
				  ;;       seq-type=list test=other end=other count=nil key=other
				  (|remove seq-type=list test=other end=other count=nil key=other|
				   item sequence test start end key)
				  ;;       seq-type=list test=other end=other count=nil key=identity
				  (|remove seq-type=list test=other end=other count=nil key=identity|
				   item sequence test start end)))
			  ;; seq-type=list from-end=t test=other end=nil
			  (if count
			      ;; seq-type=list from-end=t test=other end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=t test=other end=nil count=other key=other
				  (|remove seq-type=list from-end=t test=other end=nil count=other key=other|
				   item sequence test start count key)
				  ;;       seq-type=list from-end=t test=other end=nil count=other key=identity
				  (|remove seq-type=list from-end=t test=other end=nil count=other key=identity|
				   item sequence test start count))
			      ;; seq-type=list from-end=t test=other end=nil count=nil
			      (if key
				  ;;       seq-type=list test=other end=nil count=nil key=other
				  (|remove seq-type=list test=other end=nil count=nil key=other|
				   item sequence test start key)
				  ;;       seq-type=list test=other end=nil count=nil key=identity
				  (|remove seq-type=list test=other end=nil count=nil key=identity|
				   item sequence test start))))))
	      (if test-not
		  ;; seq-type=list from-end=t test-not=other
		  (if end
		      ;; seq-type=list from-end=t test-not=other end=other
		      (if count
			  ;; seq-type=list from-end=t test-not=other end=other count=other
			  (if key
			      ;;       seq-type=list from-end=t test-not=other end=other count=other key=other
			      (|remove seq-type=list from-end=t test-not=other end=other count=other key=other|
			       item sequence test-not start end count key)
			      ;;       seq-type=list from-end=t test-not=other end=other count=other key=identity
			      (|remove seq-type=list from-end=t test-not=other end=other count=other key=identity|
			       item sequence test-not start end count))
			  ;; seq-type=list from-end=t test-not=other end=other count=nil
			  (if key
			      ;;       seq-type=list test-not=other end=other count=nil key=other
			      (|remove seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      ;;       seq-type=list test-not=other end=other count=nil key=identity
			      (|remove seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end)))
		      ;; seq-type=list from-end=t test-not=other end=nil
		      (if count
			  ;; seq-type=list from-end=t test-not=other end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=t test-not=other end=nil count=other key=other
			      (|remove seq-type=list from-end=t test-not=other end=nil count=other key=other|
			       item sequence test-not start count key)
			      ;;       seq-type=list from-end=t test-not=other end=nil count=other key=identity
			      (|remove seq-type=list from-end=t test-not=other end=nil count=other key=identity|
			       item sequence test-not start count))
			  (if key
			      (|remove seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      (|remove seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end))))
		  ;; seq-type=list from-end=t test=eql
		  (if end
		      ;; seq-type=list from-end=t test=eql end=other
		      (if count
			  ;; seq-type=list from-end=t test=eql end=other count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eql end=other count=other key=other
			      (|remove seq-type=list from-end=t test=eql end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=t test=eql end=other count=other key=identity
			      (|remove seq-type=list from-end=t test=eql end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=t test=eql end=other count=nil
			  (if key
			      ;;       seq-type=list test=eql end=other count=nil key=other
			      (|remove seq-type=list test=eql end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eql end=other count=nil key=identity
			      (|remove seq-type=list test=eql end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=t test=eql end=nil
		      (if count
			  ;; seq-type=list from-end=t test=eql end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eql end=nil count=other key=other
			      (|remove seq-type=list from-end=t test=eql end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=t test=eql end=nil count=other key=identity
			      (|remove seq-type=list from-end=t test=eql end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=t test=eql end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eql end=nil count=nil key=other
			      (|remove seq-type=list test=eql end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eql end=nil count=nil key=identity
			      (|remove seq-type=list test=eql end=nil count=nil key=identity|
			       item sequence start))))))
	  ;; seq-type=list from-end=nil
	  (if test
	      ;; seq-type=list from-end=nil test=?
	      (if (or (eq test 'eq) (eq test #'eq))
		  ;; seq-type=list from-end=nil test=eq
		  (if end
		      ;; seq-type=list from-end=nil test=eq end=other
		      (if count
			  ;; seq-type=list from-end=nil test=eq end=other count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eq end=other count=other key=other
			      (|remove seq-type=list from-end=nil test=eq end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=nil test=eq end=other count=other key=identity
			      (|remove seq-type=list from-end=nil test=eq end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=nil test=eq end=other count=nil
			  (if key
			      ;;       seq-type=list test=eq end=other count=nil key=other
			      (|remove seq-type=list test=eq end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eq end=other count=nil key=identity
			      (|remove seq-type=list test=eq end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=nil test=eq end=nil
		      (if count
			  ;; seq-type=list from-end=nil test=eq end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eq end=nil count=other key=other
			      (|remove seq-type=list from-end=nil test=eq end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=nil test=eq end=nil count=other key=identity
			      (|remove seq-type=list from-end=nil test=eq end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=nil test=eq end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eq end=nil count=nil key=other
			      (|remove seq-type=list test=eq end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eq end=nil count=nil key=identity
			      (|remove seq-type=list test=eq end=nil count=nil key=identity|
			       item sequence start))))
		  (if (or (eq test 'eql) (eq test #'eql))
		      ;; seq-type=list from-end=nil test=eql
		      (if end
			  ;; seq-type=list from-end=nil test=eql end=other
			  (if count
			      ;; seq-type=list from-end=nil test=eql end=other count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=eql end=other count=other key=other
				  (|remove seq-type=list from-end=nil test=eql end=other count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=list from-end=nil test=eql end=other count=other key=identity
				  (|remove seq-type=list from-end=nil test=eql end=other count=other key=identity|
				   item sequence start end count))
			      ;; seq-type=list from-end=nil test=eql end=other count=nil
			      (if key
				  ;;       seq-type=list test=eql end=other count=nil key=other
				  (|remove seq-type=list test=eql end=other count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=list test=eql end=other count=nil key=identity
				  (|remove seq-type=list test=eql end=other count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=list from-end=nil test=eql end=nil
			  (if count
			      ;; seq-type=list from-end=nil test=eql end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=eql end=nil count=other key=other
				  (|remove seq-type=list from-end=nil test=eql end=nil count=other key=other|
				   item sequence start count key)
				  ;;       seq-type=list from-end=nil test=eql end=nil count=other key=identity
				  (|remove seq-type=list from-end=nil test=eql end=nil count=other key=identity|
				   item sequence start count))
			      ;; seq-type=list from-end=nil test=eql end=nil count=nil
			      (if key
				  ;;       seq-type=list test=eql end=nil count=nil key=other
				  (|remove seq-type=list test=eql end=nil count=nil key=other|
				   item sequence start key)
				  (|remove seq-type=list test=eql end=nil count=nil key=identity|
				   item sequence start))))
		      ;; seq-type=list from-end=nil test=other
		      (if end
			  ;; seq-type=list from-end=nil test=other end=other
			  (if count
			      ;; seq-type=list from-end=nil test=other end=other count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=other end=other count=other key=other
				  (|remove seq-type=list from-end=nil test=other end=other count=other key=other|
				   item sequence test start end count key)
				  ;;       seq-type=list from-end=nil test=other end=other count=other key=identity
				  (|remove seq-type=list from-end=nil test=other end=other count=other key=identity|
				   item sequence test start end count))
			      ;; seq-type=list from-end=nil test=other end=other count=nil
			      (if key
				  ;;       seq-type=list test=other end=other count=nil key=other
				  (|remove seq-type=list test=other end=other count=nil key=other|
				   item sequence test start end key)
				  ;;       seq-type=list test=other end=other count=nil key=identity
				  (|remove seq-type=list test=other end=other count=nil key=identity|
				   item sequence test start end)))
			  ;; seq-type=list from-end=nil test=other end=nil
			  (if count
			      ;; seq-type=list from-end=nil test=other end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=other end=nil count=other key=other
				  (|remove seq-type=list from-end=nil test=other end=nil count=other key=other|
				   item sequence test start count key)
				  ;;       seq-type=list from-end=nil test=other end=nil count=other key=identity
				  (|remove seq-type=list from-end=nil test=other end=nil count=other key=identity|
				   item sequence test start count))
			      ;; seq-type=list from-end=nil test=other end=nil count=nil
			      (if key
				  ;;       seq-type=list test=other end=nil count=nil key=other
				  (|remove seq-type=list test=other end=nil count=nil key=other|
				   item sequence test start key)
				  (|remove seq-type=list test=other end=nil count=nil key=identity|
				   item sequence test start))))))
	      (if test-not
		  ;; seq-type=list from-end=nil test-not=other
		  (if end
		      ;; seq-type=list from-end=nil test-not=other end=other
		      (if count
			  ;; seq-type=list from-end=nil test-not=other end=other count=other
			  (if key
			      ;;       seq-type=list from-end=nil test-not=other end=other count=other key=other
			      (|remove seq-type=list from-end=nil test-not=other end=other count=other key=other|
			       item sequence test-not start end count key)
			      ;;       seq-type=list from-end=nil test-not=other end=other count=other key=identity
			      (|remove seq-type=list from-end=nil test-not=other end=other count=other key=identity|
			       item sequence test-not start end count))
			  ;; seq-type=list from-end=nil test-not=other end=other count=nil
			  (if key
			      ;;       seq-type=list test-not=other end=other count=nil key=other
			      (|remove seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      ;;       seq-type=list test-not=other end=other count=nil key=identity
			      (|remove seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end)))
		      ;; seq-type=list from-end=nil test-not=other end=nil
		      (if count
			  ;; seq-type=list from-end=nil test-not=other end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=nil test-not=other end=nil count=other key=other
			      (|remove seq-type=list from-end=nil test-not=other end=nil count=other key=other|
			       item sequence test-not start count key)
			      ;;       seq-type=list from-end=nil test-not=other end=nil count=other key=identity
			      (|remove seq-type=list from-end=nil test-not=other end=nil count=other key=identity|
			       item sequence test-not start count))
			  (if key
			      (|remove seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      (|remove seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end))))
		  ;; seq-type=list from-end=nil test=eql
		  (if end
		      ;; seq-type=list from-end=nil test=eql end=other
		      (if count
			  ;; seq-type=list from-end=nil test=eql end=other count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eql end=other count=other key=other
			      (|remove seq-type=list from-end=nil test=eql end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=nil test=eql end=other count=other key=identity
			      (|remove seq-type=list from-end=nil test=eql end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=nil test=eql end=other count=nil
			  (if key
			      ;;       seq-type=list test=eql end=other count=nil key=other
			      (|remove seq-type=list test=eql end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eql end=other count=nil key=identity
			      (|remove seq-type=list test=eql end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=nil test=eql end=nil
		      (if count
			  ;; seq-type=list from-end=nil test=eql end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eql end=nil count=other key=other
			      (|remove seq-type=list from-end=nil test=eql end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=nil test=eql end=nil count=other key=identity
			      (|remove seq-type=list from-end=nil test=eql end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=nil test=eql end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eql end=nil count=nil key=other
			      (|remove seq-type=list test=eql end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eql end=nil count=nil key=identity
			      (|remove seq-type=list test=eql end=nil count=nil key=identity|
			       item sequence start)))))))
      (if (simple-string-p sequence)
	  ;; seq-type=simple-string
	  (if test
	      ;; seq-type=simple-string test=given
	      (if (or (eq test 'eq) (eq test #'eq))
		  ;; seq-type=simple-string test=eq
		  (if end
		      ;; seq-type=simple-string test=eq end=other
		      (if count
			  ;; seq-type=simple-string test=eq count=other end=other
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eq count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=other end=other 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=identity|
				   item sequence start end count))
			      ;; seq-type=simple-string from-end=nil test=eq count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=other end=other 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=identity|
				   item sequence start end count)))
			  ;; seq-type=simple-string test=eq count=nil end=other
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eq count=nil key=other end=other 
			      (|remove seq-type=simple-string test=eq count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=simple-string test=eq count=nil key=identity end=other 
			      (|remove seq-type=simple-string test=eq count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=simple-string test=eq end=nil
		      (if count
			  ;; seq-type=simple-string test=eq count=other end=nil
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eq count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=other end=nil 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=identity|
				   item sequence start (length sequence) count))
			      ;; seq-type=simple-string from-end=nil test=eq count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=other end=nil 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=identity|
				   item sequence start (length sequence) count)))
			  ;; seq-type=simple-string test=eq count=nil end=nil
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eq count=nil key=other end=nil 
			      (|remove seq-type=simple-string test=eq count=nil key=other|
			       item sequence start (length sequence) key)
			      ;;       seq-type=simple-string test=eq count=nil key=identity end=nil 
			      (|remove seq-type=simple-string test=eq count=nil key=identity|
			       item sequence start (length sequence)))))
		  (if (or (eq test 'eql) (eq test #'eql))
		      ;; seq-type=simple-string test=eql
		      (if end
			  ;; seq-type=simple-string test=eql end=other
			  (if count
			      ;; seq-type=simple-string test=eql count=other end=other
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				       item sequence start end count))
				  ;; seq-type=simple-string from-end=nil test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=simple-string test=eql count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=eql count=nil key=other end=other 
				  (|remove seq-type=simple-string test=eql count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=simple-string test=eql count=nil key=identity end=other 
				  (|remove seq-type=simple-string test=eql count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=simple-string test=eql end=nil
			  (if count
			      ;; seq-type=simple-string test=eql count=other end=nil
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seq-type=simple-string from-end=nil test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=simple-string test=eql count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=eql count=nil key=other end=nil 
				  (|remove seq-type=simple-string test=eql count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=simple-string test=eql count=nil key=identity end=nil 
				  (|remove seq-type=simple-string test=eql count=nil key=identity|
				   item sequence start (length sequence)))))
		      ;; seq-type=simple-string test=other
		      (if end
			  ;; seq-type=simple-string test=other end=other
			  (if count
			      ;; seq-type=simple-string test=other count=other end=other
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=other count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=t test=other count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=other|
				       item sequence test start end count key)
				      ;;       seq-type=simple-string from-end=t test=other count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=identity|
				       item sequence test start end count))
				  ;; seq-type=simple-string from-end=nil test=other count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=other|
				       item sequence test start end count key)
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=identity|
				       item sequence test start end count)))
			      ;; seq-type=simple-string test=other count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=other count=nil key=other end=other 
				  (|remove seq-type=simple-string test=other count=nil key=other|
				   item sequence test start end key)
				  ;;       seq-type=simple-string test=other count=nil key=identity end=other 
				  (|remove seq-type=simple-string test=other count=nil key=identity|
				   item sequence test start end)))
			  ;; seq-type=simple-string test=other end=nil
			  (if count
			      ;; seq-type=simple-string test=other count=other end=nil
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=t test=other count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=other|
				       item sequence test start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=t test=other count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=identity|
				       item sequence test start (length sequence) count))
				  ;; seq-type=simple-string from-end=nil test=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=other|
				       item sequence test start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=identity|
				       item sequence test start (length sequence) count)))
			      ;; seq-type=simple-string test=other count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=other count=nil key=other end=nil 
				  (|remove seq-type=simple-string test=other count=nil key=other|
				   item sequence test start (length sequence) key)
				  ;;       seq-type=simple-string test=other count=nil key=identity end=nil 
				  (|remove seq-type=simple-string test=other count=nil key=identity|
				   item sequence test start (length sequence)))))))
	      (if test-not
		  ;; seq-type=simple-string test-not=other
		  (if end
		      ;; seq-type=simple-string test-not=other end=other
		      (if count
			  ;; seq-type=simple-string test-not=other count=other end=other
			  (if from-end
			      ;; seq-type=simple-string from-end=t test-not=other count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=other end=other
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=other|
				   item sequence test-not start end count key)
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=identity|
				   item sequence test-not start end count))
			      ;; seqr-type=simple-string from-end=nil test-not=other count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=other end=other
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=other|
				   item sequence test-not start end count key)
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=identity|
				   item sequence test-not start end count)))
			  ;; seq-type=simple-string test-not=other count=nil end=other
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test-not=other count=nil key=other end=other
			      (|remove seq-type=simple-string test-not=other count=nil key=other|
			       item sequence test-not start end key)
			      ;;       seq-type=simple-string test-not=other count=nil key=identity end=other 
			      (|remove seq-type=simple-string test-not=other count=nil key=identity|
			       item sequence test-not start end)))
		      ;; seq-type=simple-string test-not=other end=nil
		      (if count
			  ;; seq-type=simple-string test-not=other count=other end=nil
			  (if from-end
			      ;; seq-type=simple-string from-end=t test-not=other count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=other|
				   item sequence test-not start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=identity|
				   item sequence test-not start (length sequence) count))
			      ;; seqr-type=simple-string from-end=nil test-not=other count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=other|
				   item sequence test-not start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=identity|
				   item sequence test-not start (length sequence) count)))
			  ;; seq-type=simple-string test-not=other count=nil end=nil
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test-not=other count=nil key=other end=nil
			      (|remove seq-type=simple-string test-not=other count=nil key=other|
			       item sequence test-not start (length sequence) key)
			      ;;       seq-type=simple-string test-not=other count=nil key=identity end=nil! 
			      (|remove seq-type=simple-string test-not=other count=nil key=identity|
			       item sequence test-not start (length sequence)))))
		  ;; seq-type=simple-string test=eql
		  (if end
		      ;; seq-type=simple-string test=eql end=other
		      (if count
			  ;; seq-type=simple-string test=eql count=other end=other
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eql count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=other
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				   item sequence start end count))
			      ;; seqr-type=simple-string from-end=nil test=eql count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=other
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				   item sequence start end count)))
			  ;; seq-type=simple-string test=eql count=nil end=other
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eql count=nil key=other end=other
			      (|remove seq-type=simple-string test=eql count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=simple-string test=eql count=nil key=identity end=other 
			      (|remove seq-type=simple-string test=eql count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=simple-string test=eql end=nil
		      (if count
			  ;; seq-type=simple-string test=eql count=other end=nil
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eql count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				   item sequence start (length sequence) count))
			      ;; seqr-type=simple-string from-end=nil test=eql count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				   item sequence start (length sequence) count)))
			  ;; seq-type=simple-string test=eql count=nil end=nil
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eql count=nil key=other end=nil
			      (|remove seq-type=simple-string test=eql count=nil key=other|
			       item sequence start (length sequence) key)
			      ;;       seq-type=simple-string test=eql count=nil key=identity end=nil! 
			      (|remove seq-type=simple-string test=eql count=nil key=identity|
			       item sequence start (length sequence)))))))
	  (if (simple-vector-p sequence)
	      ;; seq-type=simple-vector
	      (if test
		  ;; seq-type=simple-vector test=given
		  (if (or (eq test 'eq) (eq test #'eq))
		      ;; seq-type=simple-vector test=eq
		      (if end
			  ;; seq-type=simple-vector test=eq end=other
			  (if count
			      ;; seq-type=simple-vector test=eq count=other end=other
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eq count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=other end=other 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=identity|
				       item sequence start end count))
				  ;; seq-type=simple-vector from-end=nil test=eq count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=other end=other 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=simple-vector test=eq count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eq count=nil key=other end=other 
				  (|remove seq-type=simple-vector test=eq count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=simple-vector test=eq count=nil key=identity end=other 
				  (|remove seq-type=simple-vector test=eq count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=simple-vector test=eq end=nil
			  (if count
			      ;; seq-type=simple-vector test=eq count=other end=nil
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eq count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=other end=nil 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seq-type=simple-vector from-end=nil test=eq count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=other end=nil 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=simple-vector test=eq count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eq count=nil key=other end=nil 
				  (|remove seq-type=simple-vector test=eq count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=simple-vector test=eq count=nil key=identity end=nil 
				  (|remove seq-type=simple-vector test=eq count=nil key=identity|
				   item sequence start (length sequence)))))
		      (if (or (eq test 'eql) (eq test #'eql))
			  ;; seq-type=simple-vector test=eql
			  (if end
			      ;; seq-type=simple-vector test=eql end=other
			      (if count
				  ;; seq-type=simple-vector test=eql count=other end=other
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=eql count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
					   item sequence start end count))
				      ;; seq-type=simple-vector from-end=nil test=eql count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
					   item sequence start end count)))
				  ;; seq-type=simple-vector test=eql count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=eql count=nil key=other end=other 
				      (|remove seq-type=simple-vector test=eql count=nil key=other|
				       item sequence start end key)
				      ;;       seq-type=simple-vector test=eql count=nil key=identity end=other 
				      (|remove seq-type=simple-vector test=eql count=nil key=identity|
				       item sequence start end)))
			      ;; seq-type=simple-vector test=eql end=nil
			      (if count
				  ;; seq-type=simple-vector test=eql count=other end=nil
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=eql count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
					   item sequence start (length sequence) count))
				      ;; seq-type=simple-vector from-end=nil test=eql count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
					   item sequence start (length sequence) count)))
				  ;; seq-type=simple-vector test=eql count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=eql count=nil key=other end=nil 
				      (|remove seq-type=simple-vector test=eql count=nil key=other|
				       item sequence start (length sequence) key)
				      ;;       seq-type=simple-vector test=eql count=nil key=identity end=nil 
				      (|remove seq-type=simple-vector test=eql count=nil key=identity|
				       item sequence start (length sequence)))))
			  ;; seq-type=simple-vector test=other
			  (if end
			      ;; seq-type=simple-vector test=other end=other
			      (if count
				  ;; seq-type=simple-vector test=other count=other end=other
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=other count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=identity|
					   item sequence test start end count))
				      ;; seq-type=simple-vector from-end=nil test=other count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=identity|
					   item sequence test start end count)))
				  ;; seq-type=simple-vector test=other count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=other count=nil key=other end=other 
				      (|remove seq-type=simple-vector test=other count=nil key=other|
				       item sequence test start end key)
				      ;;       seq-type=simple-vector test=other count=nil key=identity end=other 
				      (|remove seq-type=simple-vector test=other count=nil key=identity|
				       item sequence test start end)))
			      ;; seq-type=simple-vector test=other end=nil
			      (if count
				  ;; seq-type=simple-vector test=other count=other end=nil
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=other count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=identity|
					   item sequence test start (length sequence) count))
				      ;; seq-type=simple-vector from-end=nil test=other count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=identity|
					   item sequence test start (length sequence) count)))
				  ;; seq-type=simple-vector test=other count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=other count=nil key=other end=nil 
				      (|remove seq-type=simple-vector test=other count=nil key=other|
				       item sequence test start (length sequence) key)
				      ;;       seq-type=simple-vector test=other count=nil key=identity end=nil 
				      (|remove seq-type=simple-vector test=other count=nil key=identity|
				       item sequence test start (length sequence)))))))
		  (if test-not
		      ;; seq-type=simple-vector test-not=other
		      (if end
			  ;; seq-type=simple-vector test-not=other end=other
			  (if count
			      ;; seq-type=simple-vector test-not=other count=other end=other
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test-not=other count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start end count))
				  ;; seqr-type=simple-vector from-end=nil test-not=other count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start end count)))
			      ;; seq-type=simple-vector test-not=other count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test-not=other count=nil key=other end=other
				  (|remove seq-type=simple-vector test-not=other count=nil key=other|
				   item sequence test-not start end key)
				  ;;       seq-type=simple-vector test-not=other count=nil key=identity end=other 
				  (|remove seq-type=simple-vector test-not=other count=nil key=identity|
				   item sequence test-not start end)))
			  ;; seq-type=simple-vector test-not=other end=nil
			  (if count
			      ;; seq-type=simple-vector test-not=other count=other end=nil
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count))
				  ;; seqr-type=simple-vector from-end=nil test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count)))
			      ;; seq-type=simple-vector test-not=other count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test-not=other count=nil key=other end=nil
				  (|remove seq-type=simple-vector test-not=other count=nil key=other|
				   item sequence test-not start (length sequence) key)
				  ;;       seq-type=simple-vector test-not=other count=nil key=identity end=nil! 
				  (|remove seq-type=simple-vector test-not=other count=nil key=identity|
				   item sequence test-not start (length sequence)))))
		      ;; seq-type=simple-vector test=eql
		      (if end
			  ;; seq-type=simple-vector test=eql end=other
			  (if count
			      ;; seq-type=simple-vector test=eql count=other end=other
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
				       item sequence start end count))
				  ;; seqr-type=simple-vector from-end=nil test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=simple-vector test=eql count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eql count=nil key=other end=other
				  (|remove seq-type=simple-vector test=eql count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=simple-vector test=eql count=nil key=identity end=other 
				  (|remove seq-type=simple-vector test=eql count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=simple-vector test=eql end=nil
			  (if count
			      ;; seq-type=simple-vector test=eql count=other end=nil
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seqr-type=simple-vector from-end=nil test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=simple-vector test=eql count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eql count=nil key=other end=nil
				  (|remove seq-type=simple-vector test=eql count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=simple-vector test=eql count=nil key=identity end=nil! 
				  (|remove seq-type=simple-vector test=eql count=nil key=identity|
				   item sequence start (length sequence)))))))
	      ;; seq-type=general-vector
	      (if test
		  ;; seq-type=general-vector test=given
		  (if (or (eq test 'eq) (eq test #'eq))
		      ;; seq-type=general-vector test=eq
		      (if end
			  ;; seq-type=general-vector test=eq end=other
			  (if count
			      ;; seq-type=general-vector test=eq count=other end=other
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eq count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=other end=other 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=identity|
				       item sequence start end count))
				  ;; seq-type=general-vector from-end=nil test=eq count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=other end=other 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=general-vector test=eq count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eq count=nil key=other end=other 
				  (|remove seq-type=general-vector test=eq count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=general-vector test=eq count=nil key=identity end=other 
				  (|remove seq-type=general-vector test=eq count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=general-vector test=eq end=nil
			  (if count
			      ;; seq-type=general-vector test=eq count=other end=nil
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eq count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=other end=nil 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seq-type=general-vector from-end=nil test=eq count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=other end=nil 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=general-vector test=eq count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eq count=nil key=other end=nil 
				  (|remove seq-type=general-vector test=eq count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=general-vector test=eq count=nil key=identity end=nil 
				  (|remove seq-type=general-vector test=eq count=nil key=identity|
				   item sequence start (length sequence)))))
		      (if (or (eq test 'eql) (eq test #'eql))
			  ;; seq-type=general-vector test=eql
			  (if end
			      ;; seq-type=general-vector test=eql end=other
			      (if count
				  ;; seq-type=general-vector test=eql count=other end=other
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=eql count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
					   item sequence start end count))
				      ;; seq-type=general-vector from-end=nil test=eql count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
					   item sequence start end count)))
				  ;; seq-type=general-vector test=eql count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=eql count=nil key=other end=other 
				      (|remove seq-type=general-vector test=eql count=nil key=other|
				       item sequence start end key)
				      ;;       seq-type=general-vector test=eql count=nil key=identity end=other 
				      (|remove seq-type=general-vector test=eql count=nil key=identity|
				       item sequence start end)))
			      ;; seq-type=general-vector test=eql end=nil
			      (if count
				  ;; seq-type=general-vector test=eql count=other end=nil
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=eql count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
					   item sequence start (length sequence) count))
				      ;; seq-type=general-vector from-end=nil test=eql count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
					   item sequence start (length sequence) count)))
				  ;; seq-type=general-vector test=eql count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=eql count=nil key=other end=nil 
				      (|remove seq-type=general-vector test=eql count=nil key=other|
				       item sequence start (length sequence) key)
				      ;;       seq-type=general-vector test=eql count=nil key=identity end=nil 
				      (|remove seq-type=general-vector test=eql count=nil key=identity|
				       item sequence start (length sequence)))))
			  ;; seq-type=general-vector test=other
			  (if end
			      ;; seq-type=general-vector test=other end=other
			      (if count
				  ;; seq-type=general-vector test=other count=other end=other
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=other count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=t test=other count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=general-vector from-end=t test=other count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=identity|
					   item sequence test start end count))
				      ;; seq-type=general-vector from-end=nil test=other count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=identity|
					   item sequence test start end count)))
				  ;; seq-type=general-vector test=other count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=other count=nil key=other end=other 
				      (|remove seq-type=general-vector test=other count=nil key=other|
				       item sequence test start end key)
				      ;;       seq-type=general-vector test=other count=nil key=identity end=other 
				      (|remove seq-type=general-vector test=other count=nil key=identity|
				       item sequence test start end)))
			      ;; seq-type=general-vector test=other end=nil
			      (if count
				  ;; seq-type=general-vector test=other count=other end=nil
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=other count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=t test=other count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=t test=other count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=identity|
					   item sequence test start (length sequence) count))
				      ;; seq-type=general-vector from-end=nil test=other count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=identity|
					   item sequence test start (length sequence) count)))
				  ;; seq-type=general-vector test=other count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=other count=nil key=other end=nil 
				      (|remove seq-type=general-vector test=other count=nil key=other|
				       item sequence test start (length sequence) key)
				      ;;       seq-type=general-vector test=other count=nil key=identity end=nil 
				      (|remove seq-type=general-vector test=other count=nil key=identity|
				       item sequence test start (length sequence)))))))
		  (if test-not
		      ;; seq-type=general-vector test-not=other
		      (if end
			  ;; seq-type=general-vector test-not=other end=other
			  (if count
			      ;; seq-type=general-vector test-not=other count=other end=other
			      (if from-end
				  ;; seq-type=general-vector from-end=t test-not=other count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=other end=other
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start end count))
				  ;; seqr-type=general-vector from-end=nil test-not=other count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=other end=other
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start end count)))
			      ;; seq-type=general-vector test-not=other count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test-not=other count=nil key=other end=other
				  (|remove seq-type=general-vector test-not=other count=nil key=other|
				   item sequence test-not start end key)
				  ;;       seq-type=general-vector test-not=other count=nil key=identity end=other 
				  (|remove seq-type=general-vector test-not=other count=nil key=identity|
				   item sequence test-not start end)))
			  ;; seq-type=general-vector test-not=other end=nil
			  (if count
			      ;; seq-type=general-vector test-not=other count=other end=nil
			      (if from-end
				  ;; seq-type=general-vector from-end=t test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count))
				  ;; seqr-type=general-vector from-end=nil test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count)))
			      ;; seq-type=general-vector test-not=other count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test-not=other count=nil key=other end=nil
				  (|remove seq-type=general-vector test-not=other count=nil key=other|
				   item sequence test-not start (length sequence) key)
				  ;;       seq-type=general-vector test-not=other count=nil key=identity end=nil! 
				  (|remove seq-type=general-vector test-not=other count=nil key=identity|
				   item sequence test-not start (length sequence)))))
		      ;; seq-type=general-vector test=eql
		      (if end
			  ;; seq-type=general-vector test=eql end=other
			  (if count
			      ;; seq-type=general-vector test=eql count=other end=other
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eql count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=other
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
				       item sequence start end count))
				  ;; seqr-type=general-vector from-end=nil test=eql count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=other
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=general-vector test=eql count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eql count=nil key=other end=other
				  (|remove seq-type=general-vector test=eql count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=general-vector test=eql count=nil key=identity end=other 
				  (|remove seq-type=general-vector test=eql count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=general-vector test=eql end=nil
			  (if count
			      ;; seq-type=general-vector test=eql count=other end=nil
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eql count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seqr-type=general-vector from-end=nil test=eql count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=general-vector test=eql count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eql count=nil key=other end=nil
				  (|remove seq-type=general-vector test=eql count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=general-vector test=eql count=nil key=identity end=nil! 
				  (|remove seq-type=general-vector test=eql count=nil key=identity|
				   item sequence start (length sequence)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function remove-if

(defun |remove-if seq-type=list end=nil count=nil key=identity|
    (test list start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (funcall test element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list end=nil count=nil key=other|
    (test list start key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  unless (funcall test (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list end=other count=nil key=identity|
    (test list start end)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list end=other count=nil key=other|
    (test list start end key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list from-end=nil end=nil count=other key=identity|
    (test list count start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (funcall test element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list from-end=nil end=nil count=other key=other|
    (test list start count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  unless (funcall test (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list from-end=nil end=other count=other key=identity|
    (test list start end count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list from-end=nil end=other count=other key=other|
    (test list start end count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  unless (funcall test (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if seq-type=list from-end=t end=nil count=other key=identity|
    (test list start count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test (car reversed-prefix))
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove-if seq-type=list from-end=t end=nil count=other key=other|
    (test list start count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test (funcall key (car reversed-prefix)))
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove-if seq-type=list from-end=t end=other count=other key=identity|
    (test list start end count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test (car reversed-prefix))
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove-if seq-type=list from-end=t end=other count=other key=other|
    (test list start end count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test (funcall key (car reversed-prefix)))
	    do (progn (pop reversed-prefix) (decf count))
	  else
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp)))
    (nreconc reversed-prefix result)))

(defun |remove-if seq-type=general-vector count=nil key=identity|
    (test vector start end)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=general-vector count=nil key=other|
    (test vector start end key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=general-vector from-end=nil count=other key=identity|
    (test vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=general-vector from-end=nil count=other key=other|
    (test vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=general-vector from-end=t count=other key=identity|
    (test vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=general-vector from-end=t count=other key=other|
    (test vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

;;; For vectors, the technique used is to allocate a bitvector that
;;; has the length of the interval in which elements should be
;;; removed, i.e. end - start.  Elements to keep are then marked with
;;; a 1 in that bitvector, and at the same time, we count the number
;;; of 1s.  Finally, we allocate a vector of the correct size, copy
;;; the prefix of the original vector (before start), the elements of
;;; the original vector marked by a 1 in the bitvector in the interval
;;; between start and end, and the suffix of the original vector
;;; (after end).  This technique has the disadvantage that elements of
;;; the original vector in the interval between start and end have to
;;; be accessed twice; once in order to apply the test to see whether
;;; to mark them in the bitvector, and once more to move them from the
;;; original vector to the result vector.  And of course, the
;;; bitvector has to be manipulated as well.  For very quick
;;; combinations of tests and keys, for instance eq and identity, it
;;; may be faster to apply the test twice; once by going through the
;;; original vector and just counting the number of elements to keep,
;;; and then once more in order to move from the original to the
;;; resulting vector.  That method would save the bitvector
;;; manipulation, but it would access *all* of the elements in the the
;;; interval between start and end twice, not only those that are to
;;; be kept.

(defun |remove-if seq-type=simple-vector count=nil key=identity|
    (test vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-vector count=nil key=other|
    (test vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-vector from-end=nil count=other key=identity|
    (test vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-vector from-end=nil count=other key=other|
    (test vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-vector from-end=t count=other key=identity|
    (test vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-vector from-end=t count=other key=other|
    (test vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-string count=nil key=identity|
    (test vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-string count=nil key=other|
    (test vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  unless (funcall test (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-string from-end=nil count=other key=identity|
    (test vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-string from-end=nil count=other key=other|
    (test vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  unless (funcall test (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-string from-end=t count=other key=identity|
    (test vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if seq-type=simple-string from-end=t count=other key=other|
    (test vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  unless (funcall test (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun remove-if (test sequence &key from-end (start 0) end count key)
  ;; FIXME test if it is a sequence at all.
  (if (listp sequence)
      ;; seq-type=list
      (if from-end
	  ;; seq-type=list from-end=t
	  (if end
	      ;; seq-type=list from-end=t end=other
	      (if count
		  ;; seq-type=list from-end=t end=other count=other
		  (if key
		      ;;          seq-type=list from-end=t end=other count=other key=other
		      (|remove-if seq-type=list from-end=t end=other count=other key=other|
		       test sequence start end count key)
		      ;;          seq-type=list from-end=t end=other count=other key=identity
		      (|remove-if seq-type=list from-end=t end=other count=other key=identity|
		       test sequence start end count))
		  ;; seq-type=list from-end=t end=other count=nil
		  (if key
		      ;;          seq-type=list end=other count=nil key=other
		      (|remove-if seq-type=list end=other count=nil key=other|
		       test sequence start end key)
		      ;;          seq-type=list end=other count=nil key=identity
		      (|remove-if seq-type=list end=other count=nil key=identity|
		       test sequence start end)))
	      ;; seq-type=list from-end=t end=nil
	      (if count
		  ;; seq-type=list from-end=t end=nil count=other
		  (if key
		      ;;          seq-type=list from-end=t end=nil count=other key=other
		      (|remove-if seq-type=list from-end=t end=nil count=other key=other|
		       test sequence start count key)
		      ;;          seq-type=list from-end=t end=nil count=other key=identity
		      (|remove-if seq-type=list from-end=t end=nil count=other key=identity|
		       test sequence start count))
		  ;; seq-type=list from-end=t end=nil count=nil
		  (if key
		      ;;          seq-type=list end=nil count=nil key=other
		      (|remove-if seq-type=list end=nil count=nil key=other|
		       test sequence start key)
		      ;;          seq-type=list end=nil count=nil key=identity
		      (|remove-if seq-type=list end=nil count=nil key=identity|
		       test sequence start))))
	  ;; seq-type=list from-end=nil
	  (if end
	      ;; seq-type=list from-end=nil end=other
	      (if count
		  ;; seq-type=list from-end=nil end=other count=other
		  (if key
		      ;;          seq-type=list from-end=nil end=other count=other key=other
		      (|remove-if seq-type=list from-end=nil end=other count=other key=other|
		       test sequence start end count key)
		      ;;          seq-type=list from-end=nil end=other count=other key=identity
		      (|remove-if seq-type=list from-end=nil end=other count=other key=identity|
		       test sequence start end count))
		  ;; seq-type=list from-end=nil end=other count=nil
		  (if key
		      ;;          seq-type=list end=other count=nil key=other
		      (|remove-if seq-type=list end=other count=nil key=other|
		       test sequence start end key)
		      ;;          seq-type=list end=other count=nil key=identity
		      (|remove-if seq-type=list end=other count=nil key=identity|
		       test sequence start end)))
	      ;; seq-type=list from-end=nil end=nil
	      (if count
		  ;; seq-type=list from-end=nil end=nil count=other
		  (if key
		      ;;          seq-type=list from-end=nil end=nil count=other key=other
		      (|remove-if seq-type=list from-end=nil end=nil count=other key=other|
		       test sequence start count key)
		      ;;          seq-type=list from-end=nil end=nil count=other key=identity
		      (|remove-if seq-type=list from-end=nil end=nil count=other key=identity|
		       test sequence start count))
		  ;; seq-type=list from-end=nil end=nil count=nil
		  (if key
		      ;;          seq-type=list end=nil count=nil key=other
		      (|remove-if seq-type=list end=nil count=nil key=other|
		       test sequence start key)
		      (|remove-if seq-type=list end=nil count=nil key=identity|
		       test sequence start)))))
      (if (simple-string-p sequence)
	  ;; seq-type=simple-string
	  ;; seq-type=simple-string test=given
	  ;; seq-type=simple-string
	  (if end
	      ;; seq-type=simple-string end=other
	      (if count
		  ;; seq-type=simple-string count=other end=other
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=other
		      (if key
			  ;;          seq-type=simple-string from-end=t count=other key=other end=other 
			  (|remove-if seq-type=simple-string from-end=t count=other key=other|
			   test sequence start end count key)
			  ;;          seq-type=simple-string from-end=t count=other key=identity end=other 
			  (|remove-if seq-type=simple-string from-end=t count=other key=identity|
			   test sequence start end count))
		      ;; seq-type=simple-string from-end=nil count=other end=other
		      (if key
			  ;;          seq-type=simple-string from-end=nil count=other key=other end=other 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=other|
			   test sequence start end count key)
			  ;;          seq-type=simple-string from-end=nil count=other key=identity end=other 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=identity|
			   test sequence start end count)))
		  ;; seq-type=simple-string count=nil end=other
		  ;; no need to test from-end
		  (if key
		      ;;          seq-type=simple-string count=nil key=other end=other 
		      (|remove-if seq-type=simple-string count=nil key=other|
		       test sequence start end key)
		      ;;          seq-type=simple-string count=nil key=identity end=other 
		      (|remove-if seq-type=simple-string count=nil key=identity|
		       test sequence start end)))
	      ;; seq-type=simple-string end=nil
	      (if count
		  ;; seq-type=simple-string count=other end=nil
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=nil
		      (if key
			  ;;          seq-type=simple-string from-end=t count=other key=other end=nil 
			  (|remove-if seq-type=simple-string from-end=t count=other key=other|
			   test sequence start (length sequence) count key)
			  ;;          seq-type=simple-string from-end=t count=other key=identity end=nil 
			  (|remove-if seq-type=simple-string from-end=t count=other key=identity|
			   test sequence start (length sequence) count))
		      ;; seq-type=simple-string from-end=nil count=other end=nil
		      (if key
			  ;;          seq-type=simple-string from-end=nil count=other key=other end=nil 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=other|
			   test sequence start (length sequence) count key)
			  ;;          seq-type=simple-string from-end=nil count=other key=identity end=nil 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=identity|
			   test sequence start (length sequence) count)))
		  ;; seq-type=simple-string count=nil end=nil
		  ;; no need to test from-end
		  (if key
		      ;;          seq-type=simple-string count=nil key=other end=nil 
		      (|remove-if seq-type=simple-string count=nil key=other|
		       test sequence start (length sequence) key)
		      ;;          seq-type=simple-string count=nil key=identity end=nil 
		      (|remove-if seq-type=simple-string count=nil key=identity|
		       test sequence start (length sequence)))))
	  (if (simple-vector-p sequence)
	      ;; seq-type=simple-vector
	      ;; seq-type=simple-vector test=given
	      ;; seq-type=simple-vector
	      (if end
		  ;; seq-type=simple-vector end=other
		  (if count
		      ;; seq-type=simple-vector count=other end=other
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=other
			  (if key
			      ;;          seq-type=simple-vector from-end=t count=other key=other end=other 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=simple-vector from-end=t count=other key=identity end=other 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=identity|
			       test sequence start end count))
			  ;; seq-type=simple-vector from-end=nil count=other end=other
			  (if key
			      ;;          seq-type=simple-vector from-end=nil count=other key=other end=other 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=simple-vector from-end=nil count=other key=identity end=other 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=identity|
			       test sequence start end count)))
		      ;; seq-type=simple-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=simple-vector count=nil key=other end=other 
			  (|remove-if seq-type=simple-vector count=nil key=other|
			   test sequence start end key)
			  ;;          seq-type=simple-vector count=nil key=identity end=other 
			  (|remove-if seq-type=simple-vector count=nil key=identity|
			   test sequence start end)))
		  ;; seq-type=simple-vector end=nil
		  (if count
		      ;; seq-type=simple-vector count=other end=nil
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=nil
			  (if key
			      ;;          seq-type=simple-vector from-end=t count=other key=other end=nil 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=simple-vector from-end=t count=other key=identity end=nil 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=identity|
			       test sequence start (length sequence) count))
			  ;; seq-type=simple-vector from-end=nil count=other end=nil
			  (if key
			      ;;          seq-type=simple-vector from-end=nil count=other key=other end=nil 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=simple-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=identity|
			       test sequence start (length sequence) count)))
		      ;; seq-type=simple-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=simple-vector count=nil key=other end=nil 
			  (|remove-if seq-type=simple-vector count=nil key=other|
			   test sequence start (length sequence) key)
			  ;;          seq-type=simple-vector count=nil key=identity end=nil 
			  (|remove-if seq-type=simple-vector count=nil key=identity|
			   test sequence start (length sequence)))))
	      ;; seq-type=general-vector
	      (if end
		  ;; seq-type=general-vector end=other
		  (if count
		      ;; seq-type=general-vector count=other end=other
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=other
			  (if key
			      ;;          seq-type=general-vector from-end=t count=other key=other end=other 
			      (|remove-if seq-type=general-vector from-end=t count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=general-vector from-end=t count=other key=identity end=other 
			      (|remove-if seq-type=general-vector from-end=t count=other key=identity|
			       test sequence start end count))
			  ;; seq-type=general-vector from-end=nil count=other end=other
			  (if key
			      ;;          seq-type=general-vector from-end=nil count=other key=other end=other 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=general-vector from-end=nil count=other key=identity end=other 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=identity|
			       test sequence start end count)))
		      ;; seq-type=general-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=general-vector count=nil key=other end=other 
			  (|remove-if seq-type=general-vector count=nil key=other|
			   test sequence start end key)
			  ;;          seq-type=general-vector count=nil key=identity end=other 
			  (|remove-if seq-type=general-vector count=nil key=identity|
			   test sequence start end)))
		  ;; seq-type=general-vector end=nil
		  (if count
		      ;; seq-type=general-vector count=other end=nil
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=nil
			  (if key
			      ;;          seq-type=general-vector from-end=t count=other key=other end=nil 
			      (|remove-if seq-type=general-vector from-end=t count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=general-vector from-end=t count=other key=identity end=nil 
			      (|remove-if seq-type=general-vector from-end=t count=other key=identity|
			       test sequence start (length sequence) count))
			  ;; seq-type=general-vector from-end=nil count=other end=nil
			  (if key
			      ;;          seq-type=general-vector from-end=nil count=other key=other end=nil 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=general-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=identity|
			       test sequence start (length sequence) count)))
		      ;; seq-type=general-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=general-vector count=nil key=other end=nil 
			  (|remove-if seq-type=general-vector count=nil key=other|
			   test sequence start (length sequence) key)
			  ;;          seq-type=general-vector count=nil key=identity end=nil 
			  (|remove-if seq-type=general-vector count=nil key=identity|
			   test sequence start (length sequence)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function remove-if-not

(defun |remove-if-not seq-type=list end=nil count=nil key=identity|
    (test-not list start)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  when (funcall test-not element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list end=nil count=nil key=other|
    (test-not list start key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  for element = (pop list)
	  when (funcall test-not (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp)))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list end=other count=nil key=identity|
    (test-not list start end)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list end=other count=nil key=other|
    (test-not list start end key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list from-end=nil end=nil count=other key=identity|
    (test-not list start count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  when (funcall test-not element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list from-end=nil end=nil count=other key=other|
    (test-not list start count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  for element = (pop list)
	  when (funcall test-not (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list from-end=nil end=other count=other key=identity|
    (test-not list start end count)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not element)
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list from-end=nil end=other count=other key=other|
    (test-not list start end count key)
  (let* ((result (list nil))
	 (last result)
	 (start-bis start)
	 (end-start (- end start)))
    (loop until (zerop start-bis)
	  until (null list)
	  do (let ((temp (list (car list))))
	       (setf (cdr last) temp)
	       (setf last temp)
	       (setf list (cdr list))
	       (decf start-bis)))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    (loop until (null list)
	  until (zerop count)
	  until (zerop end-start)
	  for element = (pop list)
	  when (funcall test-not (funcall key element))
	    do (let ((temp (list element)))
		 (setf (cdr last) temp)
		 (setf last temp))
	  else
	    do (decf count)
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (setf (cdr last) list)
    (cdr result)))

(defun |remove-if-not seq-type=list from-end=t end=nil count=other key=identity|
    (test-not list start count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (car reversed-prefix))
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

(defun |remove-if-not seq-type=list from-end=t end=nil count=other key=other|
    (test-not list start count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    ;; For end=nil, the prefix is the entire list.
    (loop until (null result)
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (funcall key (car reversed-prefix)))
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

(defun |remove-if-not seq-type=list from-end=t end=other count=other key=identity|
    (test-not list start end count)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (car reversed-prefix))
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

(defun |remove-if-not seq-type=list from-end=t end=other count=other key=other|
    (test-not list start end count key)
  (let ((result list)
	(reversed-prefix '())
	(prefix-length 0))
    ;; Reverse a prefix and figure out its length.
    (loop until (null result)
	  repeat end
	  do (push (pop result) reversed-prefix)
	     (incf prefix-length))
    ;; FIXME: Check here whether start is a valid index
    ;; Go through the interval concerned and remove if the test is satisfied.
    ;; The cons cells are ours, so we can reuse them.
    (loop repeat (- prefix-length start)
	  until (zerop count)
	  if (funcall test-not (funcall key (car reversed-prefix)))
	    do (let ((temp (cdr reversed-prefix)))
		 (setf (cdr reversed-prefix) result
		       result reversed-prefix
		       reversed-prefix temp))
	  else
	    do (progn (pop reversed-prefix) (decf count)))
    (nreconc reversed-prefix result)))

(defun |remove-if-not seq-type=general-vector count=nil key=identity|
    (test-not vector start end)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=general-vector count=nil key=other|
    (test-not vector start end key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=general-vector from-end=nil count=other key=identity|
    (test-not vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=general-vector from-end=nil count=other key=other|
    (test-not vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=general-vector from-end=t count=other key=identity|
    (test-not vector start end count)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not (aref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=general-vector from-end=t count=other key=other|
    (test-not vector start end count key)
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not (funcall key (aref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-general vector start end bit-vector items-to-keep)))

;;; For vectors, the technique used is to allocate a bitvector that
;;; has the length of the interval in which elements should be
;;; removed, i.e. end - start.  Elements to keep are then marked with
;;; a 1 in that bitvector, and at the same time, we count the number
;;; of 1s.  Finally, we allocate a vector of the correct size, copy
;;; the prefix of the original vector (before start), the elements of
;;; the original vector marked by a 1 in the bitvector in the interval
;;; between start and end, and the suffix of the original vector
;;; (after end).  This technique has the disadvantage that elements of
;;; the original vector in the interval between start and end have to
;;; be accessed twice; once in order to apply the test to see whether
;;; to mark them in the bitvector, and once more to move them from the
;;; original vector to the result vector.  And of course, the
;;; bitvector has to be manipulated as well.  For very quick
;;; combinations of tests and keys, for instance eq and identity, it
;;; may be faster to apply the test twice; once by going through the
;;; original vector and just counting the number of elements to keep,
;;; and then once more in order to move from the original to the
;;; resulting vector.  That method would save the bitvector
;;; manipulation, but it would access *all* of the elements in the the
;;; interval between start and end twice, not only those that are to
;;; be kept.

(defun |remove-if-not seq-type=simple-vector count=nil key=identity|
    (test-not vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-vector count=nil key=other|
    (test-not vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-vector from-end=nil count=other key=identity|
    (test-not vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-vector from-end=nil count=other key=other|
    (test-not vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-vector from-end=t count=other key=identity|
    (test-not vector start end count)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not (svref vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-vector from-end=t count=other key=other|
    (test-not vector start end count key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not (funcall key (svref vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-string count=nil key=identity|
    (test-not vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-string count=nil key=other|
    (test-not vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  when (funcall test-not (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-string from-end=nil count=other key=identity|
    (test-not vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-string from-end=nil count=other key=other|
    (test-not vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i from start below end
	  until (zerop count)
	  when (funcall test-not (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-string from-end=t count=other key=identity|
    (test-not vector start end count)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not (schar vector i))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun |remove-if-not seq-type=simple-string from-end=t count=other key=other|
    (test-not vector start end count key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (let ((bit-vector (make-array (- end start) :element-type 'bit :initial-element 0))
	(items-to-keep 0))
    (declare (type fixnum items-to-keep))
    (loop for i downfrom (1- end) to start
	  until (zerop count)
	  when (funcall test-not (funcall key (schar vector i)))
	    do (setf (sbit bit-vector (- i start)) 1)
	       (incf items-to-keep)
	  else
	    do (decf count))
    (copy-result-simple-string vector start end bit-vector items-to-keep)))

(defun remove-if-not (test-not sequence &key from-end (start 0) end count key)
  ;; FIXME test if it is a sequence at all.
  (if (listp sequence)
      ;; seq-type=list
      (if from-end
	  ;; seq-type=list from-end=t
	  ;; seq-type=list from-end=t
	  (if end
	      ;; seq-type=list from-end=t end=other
	      (if count
		  ;; seq-type=list from-end=t end=other count=other
		  (if key
		      ;;              seq-type=list from-end=t end=other count=other key=other
		      (|remove-if-not seq-type=list from-end=t end=other count=other key=other|
		       test-not sequence start end count key)
		      ;;              seq-type=list from-end=t end=other count=other key=identity
		      (|remove-if-not seq-type=list from-end=t end=other count=other key=identity|
		       test-not sequence start end count))
		  ;; seq-type=list from-end=t end=other count=nil
		  (if key
		      ;;              seq-type=list end=other count=nil key=other
		      (|remove-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      ;;              seq-type=list end=other count=nil key=identity
		      (|remove-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end)))
	      ;; seq-type=list from-end=t end=nil
	      (if count
		  ;; seq-type=list from-end=t end=nil count=other
		  (if key
		      ;;              seq-type=list from-end=t end=nil count=other key=other
		      (|remove-if-not seq-type=list from-end=t end=nil count=other key=other|
		       test-not sequence start count key)
		      ;;              seq-type=list from-end=t end=nil count=other key=identity
		      (|remove-if-not seq-type=list from-end=t end=nil count=other key=identity|
		       test-not sequence start count))
		  (if key
		      (|remove-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      (|remove-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end))))
	  ;; seq-type=list from-end=nil
	  ;; seq-type=list from-end=nil
	  (if end
	      ;; seq-type=list from-end=nil end=other
	      (if count
		  ;; seq-type=list from-end=nil end=other count=other
		  (if key
		      ;;              seq-type=list from-end=nil end=other count=other key=other
		      (|remove-if-not seq-type=list from-end=nil end=other count=other key=other|
		       test-not sequence start end count key)
		      ;;              seq-type=list from-end=nil end=other count=other key=identity
		      (|remove-if-not seq-type=list from-end=nil end=other count=other key=identity|
		       test-not sequence start end count))
		  ;; seq-type=list from-end=nil end=other count=nil
		  (if key
		      ;;              seq-type=list end=other count=nil key=other
		      (|remove-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      ;;              seq-type=list end=other count=nil key=identity
		      (|remove-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end)))
	      ;; seq-type=list from-end=nil end=nil
	      (if count
		  ;; seq-type=list from-end=nil end=nil count=other
		  (if key
		      ;;              seq-type=list from-end=nil end=nil count=other key=other
		      (|remove-if-not seq-type=list from-end=nil end=nil count=other key=other|
		       test-not sequence start count key)
		      ;;              seq-type=list from-end=nil end=nil count=other key=identity
		      (|remove-if-not seq-type=list from-end=nil end=nil count=other key=identity|
		       test-not sequence start count))
		  (if key
		      (|remove-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      (|remove-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end)))))
      (if (simple-string-p sequence)
	  ;; seq-type=simple-string
	  ;; seq-type=simple-string
	  (if end
	      ;; seq-type=simple-string end=other
	      (if count
		  ;; seq-type=simple-string count=other end=other
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=other
		      (if key
			  ;;              seq-type=simple-string from-end=t count=other key=other end=other
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=other|
			   test-not sequence start end count key)
			  ;;              seq-type=simple-string from-end=t count=other key=identity end=other 
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=identity|
			   test-not sequence start end count))
		      ;; seqr-type=simple-string from-end=nil count=other end=other
		      (if key
			  ;;              seq-type=simple-string from-end=nil count=other key=other end=other
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=other|
			   test-not sequence start end count key)
			  ;;              seq-type=simple-string from-end=nil count=other key=identity end=other 
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=identity|
			   test-not sequence start end count)))
		  ;; seq-type=simple-string count=nil end=other
		  ;; no need to test from-end
		  (if key
		      ;;              seq-type=simple-string count=nil key=other end=other
		      (|remove-if-not seq-type=simple-string count=nil key=other|
		       test-not sequence start end key)
		      ;;              seq-type=simple-string count=nil key=identity end=other 
		      (|remove-if-not seq-type=simple-string count=nil key=identity|
		       test-not sequence start end)))
	      ;; seq-type=simple-string end=nil
	      (if count
		  ;; seq-type=simple-string count=other end=nil
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=nil
		      (if key
			  ;;              seq-type=simple-string from-end=t count=other key=other end=nil
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=other|
			   test-not sequence start (length sequence) count key)
			  ;;              seq-type=simple-string from-end=t count=other key=identity end=nil 
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=identity|
			   test-not sequence start (length sequence) count))
		      ;; seqr-type=simple-string from-end=nil count=other end=nil
		      (if key
			  ;;              seq-type=simple-string from-end=nil count=other key=other end=nil
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=other|
			   test-not sequence start (length sequence) count key)
			  ;;              seq-type=simple-string from-end=nil count=other key=identity end=nil 
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=identity|
			   test-not sequence start (length sequence) count)))
		  ;; seq-type=simple-string count=nil end=nil
		  ;; no need to test from-end
		  (if key
		      ;;              seq-type=simple-string count=nil key=other end=nil
		      (|remove-if-not seq-type=simple-string count=nil key=other|
		       test-not sequence start (length sequence) key)
		      ;;              seq-type=simple-string count=nil key=identity end=nil! 
		      (|remove-if-not seq-type=simple-string count=nil key=identity|
		       test-not sequence start (length sequence)))))
	  (if (simple-vector-p sequence)
	      ;; seq-type=simple-vector
	      ;; seq-type=simple-vector
	      (if end
		  ;; seq-type=simple-vector end=other
		  (if count
		      ;; seq-type=simple-vector count=other end=other
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=other
			  (if key
			      ;;              seq-type=simple-vector from-end=t count=other key=other end=other
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=simple-vector from-end=t count=other key=identity end=other 
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=identity|
			       test-not sequence start end count))
			  ;; seqr-type=simple-vector from-end=nil count=other end=other
			  (if key
			      ;;              seq-type=simple-vector from-end=nil count=other key=other end=other
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=simple-vector from-end=nil count=other key=identity end=other 
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=identity|
			       test-not sequence start end count)))
		      ;; seq-type=simple-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=simple-vector count=nil key=other end=other
			  (|remove-if-not seq-type=simple-vector count=nil key=other|
			   test-not sequence start end key)
			  ;;              seq-type=simple-vector count=nil key=identity end=other 
			  (|remove-if-not seq-type=simple-vector count=nil key=identity|
			   test-not sequence start end)))
		  ;; seq-type=simple-vector end=nil
		  (if count
		      ;; seq-type=simple-vector count=other end=nil
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=nil
			  (if key
			      ;;              seq-type=simple-vector from-end=t count=other key=other end=nil
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=simple-vector from-end=t count=other key=identity end=nil 
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=identity|
			       test-not sequence start (length sequence) count))
			  ;; seqr-type=simple-vector from-end=nil count=other end=nil
			  (if key
			      ;;              seq-type=simple-vector from-end=nil count=other key=other end=nil
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=simple-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=identity|
			       test-not sequence start (length sequence) count)))
		      ;; seq-type=simple-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=simple-vector count=nil key=other end=nil
			  (|remove-if-not seq-type=simple-vector count=nil key=other|
			   test-not sequence start (length sequence) key)
			  ;;              seq-type=simple-vector count=nil key=identity end=nil! 
			  (|remove-if-not seq-type=simple-vector count=nil key=identity|
			   test-not sequence start (length sequence)))))
	      ;; seq-type=general-vector
	      ;; seq-type=general-vector
	      (if end
		  ;; seq-type=general-vector end=other
		  (if count
		      ;; seq-type=general-vector count=other end=other
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=other
			  (if key
			      ;;              seq-type=general-vector from-end=t count=other key=other end=other
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=general-vector from-end=t count=other key=identity end=other 
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=identity|
			       test-not sequence start end count))
			  ;; seqr-type=general-vector from-end=nil count=other end=other
			  (if key
			      ;;              seq-type=general-vector from-end=nil count=other key=other end=other
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=general-vector from-end=nil count=other key=identity end=other 
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=identity|
			       test-not sequence start end count)))
		      ;; seq-type=general-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=general-vector count=nil key=other end=other
			  (|remove-if-not seq-type=general-vector count=nil key=other|
			   test-not sequence start end key)
			  ;;              seq-type=general-vector count=nil key=identity end=other 
			  (|remove-if-not seq-type=general-vector count=nil key=identity|
			   test-not sequence start end)))
		  ;; seq-type=general-vector end=nil
		  (if count
		      ;; seq-type=general-vector count=other end=nil
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=nil
			  (if key
			      ;;              seq-type=general-vector from-end=t count=other key=other end=nil
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=general-vector from-end=t count=other key=identity end=nil 
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=identity|
			       test-not sequence start (length sequence) count))
			  ;; seqr-type=general-vector from-end=nil count=other end=nil
			  (if key
			      ;;              seq-type=general-vector from-end=nil count=other key=other end=nil
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=general-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=identity|
			       test-not sequence start (length sequence) count)))
		      ;; seq-type=general-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=general-vector count=nil key=other end=nil
			  (|remove-if-not seq-type=general-vector count=nil key=other|
			   test-not sequence start (length sequence) key)
			  ;;              seq-type=general-vector count=nil key=identity end=nil! 
			  (|remove-if-not seq-type=general-vector count=nil key=identity|
			   test-not sequence start (length sequence)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function delete

(defun |delete seq-type=list test=eql end=nil count=nil key=identity|
    (item list start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (eql item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test=eq end=nil count=nil key=identity|
    (item list start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (eq item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test=eql end=nil count=nil key=other|
    (item list start key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (eql item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test=eq end=nil count=nil key=other|
    (item list start key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (eq item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test=eql end=other count=nil key=identity|
    (item list start end)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (eql item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list test=eq end=other count=nil key=identity|
    (item list start end)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (eq item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list test=eql end=other count=nil key=other|
    (item list start end key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (eql item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list test=eq end=other count=nil key=other|
    (item list start end key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (eq item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list test=other end=nil count=nil key=identity|
    (item list test start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (funcall test item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test=other end=nil count=nil key=other|
    (item list test start key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (funcall test item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test=other end=other count=nil key=identity|
    (item list test start end)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (funcall test item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list test=other end=other count=nil key=other|
    (item list test start end key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (funcall test item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list test-not=other end=nil count=nil key=identity|
    (item list test-not start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  unless (funcall test-not item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test-not=other end=nil count=nil key=other|
    (item list test-not start key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  unless (funcall test-not item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list test-not=other end=other count=nil key=identity|
    (item list test-not start end)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  unless (funcall test-not item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list test-not=other end=other count=nil key=other|
    (item list test-not start end key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  unless (funcall test-not item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eql end=nil count=other key=identity|
    (item list start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (eql item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eq end=nil count=other key=identity|
    (item list start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (eq item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eql end=nil count=other key=other|
    (item list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (eql item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eq end=nil count=other key=other|
    (item list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (eq item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eql end=other count=other key=identity|
    (item list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (eql item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eq end=other count=other key=identity|
    (item list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (eq item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eql end=other count=other key=other|
    (item list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (eql item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=eq end=other count=other key=other|
    (item list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (eq item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=other end=nil count=other key=identity|
    (item list test count start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (funcall test item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=other end=nil count=other key=other|
    (item list test start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (funcall test item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=other end=other count=other key=identity|
    (item list test start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (funcall test item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test=other end=other count=other key=other|
    (item list test start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (funcall test item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test-not=other end=nil count=other key=identity|
    (item list test-not start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  unless (funcall test-not item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test-not=other end=nil count=other key=other|
    (item list test-not start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  unless (funcall test-not item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test-not=other end=other count=other key=identity|
    (item list test-not start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  unless (funcall test-not item (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=nil test-not=other end=other count=other key=other|
    (item list test-not start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  unless (funcall test-not item (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eql end=nil count=other key=identity|
    (item list start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eql item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eq end=nil count=other key=identity|
    (item list start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eq item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eql end=nil count=other key=other|
    (item list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eql item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eq end=nil count=other key=other|
    (item list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eq item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eql end=other count=other key=identity|
    (item list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eql item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eq end=other count=other key=identity|
    (item list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eq item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eql end=other count=other key=other|
    (item list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eql item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=eq end=other count=other key=other|
    (item list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (eq item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=other end=nil count=other key=identity|
    (item list test start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=other end=nil count=other key=other|
    (item list test start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=other end=other count=other key=identity|
    (item list test start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test=other end=other count=other key=other|
    (item list test start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test-not=other end=nil count=other key=identity|
    (item list test-not start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test-not=other end=nil count=other key=other|
    (item list test-not start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test-not=other end=other count=other key=identity|
    (item list test-not start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not item (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete seq-type=list from-end=t test-not=other end=other count=other key=other|
    (item list test-not start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not item (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

;;; For now, use the corresponding remove functions on sequences

(defun delete (item sequence &key from-end test test-not (start 0) end count key)
  (assert (or (null test) (null test-not)))
  ;; FIXME test if it is a sequence at all.
  (if (listp sequence)
      ;; seq-type=list
      (if from-end
	  ;; seq-type=list from-end=t
	  (if test
	      ;; seq-type=list from-end=t test=?
	      (if (or (eq test 'eq) (eq test #'eq))
		  ;; seq-type=list from-end=t test=eq
		  (if end
		      ;; seq-type=list from-end=t test=eq end=other
		      (if count
			  ;; seq-type=list from-end=t test=eq end=other count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eq end=other count=other key=other
			      (|delete seq-type=list from-end=t test=eq end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=t test=eq end=other count=other key=identity
			      (|delete seq-type=list from-end=t test=eq end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=t test=eq end=other count=nil
			  (if key
			      ;;       seq-type=list test=eq end=other count=nil key=other
			      (|delete seq-type=list test=eq end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eq end=other count=nil key=identity
			      (|delete seq-type=list test=eq end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=t test=eq end=nil
		      (if count
			  ;; seq-type=list from-end=t test=eq end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eq end=nil count=other key=other
			      (|delete seq-type=list from-end=t test=eq end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=t test=eq end=nil count=other key=identity
			      (|delete seq-type=list from-end=t test=eq end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=t test=eq end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eq end=nil count=nil key=other
			      (|delete seq-type=list test=eq end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eq end=nil count=nil key=identity
			      (|delete seq-type=list test=eq end=nil count=nil key=identity|
			       item sequence start))))
		  (if (or (eq test 'eql) (eq test #'eql))
		      ;; seq-type=list from-end=t test=eql
		      (if end
			  ;; seq-type=list from-end=t test=eql end=other
			  (if count
			      ;; seq-type=list from-end=t test=eql end=other count=other
			      (if key
				  ;;       seq-type=list from-end=t test=eql end=other count=other key=other
				  (|delete seq-type=list from-end=t test=eql end=other count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=list from-end=t test=eql end=other count=other key=identity
				  (|delete seq-type=list from-end=t test=eql end=other count=other key=identity|
				   item sequence start end count))
			      ;; seq-type=list from-end=t test=eql end=other count=nil
			      (if key
				  ;;       seq-type=list test=eql end=other count=nil key=other
				  (|delete seq-type=list test=eql end=other count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=list test=eql end=other count=nil key=identity
				  (|delete seq-type=list test=eql end=other count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=list from-end=t test=eql end=nil
			  (if count
			      ;; seq-type=list from-end=t test=eql end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=t test=eql end=nil count=other key=other
				  (|delete seq-type=list from-end=t test=eql end=nil count=other key=other|
				   item sequence start count key)
				  ;;       seq-type=list from-end=t test=eql end=nil count=other key=identity
				  (|delete seq-type=list from-end=t test=eql end=nil count=other key=identity|
				   item sequence start count))
			      ;; seq-type=list from-end=t test=eql end=nil count=nil
			      (if key
				  ;;       seq-type=list test=eql end=nil count=nil key=other
				  (|delete seq-type=list test=eql end=nil count=nil key=other|
				   item sequence start key)
				  ;;       seq-type=list test=eql end=nil count=nil key=identity
				  (|delete seq-type=list test=eql end=nil count=nil key=identity|
				   item sequence start))))
		      ;; seq-type=list from-end=t test=other
		      (if end
			  ;; seq-type=list from-end=t test=other end=other
			  (if count
			      ;; seq-type=list from-end=t test=other end=other count=other
			      (if key
				  ;;       seq-type=list from-end=t test=other end=other count=other key=other
				  (|delete seq-type=list from-end=t test=other end=other count=other key=other|
				   item sequence test start end count key)
				  ;;       seq-type=list from-end=t test=other end=other count=other key=identity
				  (|delete seq-type=list from-end=t test=other end=other count=other key=identity|
				   item sequence test start end count))
			      ;; seq-type=list from-end=t test=other end=other count=nil
			      (if key
				  ;;       seq-type=list test=other end=other count=nil key=other
				  (|delete seq-type=list test=other end=other count=nil key=other|
				   item sequence test start end key)
				  ;;       seq-type=list test=other end=other count=nil key=identity
				  (|delete seq-type=list test=other end=other count=nil key=identity|
				   item sequence test start end)))
			  ;; seq-type=list from-end=t test=other end=nil
			  (if count
			      ;; seq-type=list from-end=t test=other end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=t test=other end=nil count=other key=other
				  (|delete seq-type=list from-end=t test=other end=nil count=other key=other|
				   item sequence test start count key)
				  ;;       seq-type=list from-end=t test=other end=nil count=other key=identity
				  (|delete seq-type=list from-end=t test=other end=nil count=other key=identity|
				   item sequence test start count))
			      ;; seq-type=list from-end=t test=other end=nil count=nil
			      (if key
				  ;;       seq-type=list test=other end=nil count=nil key=other
				  (|delete seq-type=list test=other end=nil count=nil key=other|
				   item sequence test start key)
				  ;;       seq-type=list test=other end=nil count=nil key=identity
				  (|delete seq-type=list test=other end=nil count=nil key=identity|
				   item sequence test start))))))
	      (if test-not
		  ;; seq-type=list from-end=t test-not=other
		  (if end
		      ;; seq-type=list from-end=t test-not=other end=other
		      (if count
			  ;; seq-type=list from-end=t test-not=other end=other count=other
			  (if key
			      ;;       seq-type=list from-end=t test-not=other end=other count=other key=other
			      (|delete seq-type=list from-end=t test-not=other end=other count=other key=other|
			       item sequence test-not start end count key)
			      ;;       seq-type=list from-end=t test-not=other end=other count=other key=identity
			      (|delete seq-type=list from-end=t test-not=other end=other count=other key=identity|
			       item sequence test-not start end count))
			  ;; seq-type=list from-end=t test-not=other end=other count=nil
			  (if key
			      ;;       seq-type=list test-not=other end=other count=nil key=other
			      (|delete seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      ;;       seq-type=list test-not=other end=other count=nil key=identity
			      (|delete seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end)))
		      ;; seq-type=list from-end=t test-not=other end=nil
		      (if count
			  ;; seq-type=list from-end=t test-not=other end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=t test-not=other end=nil count=other key=other
			      (|delete seq-type=list from-end=t test-not=other end=nil count=other key=other|
			       item sequence test-not start count key)
			      ;;       seq-type=list from-end=t test-not=other end=nil count=other key=identity
			      (|delete seq-type=list from-end=t test-not=other end=nil count=other key=identity|
			       item sequence test-not start count))
			  (if key
			      (|delete seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      (|delete seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end))))
		  ;; seq-type=list from-end=t test=eql
		  (if end
		      ;; seq-type=list from-end=t test=eql end=other
		      (if count
			  ;; seq-type=list from-end=t test=eql end=other count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eql end=other count=other key=other
			      (|delete seq-type=list from-end=t test=eql end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=t test=eql end=other count=other key=identity
			      (|delete seq-type=list from-end=t test=eql end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=t test=eql end=other count=nil
			  (if key
			      ;;       seq-type=list test=eql end=other count=nil key=other
			      (|delete seq-type=list test=eql end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eql end=other count=nil key=identity
			      (|delete seq-type=list test=eql end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=t test=eql end=nil
		      (if count
			  ;; seq-type=list from-end=t test=eql end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=t test=eql end=nil count=other key=other
			      (|delete seq-type=list from-end=t test=eql end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=t test=eql end=nil count=other key=identity
			      (|delete seq-type=list from-end=t test=eql end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=t test=eql end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eql end=nil count=nil key=other
			      (|delete seq-type=list test=eql end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eql end=nil count=nil key=identity
			      (|delete seq-type=list test=eql end=nil count=nil key=identity|
			       item sequence start))))))
	  ;; seq-type=list from-end=nil
	  (if test
	      ;; seq-type=list from-end=nil test=?
	      (if (or (eq test 'eq) (eq test #'eq))
		  ;; seq-type=list from-end=nil test=eq
		  (if end
		      ;; seq-type=list from-end=nil test=eq end=other
		      (if count
			  ;; seq-type=list from-end=nil test=eq end=other count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eq end=other count=other key=other
			      (|delete seq-type=list from-end=nil test=eq end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=nil test=eq end=other count=other key=identity
			      (|delete seq-type=list from-end=nil test=eq end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=nil test=eq end=other count=nil
			  (if key
			      ;;       seq-type=list test=eq end=other count=nil key=other
			      (|delete seq-type=list test=eq end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eq end=other count=nil key=identity
			      (|delete seq-type=list test=eq end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=nil test=eq end=nil
		      (if count
			  ;; seq-type=list from-end=nil test=eq end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eq end=nil count=other key=other
			      (|delete seq-type=list from-end=nil test=eq end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=nil test=eq end=nil count=other key=identity
			      (|delete seq-type=list from-end=nil test=eq end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=nil test=eq end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eq end=nil count=nil key=other
			      (|delete seq-type=list test=eq end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eq end=nil count=nil key=identity
			      (|delete seq-type=list test=eq end=nil count=nil key=identity|
			       item sequence start))))
		  (if (or (eq test 'eql) (eq test #'eql))
		      ;; seq-type=list from-end=nil test=eql
		      (if end
			  ;; seq-type=list from-end=nil test=eql end=other
			  (if count
			      ;; seq-type=list from-end=nil test=eql end=other count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=eql end=other count=other key=other
				  (|delete seq-type=list from-end=nil test=eql end=other count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=list from-end=nil test=eql end=other count=other key=identity
				  (|delete seq-type=list from-end=nil test=eql end=other count=other key=identity|
				   item sequence start end count))
			      ;; seq-type=list from-end=nil test=eql end=other count=nil
			      (if key
				  ;;       seq-type=list test=eql end=other count=nil key=other
				  (|delete seq-type=list test=eql end=other count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=list test=eql end=other count=nil key=identity
				  (|delete seq-type=list test=eql end=other count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=list from-end=nil test=eql end=nil
			  (if count
			      ;; seq-type=list from-end=nil test=eql end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=eql end=nil count=other key=other
				  (|delete seq-type=list from-end=nil test=eql end=nil count=other key=other|
				   item sequence start count key)
				  ;;       seq-type=list from-end=nil test=eql end=nil count=other key=identity
				  (|delete seq-type=list from-end=nil test=eql end=nil count=other key=identity|
				   item sequence start count))
			      ;; seq-type=list from-end=nil test=eql end=nil count=nil
			      (if key
				  ;;       seq-type=list test=eql end=nil count=nil key=other
				  (|delete seq-type=list test=eql end=nil count=nil key=other|
				   item sequence start key)
				  (|delete seq-type=list test=eql end=nil count=nil key=identity|
				   item sequence start))))
		      ;; seq-type=list from-end=nil test=other
		      (if end
			  ;; seq-type=list from-end=nil test=other end=other
			  (if count
			      ;; seq-type=list from-end=nil test=other end=other count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=other end=other count=other key=other
				  (|delete seq-type=list from-end=nil test=other end=other count=other key=other|
				   item sequence test start end count key)
				  ;;       seq-type=list from-end=nil test=other end=other count=other key=identity
				  (|delete seq-type=list from-end=nil test=other end=other count=other key=identity|
				   item sequence test start end count))
			      ;; seq-type=list from-end=nil test=other end=other count=nil
			      (if key
				  ;;       seq-type=list test=other end=other count=nil key=other
				  (|delete seq-type=list test=other end=other count=nil key=other|
				   item sequence test start end key)
				  ;;       seq-type=list test=other end=other count=nil key=identity
				  (|delete seq-type=list test=other end=other count=nil key=identity|
				   item sequence test start end)))
			  ;; seq-type=list from-end=nil test=other end=nil
			  (if count
			      ;; seq-type=list from-end=nil test=other end=nil count=other
			      (if key
				  ;;       seq-type=list from-end=nil test=other end=nil count=other key=other
				  (|delete seq-type=list from-end=nil test=other end=nil count=other key=other|
				   item sequence test start count key)
				  ;;       seq-type=list from-end=nil test=other end=nil count=other key=identity
				  (|delete seq-type=list from-end=nil test=other end=nil count=other key=identity|
				   item sequence test start count))
			      ;; seq-type=list from-end=nil test=other end=nil count=nil
			      (if key
				  ;;       seq-type=list test=other end=nil count=nil key=other
				  (|delete seq-type=list test=other end=nil count=nil key=other|
				   item sequence test start key)
				  (|delete seq-type=list test=other end=nil count=nil key=identity|
				   item sequence test start))))))
	      (if test-not
		  ;; seq-type=list from-end=nil test-not=other
		  (if end
		      ;; seq-type=list from-end=nil test-not=other end=other
		      (if count
			  ;; seq-type=list from-end=nil test-not=other end=other count=other
			  (if key
			      ;;       seq-type=list from-end=nil test-not=other end=other count=other key=other
			      (|delete seq-type=list from-end=nil test-not=other end=other count=other key=other|
			       item sequence test-not start end count key)
			      ;;       seq-type=list from-end=nil test-not=other end=other count=other key=identity
			      (|delete seq-type=list from-end=nil test-not=other end=other count=other key=identity|
			       item sequence test-not start end count))
			  ;; seq-type=list from-end=nil test-not=other end=other count=nil
			  (if key
			      ;;       seq-type=list test-not=other end=other count=nil key=other
			      (|delete seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      ;;       seq-type=list test-not=other end=other count=nil key=identity
			      (|delete seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end)))
		      ;; seq-type=list from-end=nil test-not=other end=nil
		      (if count
			  ;; seq-type=list from-end=nil test-not=other end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=nil test-not=other end=nil count=other key=other
			      (|delete seq-type=list from-end=nil test-not=other end=nil count=other key=other|
			       item sequence test-not start count key)
			      ;;       seq-type=list from-end=nil test-not=other end=nil count=other key=identity
			      (|delete seq-type=list from-end=nil test-not=other end=nil count=other key=identity|
			       item sequence test-not start count))
			  (if key
			      (|delete seq-type=list test-not=other end=other count=nil key=other|
			       item sequence test-not start end key)
			      (|delete seq-type=list test-not=other end=other count=nil key=identity|
			       item sequence test-not start end))))
		  ;; seq-type=list from-end=nil test=eql
		  (if end
		      ;; seq-type=list from-end=nil test=eql end=other
		      (if count
			  ;; seq-type=list from-end=nil test=eql end=other count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eql end=other count=other key=other
			      (|delete seq-type=list from-end=nil test=eql end=other count=other key=other|
			       item sequence start end count key)
			      ;;       seq-type=list from-end=nil test=eql end=other count=other key=identity
			      (|delete seq-type=list from-end=nil test=eql end=other count=other key=identity|
			       item sequence start end count))
			  ;; seq-type=list from-end=nil test=eql end=other count=nil
			  (if key
			      ;;       seq-type=list test=eql end=other count=nil key=other
			      (|delete seq-type=list test=eql end=other count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=list test=eql end=other count=nil key=identity
			      (|delete seq-type=list test=eql end=other count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=list from-end=nil test=eql end=nil
		      (if count
			  ;; seq-type=list from-end=nil test=eql end=nil count=other
			  (if key
			      ;;       seq-type=list from-end=nil test=eql end=nil count=other key=other
			      (|delete seq-type=list from-end=nil test=eql end=nil count=other key=other|
			       item sequence start count key)
			      ;;       seq-type=list from-end=nil test=eql end=nil count=other key=identity
			      (|delete seq-type=list from-end=nil test=eql end=nil count=other key=identity|
			       item sequence start count))
			  ;; seq-type=list from-end=nil test=eql end=nil count=nil
			  (if key
			      ;;       seq-type=list test=eql end=nil count=nil key=other
			      (|delete seq-type=list test=eql end=nil count=nil key=other|
			       item sequence start key)
			      ;;       seq-type=list test=eql end=nil count=nil key=identity
			      (|delete seq-type=list test=eql end=nil count=nil key=identity|
			       item sequence start)))))))
      (if (simple-string-p sequence)
	  ;; seq-type=simple-string
	  (if test
	      ;; seq-type=simple-string test=given
	      (if (or (eq test 'eq) (eq test #'eq))
		  ;; seq-type=simple-string test=eq
		  (if end
		      ;; seq-type=simple-string test=eq end=other
		      (if count
			  ;; seq-type=simple-string test=eq count=other end=other
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eq count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=other end=other 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=identity|
				   item sequence start end count))
			      ;; seq-type=simple-string from-end=nil test=eq count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=other end=other 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=identity|
				   item sequence start end count)))
			  ;; seq-type=simple-string test=eq count=nil end=other
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eq count=nil key=other end=other 
			      (|remove seq-type=simple-string test=eq count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=simple-string test=eq count=nil key=identity end=other 
			      (|remove seq-type=simple-string test=eq count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=simple-string test=eq end=nil
		      (if count
			  ;; seq-type=simple-string test=eq count=other end=nil
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eq count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=other end=nil 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=t test=eq count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=t test=eq count=other key=identity|
				   item sequence start (length sequence) count))
			      ;; seq-type=simple-string from-end=nil test=eq count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=other end=nil 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=nil test=eq count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=nil test=eq count=other key=identity|
				   item sequence start (length sequence) count)))
			  ;; seq-type=simple-string test=eq count=nil end=nil
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eq count=nil key=other end=nil 
			      (|remove seq-type=simple-string test=eq count=nil key=other|
			       item sequence start (length sequence) key)
			      ;;       seq-type=simple-string test=eq count=nil key=identity end=nil 
			      (|remove seq-type=simple-string test=eq count=nil key=identity|
			       item sequence start (length sequence)))))
		  (if (or (eq test 'eql) (eq test #'eql))
		      ;; seq-type=simple-string test=eql
		      (if end
			  ;; seq-type=simple-string test=eql end=other
			  (if count
			      ;; seq-type=simple-string test=eql count=other end=other
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				       item sequence start end count))
				  ;; seq-type=simple-string from-end=nil test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=simple-string test=eql count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=eql count=nil key=other end=other 
				  (|remove seq-type=simple-string test=eql count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=simple-string test=eql count=nil key=identity end=other 
				  (|remove seq-type=simple-string test=eql count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=simple-string test=eql end=nil
			  (if count
			      ;; seq-type=simple-string test=eql count=other end=nil
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seq-type=simple-string from-end=nil test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=simple-string test=eql count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=eql count=nil key=other end=nil 
				  (|remove seq-type=simple-string test=eql count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=simple-string test=eql count=nil key=identity end=nil 
				  (|remove seq-type=simple-string test=eql count=nil key=identity|
				   item sequence start (length sequence)))))
		      ;; seq-type=simple-string test=other
		      (if end
			  ;; seq-type=simple-string test=other end=other
			  (if count
			      ;; seq-type=simple-string test=other count=other end=other
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=other count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=t test=other count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=other|
				       item sequence test start end count key)
				      ;;       seq-type=simple-string from-end=t test=other count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=identity|
				       item sequence test start end count))
				  ;; seq-type=simple-string from-end=nil test=other count=other end=other
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=other end=other 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=other|
				       item sequence test start end count key)
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=identity end=other 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=identity|
				       item sequence test start end count)))
			      ;; seq-type=simple-string test=other count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=other count=nil key=other end=other 
				  (|remove seq-type=simple-string test=other count=nil key=other|
				   item sequence test start end key)
				  ;;       seq-type=simple-string test=other count=nil key=identity end=other 
				  (|remove seq-type=simple-string test=other count=nil key=identity|
				   item sequence test start end)))
			  ;; seq-type=simple-string test=other end=nil
			  (if count
			      ;; seq-type=simple-string test=other count=other end=nil
			      (if from-end
				  ;; seq-type=simple-string from-end=t test=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=t test=other count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=other|
				       item sequence test start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=t test=other count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=t test=other count=other key=identity|
				       item sequence test start (length sequence) count))
				  ;; seq-type=simple-string from-end=nil test=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=other end=nil 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=other|
				       item sequence test start (length sequence) count key)
				      ;;       seq-type=simple-string from-end=nil test=other count=other key=identity end=nil 
				      (|remove seq-type=simple-string from-end=nil test=other count=other key=identity|
				       item sequence test start (length sequence) count)))
			      ;; seq-type=simple-string test=other count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-string test=other count=nil key=other end=nil 
				  (|remove seq-type=simple-string test=other count=nil key=other|
				   item sequence test start (length sequence) key)
				  ;;       seq-type=simple-string test=other count=nil key=identity end=nil 
				  (|remove seq-type=simple-string test=other count=nil key=identity|
				   item sequence test start (length sequence)))))))
	      (if test-not
		  ;; seq-type=simple-string test-not=other
		  (if end
		      ;; seq-type=simple-string test-not=other end=other
		      (if count
			  ;; seq-type=simple-string test-not=other count=other end=other
			  (if from-end
			      ;; seq-type=simple-string from-end=t test-not=other count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=other end=other
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=other|
				   item sequence test-not start end count key)
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=identity|
				   item sequence test-not start end count))
			      ;; seqr-type=simple-string from-end=nil test-not=other count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=other end=other
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=other|
				   item sequence test-not start end count key)
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=identity|
				   item sequence test-not start end count)))
			  ;; seq-type=simple-string test-not=other count=nil end=other
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test-not=other count=nil key=other end=other
			      (|remove seq-type=simple-string test-not=other count=nil key=other|
			       item sequence test-not start end key)
			      ;;       seq-type=simple-string test-not=other count=nil key=identity end=other 
			      (|remove seq-type=simple-string test-not=other count=nil key=identity|
			       item sequence test-not start end)))
		      ;; seq-type=simple-string test-not=other end=nil
		      (if count
			  ;; seq-type=simple-string test-not=other count=other end=nil
			  (if from-end
			      ;; seq-type=simple-string from-end=t test-not=other count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=other|
				   item sequence test-not start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=t test-not=other count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=t test-not=other count=other key=identity|
				   item sequence test-not start (length sequence) count))
			      ;; seqr-type=simple-string from-end=nil test-not=other count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=other|
				   item sequence test-not start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=nil test-not=other count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=nil test-not=other count=other key=identity|
				   item sequence test-not start (length sequence) count)))
			  ;; seq-type=simple-string test-not=other count=nil end=nil
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test-not=other count=nil key=other end=nil
			      (|remove seq-type=simple-string test-not=other count=nil key=other|
			       item sequence test-not start (length sequence) key)
			      ;;       seq-type=simple-string test-not=other count=nil key=identity end=nil! 
			      (|remove seq-type=simple-string test-not=other count=nil key=identity|
			       item sequence test-not start (length sequence)))))
		  ;; seq-type=simple-string test=eql
		  (if end
		      ;; seq-type=simple-string test=eql end=other
		      (if count
			  ;; seq-type=simple-string test=eql count=other end=other
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eql count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=other
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				   item sequence start end count))
			      ;; seqr-type=simple-string from-end=nil test=eql count=other end=other
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=other
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				   item sequence start end count key)
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=other 
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				   item sequence start end count)))
			  ;; seq-type=simple-string test=eql count=nil end=other
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eql count=nil key=other end=other
			      (|remove seq-type=simple-string test=eql count=nil key=other|
			       item sequence start end key)
			      ;;       seq-type=simple-string test=eql count=nil key=identity end=other 
			      (|remove seq-type=simple-string test=eql count=nil key=identity|
			       item sequence start end)))
		      ;; seq-type=simple-string test=eql end=nil
		      (if count
			  ;; seq-type=simple-string test=eql count=other end=nil
			  (if from-end
			      ;; seq-type=simple-string from-end=t test=eql count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=t test=eql count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=t test=eql count=other key=identity|
				   item sequence start (length sequence) count))
			      ;; seqr-type=simple-string from-end=nil test=eql count=other end=nil
			      (if key
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=other end=nil
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=other|
				   item sequence start (length sequence) count key)
				  ;;       seq-type=simple-string from-end=nil test=eql count=other key=identity end=nil 
				  (|remove seq-type=simple-string from-end=nil test=eql count=other key=identity|
				   item sequence start (length sequence) count)))
			  ;; seq-type=simple-string test=eql count=nil end=nil
			  ;; no need to test from-end
			  (if key
			      ;;       seq-type=simple-string test=eql count=nil key=other end=nil
			      (|remove seq-type=simple-string test=eql count=nil key=other|
			       item sequence start (length sequence) key)
			      ;;       seq-type=simple-string test=eql count=nil key=identity end=nil! 
			      (|remove seq-type=simple-string test=eql count=nil key=identity|
			       item sequence start (length sequence)))))))
	  (if (simple-vector-p sequence)
	      ;; seq-type=simple-vector
	      (if test
		  ;; seq-type=simple-vector test=given
		  (if (or (eq test 'eq) (eq test #'eq))
		      ;; seq-type=simple-vector test=eq
		      (if end
			  ;; seq-type=simple-vector test=eq end=other
			  (if count
			      ;; seq-type=simple-vector test=eq count=other end=other
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eq count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=other end=other 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=identity|
				       item sequence start end count))
				  ;; seq-type=simple-vector from-end=nil test=eq count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=other end=other 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=simple-vector test=eq count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eq count=nil key=other end=other 
				  (|remove seq-type=simple-vector test=eq count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=simple-vector test=eq count=nil key=identity end=other 
				  (|remove seq-type=simple-vector test=eq count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=simple-vector test=eq end=nil
			  (if count
			      ;; seq-type=simple-vector test=eq count=other end=nil
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eq count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=other end=nil 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=t test=eq count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=t test=eq count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seq-type=simple-vector from-end=nil test=eq count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=other end=nil 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=nil test=eq count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=nil test=eq count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=simple-vector test=eq count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eq count=nil key=other end=nil 
				  (|remove seq-type=simple-vector test=eq count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=simple-vector test=eq count=nil key=identity end=nil 
				  (|remove seq-type=simple-vector test=eq count=nil key=identity|
				   item sequence start (length sequence)))))
		      (if (or (eq test 'eql) (eq test #'eql))
			  ;; seq-type=simple-vector test=eql
			  (if end
			      ;; seq-type=simple-vector test=eql end=other
			      (if count
				  ;; seq-type=simple-vector test=eql count=other end=other
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=eql count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
					   item sequence start end count))
				      ;; seq-type=simple-vector from-end=nil test=eql count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
					   item sequence start end count)))
				  ;; seq-type=simple-vector test=eql count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=eql count=nil key=other end=other 
				      (|remove seq-type=simple-vector test=eql count=nil key=other|
				       item sequence start end key)
				      ;;       seq-type=simple-vector test=eql count=nil key=identity end=other 
				      (|remove seq-type=simple-vector test=eql count=nil key=identity|
				       item sequence start end)))
			      ;; seq-type=simple-vector test=eql end=nil
			      (if count
				  ;; seq-type=simple-vector test=eql count=other end=nil
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=eql count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
					   item sequence start (length sequence) count))
				      ;; seq-type=simple-vector from-end=nil test=eql count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
					   item sequence start (length sequence) count)))
				  ;; seq-type=simple-vector test=eql count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=eql count=nil key=other end=nil 
				      (|remove seq-type=simple-vector test=eql count=nil key=other|
				       item sequence start (length sequence) key)
				      ;;       seq-type=simple-vector test=eql count=nil key=identity end=nil 
				      (|remove seq-type=simple-vector test=eql count=nil key=identity|
				       item sequence start (length sequence)))))
			  ;; seq-type=simple-vector test=other
			  (if end
			      ;; seq-type=simple-vector test=other end=other
			      (if count
				  ;; seq-type=simple-vector test=other count=other end=other
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=other count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=identity|
					   item sequence test start end count))
				      ;; seq-type=simple-vector from-end=nil test=other count=other end=other
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=other end=other 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=identity end=other 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=identity|
					   item sequence test start end count)))
				  ;; seq-type=simple-vector test=other count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=other count=nil key=other end=other 
				      (|remove seq-type=simple-vector test=other count=nil key=other|
				       item sequence test start end key)
				      ;;       seq-type=simple-vector test=other count=nil key=identity end=other 
				      (|remove seq-type=simple-vector test=other count=nil key=identity|
				       item sequence test start end)))
			      ;; seq-type=simple-vector test=other end=nil
			      (if count
				  ;; seq-type=simple-vector test=other count=other end=nil
				  (if from-end
				      ;; seq-type=simple-vector from-end=t test=other count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=t test=other count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=t test=other count=other key=identity|
					   item sequence test start (length sequence) count))
				      ;; seq-type=simple-vector from-end=nil test=other count=other end=nil
				      (if key
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=other end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=simple-vector from-end=nil test=other count=other key=identity end=nil 
					  (|remove seq-type=simple-vector from-end=nil test=other count=other key=identity|
					   item sequence test start (length sequence) count)))
				  ;; seq-type=simple-vector test=other count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=simple-vector test=other count=nil key=other end=nil 
				      (|remove seq-type=simple-vector test=other count=nil key=other|
				       item sequence test start (length sequence) key)
				      ;;       seq-type=simple-vector test=other count=nil key=identity end=nil 
				      (|remove seq-type=simple-vector test=other count=nil key=identity|
				       item sequence test start (length sequence)))))))
		  (if test-not
		      ;; seq-type=simple-vector test-not=other
		      (if end
			  ;; seq-type=simple-vector test-not=other end=other
			  (if count
			      ;; seq-type=simple-vector test-not=other count=other end=other
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test-not=other count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start end count))
				  ;; seqr-type=simple-vector from-end=nil test-not=other count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start end count)))
			      ;; seq-type=simple-vector test-not=other count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test-not=other count=nil key=other end=other
				  (|remove seq-type=simple-vector test-not=other count=nil key=other|
				   item sequence test-not start end key)
				  ;;       seq-type=simple-vector test-not=other count=nil key=identity end=other 
				  (|remove seq-type=simple-vector test-not=other count=nil key=identity|
				   item sequence test-not start end)))
			  ;; seq-type=simple-vector test-not=other end=nil
			  (if count
			      ;; seq-type=simple-vector test-not=other count=other end=nil
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=t test-not=other count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count))
				  ;; seqr-type=simple-vector from-end=nil test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=nil test-not=other count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count)))
			      ;; seq-type=simple-vector test-not=other count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test-not=other count=nil key=other end=nil
				  (|remove seq-type=simple-vector test-not=other count=nil key=other|
				   item sequence test-not start (length sequence) key)
				  ;;       seq-type=simple-vector test-not=other count=nil key=identity end=nil! 
				  (|remove seq-type=simple-vector test-not=other count=nil key=identity|
				   item sequence test-not start (length sequence)))))
		      ;; seq-type=simple-vector test=eql
		      (if end
			  ;; seq-type=simple-vector test=eql end=other
			  (if count
			      ;; seq-type=simple-vector test=eql count=other end=other
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
				       item sequence start end count))
				  ;; seqr-type=simple-vector from-end=nil test=eql count=other end=other
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=other
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=other 
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=simple-vector test=eql count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eql count=nil key=other end=other
				  (|remove seq-type=simple-vector test=eql count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=simple-vector test=eql count=nil key=identity end=other 
				  (|remove seq-type=simple-vector test=eql count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=simple-vector test=eql end=nil
			  (if count
			      ;; seq-type=simple-vector test=eql count=other end=nil
			      (if from-end
				  ;; seq-type=simple-vector from-end=t test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=t test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=t test=eql count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seqr-type=simple-vector from-end=nil test=eql count=other end=nil
				  (if key
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=other end=nil
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=simple-vector from-end=nil test=eql count=other key=identity end=nil 
				      (|remove seq-type=simple-vector from-end=nil test=eql count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=simple-vector test=eql count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=simple-vector test=eql count=nil key=other end=nil
				  (|remove seq-type=simple-vector test=eql count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=simple-vector test=eql count=nil key=identity end=nil! 
				  (|remove seq-type=simple-vector test=eql count=nil key=identity|
				   item sequence start (length sequence)))))))
	      ;; seq-type=general-vector
	      (if test
		  ;; seq-type=general-vector test=given
		  (if (or (eq test 'eq) (eq test #'eq))
		      ;; seq-type=general-vector test=eq
		      (if end
			  ;; seq-type=general-vector test=eq end=other
			  (if count
			      ;; seq-type=general-vector test=eq count=other end=other
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eq count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=other end=other 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=identity|
				       item sequence start end count))
				  ;; seq-type=general-vector from-end=nil test=eq count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=other end=other 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=general-vector test=eq count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eq count=nil key=other end=other 
				  (|remove seq-type=general-vector test=eq count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=general-vector test=eq count=nil key=identity end=other 
				  (|remove seq-type=general-vector test=eq count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=general-vector test=eq end=nil
			  (if count
			      ;; seq-type=general-vector test=eq count=other end=nil
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eq count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=other end=nil 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=t test=eq count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=t test=eq count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seq-type=general-vector from-end=nil test=eq count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=other end=nil 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=nil test=eq count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=nil test=eq count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=general-vector test=eq count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eq count=nil key=other end=nil 
				  (|remove seq-type=general-vector test=eq count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=general-vector test=eq count=nil key=identity end=nil 
				  (|remove seq-type=general-vector test=eq count=nil key=identity|
				   item sequence start (length sequence)))))
		      (if (or (eq test 'eql) (eq test #'eql))
			  ;; seq-type=general-vector test=eql
			  (if end
			      ;; seq-type=general-vector test=eql end=other
			      (if count
				  ;; seq-type=general-vector test=eql count=other end=other
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=eql count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
					   item sequence start end count))
				      ;; seq-type=general-vector from-end=nil test=eql count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
					   item sequence start end count key)
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
					   item sequence start end count)))
				  ;; seq-type=general-vector test=eql count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=eql count=nil key=other end=other 
				      (|remove seq-type=general-vector test=eql count=nil key=other|
				       item sequence start end key)
				      ;;       seq-type=general-vector test=eql count=nil key=identity end=other 
				      (|remove seq-type=general-vector test=eql count=nil key=identity|
				       item sequence start end)))
			      ;; seq-type=general-vector test=eql end=nil
			      (if count
				  ;; seq-type=general-vector test=eql count=other end=nil
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=eql count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
					   item sequence start (length sequence) count))
				      ;; seq-type=general-vector from-end=nil test=eql count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
					   item sequence start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
					   item sequence start (length sequence) count)))
				  ;; seq-type=general-vector test=eql count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=eql count=nil key=other end=nil 
				      (|remove seq-type=general-vector test=eql count=nil key=other|
				       item sequence start (length sequence) key)
				      ;;       seq-type=general-vector test=eql count=nil key=identity end=nil 
				      (|remove seq-type=general-vector test=eql count=nil key=identity|
				       item sequence start (length sequence)))))
			  ;; seq-type=general-vector test=other
			  (if end
			      ;; seq-type=general-vector test=other end=other
			      (if count
				  ;; seq-type=general-vector test=other count=other end=other
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=other count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=t test=other count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=general-vector from-end=t test=other count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=identity|
					   item sequence test start end count))
				      ;; seq-type=general-vector from-end=nil test=other count=other end=other
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=other end=other 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=other|
					   item sequence test start end count key)
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=identity end=other 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=identity|
					   item sequence test start end count)))
				  ;; seq-type=general-vector test=other count=nil end=other
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=other count=nil key=other end=other 
				      (|remove seq-type=general-vector test=other count=nil key=other|
				       item sequence test start end key)
				      ;;       seq-type=general-vector test=other count=nil key=identity end=other 
				      (|remove seq-type=general-vector test=other count=nil key=identity|
				       item sequence test start end)))
			      ;; seq-type=general-vector test=other end=nil
			      (if count
				  ;; seq-type=general-vector test=other count=other end=nil
				  (if from-end
				      ;; seq-type=general-vector from-end=t test=other count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=t test=other count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=t test=other count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=t test=other count=other key=identity|
					   item sequence test start (length sequence) count))
				      ;; seq-type=general-vector from-end=nil test=other count=other end=nil
				      (if key
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=other end=nil 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=other|
					   item sequence test start (length sequence) count key)
					  ;;       seq-type=general-vector from-end=nil test=other count=other key=identity end=nil 
					  (|remove seq-type=general-vector from-end=nil test=other count=other key=identity|
					   item sequence test start (length sequence) count)))
				  ;; seq-type=general-vector test=other count=nil end=nil
				  ;; no need to test from-end
				  (if key
				      ;;       seq-type=general-vector test=other count=nil key=other end=nil 
				      (|remove seq-type=general-vector test=other count=nil key=other|
				       item sequence test start (length sequence) key)
				      ;;       seq-type=general-vector test=other count=nil key=identity end=nil 
				      (|remove seq-type=general-vector test=other count=nil key=identity|
				       item sequence test start (length sequence)))))))
		  (if test-not
		      ;; seq-type=general-vector test-not=other
		      (if end
			  ;; seq-type=general-vector test-not=other end=other
			  (if count
			      ;; seq-type=general-vector test-not=other count=other end=other
			      (if from-end
				  ;; seq-type=general-vector from-end=t test-not=other count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=other end=other
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start end count))
				  ;; seqr-type=general-vector from-end=nil test-not=other count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=other end=other
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start end count key)
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start end count)))
			      ;; seq-type=general-vector test-not=other count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test-not=other count=nil key=other end=other
				  (|remove seq-type=general-vector test-not=other count=nil key=other|
				   item sequence test-not start end key)
				  ;;       seq-type=general-vector test-not=other count=nil key=identity end=other 
				  (|remove seq-type=general-vector test-not=other count=nil key=identity|
				   item sequence test-not start end)))
			  ;; seq-type=general-vector test-not=other end=nil
			  (if count
			      ;; seq-type=general-vector test-not=other count=other end=nil
			      (if from-end
				  ;; seq-type=general-vector from-end=t test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=t test-not=other count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=t test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count))
				  ;; seqr-type=general-vector from-end=nil test-not=other count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=other|
				       item sequence test-not start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=nil test-not=other count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=nil test-not=other count=other key=identity|
				       item sequence test-not start (length sequence) count)))
			      ;; seq-type=general-vector test-not=other count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test-not=other count=nil key=other end=nil
				  (|remove seq-type=general-vector test-not=other count=nil key=other|
				   item sequence test-not start (length sequence) key)
				  ;;       seq-type=general-vector test-not=other count=nil key=identity end=nil! 
				  (|remove seq-type=general-vector test-not=other count=nil key=identity|
				   item sequence test-not start (length sequence)))))
		      ;; seq-type=general-vector test=eql
		      (if end
			  ;; seq-type=general-vector test=eql end=other
			  (if count
			      ;; seq-type=general-vector test=eql count=other end=other
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eql count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=other
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
				       item sequence start end count))
				  ;; seqr-type=general-vector from-end=nil test=eql count=other end=other
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=other
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
				       item sequence start end count key)
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=other 
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
				       item sequence start end count)))
			      ;; seq-type=general-vector test=eql count=nil end=other
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eql count=nil key=other end=other
				  (|remove seq-type=general-vector test=eql count=nil key=other|
				   item sequence start end key)
				  ;;       seq-type=general-vector test=eql count=nil key=identity end=other 
				  (|remove seq-type=general-vector test=eql count=nil key=identity|
				   item sequence start end)))
			  ;; seq-type=general-vector test=eql end=nil
			  (if count
			      ;; seq-type=general-vector test=eql count=other end=nil
			      (if from-end
				  ;; seq-type=general-vector from-end=t test=eql count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=t test=eql count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=t test=eql count=other key=identity|
				       item sequence start (length sequence) count))
				  ;; seqr-type=general-vector from-end=nil test=eql count=other end=nil
				  (if key
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=other end=nil
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=other|
				       item sequence start (length sequence) count key)
				      ;;       seq-type=general-vector from-end=nil test=eql count=other key=identity end=nil 
				      (|remove seq-type=general-vector from-end=nil test=eql count=other key=identity|
				       item sequence start (length sequence) count)))
			      ;; seq-type=general-vector test=eql count=nil end=nil
			      ;; no need to test from-end
			      (if key
				  ;;       seq-type=general-vector test=eql count=nil key=other end=nil
				  (|remove seq-type=general-vector test=eql count=nil key=other|
				   item sequence start (length sequence) key)
				  ;;       seq-type=general-vector test=eql count=nil key=identity end=nil! 
				  (|remove seq-type=general-vector test=eql count=nil key=identity|
				   item sequence start (length sequence)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function delete-if

(defun |delete-if seq-type=list end=nil count=nil key=identity|
    (test list start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (funcall test (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if seq-type=list end=nil count=nil key=other|
    (test list start key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  when (funcall test (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if seq-type=list end=other count=nil key=identity|
    (test list start end)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (funcall test (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if seq-type=list end=other count=nil key=other|
    (test list start end key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  when (funcall test (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if seq-type=list from-end=nil end=nil count=other key=identity|
    (test list count start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (funcall test (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if seq-type=list from-end=nil end=nil count=other key=other|
    (test list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  when (funcall test (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if seq-type=list from-end=nil end=other count=other key=identity|
    (test list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (funcall test (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if seq-type=list from-end=nil end=other count=other key=other|
    (test list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  when (funcall test (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if seq-type=list from-end=t end=nil count=other key=identity|
    (test list start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete-if seq-type=list from-end=t end=nil count=other key=other|
    (test list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete-if seq-type=list from-end=t end=other count=other key=identity|
    (test list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete-if seq-type=list from-end=t end=other count=other key=other|
    (test list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  unless (funcall test (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun delete-if (test sequence &key from-end (start 0) end count key)
  ;; FIXME test if it is a sequence at all.
  (if (listp sequence)
      ;; seq-type=list
      (if from-end
	  ;; seq-type=list from-end=t
	  (if end
	      ;; seq-type=list from-end=t end=other
	      (if count
		  ;; seq-type=list from-end=t end=other count=other
		  (if key
		      ;;          seq-type=list from-end=t end=other count=other key=other
		      (|delete-if seq-type=list from-end=t end=other count=other key=other|
		       test sequence start end count key)
		      ;;          seq-type=list from-end=t end=other count=other key=identity
		      (|delete-if seq-type=list from-end=t end=other count=other key=identity|
		       test sequence start end count))
		  ;; seq-type=list from-end=t end=other count=nil
		  (if key
		      ;;          seq-type=list end=other count=nil key=other
		      (|delete-if seq-type=list end=other count=nil key=other|
		       test sequence start end key)
		      ;;          seq-type=list end=other count=nil key=identity
		      (|delete-if seq-type=list end=other count=nil key=identity|
		       test sequence start end)))
	      ;; seq-type=list from-end=t end=nil
	      (if count
		  ;; seq-type=list from-end=t end=nil count=other
		  (if key
		      ;;          seq-type=list from-end=t end=nil count=other key=other
		      (|delete-if seq-type=list from-end=t end=nil count=other key=other|
		       test sequence start count key)
		      ;;          seq-type=list from-end=t end=nil count=other key=identity
		      (|delete-if seq-type=list from-end=t end=nil count=other key=identity|
		       test sequence start count))
		  ;; seq-type=list from-end=t end=nil count=nil
		  (if key
		      ;;          seq-type=list end=nil count=nil key=other
		      (|delete-if seq-type=list end=nil count=nil key=other|
		       test sequence start key)
		      ;;          seq-type=list end=nil count=nil key=identity
		      (|delete-if seq-type=list end=nil count=nil key=identity|
		       test sequence start))))
	  ;; seq-type=list from-end=nil
	  (if end
	      ;; seq-type=list from-end=nil end=other
	      (if count
		  ;; seq-type=list from-end=nil end=other count=other
		  (if key
		      ;;          seq-type=list from-end=nil end=other count=other key=other
		      (|delete-if seq-type=list from-end=nil end=other count=other key=other|
		       test sequence start end count key)
		      ;;          seq-type=list from-end=nil end=other count=other key=identity
		      (|delete-if seq-type=list from-end=nil end=other count=other key=identity|
		       test sequence start end count))
		  ;; seq-type=list from-end=nil end=other count=nil
		  (if key
		      ;;          seq-type=list end=other count=nil key=other
		      (|delete-if seq-type=list end=other count=nil key=other|
		       test sequence start end key)
		      ;;          seq-type=list end=other count=nil key=identity
		      (|delete-if seq-type=list end=other count=nil key=identity|
		       test sequence start end)))
	      ;; seq-type=list from-end=nil end=nil
	      (if count
		  ;; seq-type=list from-end=nil end=nil count=other
		  (if key
		      ;;          seq-type=list from-end=nil end=nil count=other key=other
		      (|delete-if seq-type=list from-end=nil end=nil count=other key=other|
		       test sequence start count key)
		      ;;          seq-type=list from-end=nil end=nil count=other key=identity
		      (|delete-if seq-type=list from-end=nil end=nil count=other key=identity|
		       test sequence start count))
		  ;; seq-type=list from-end=nil end=nil count=nil
		  (if key
		      ;;          seq-type=list end=nil count=nil key=other
		      (|delete-if seq-type=list end=nil count=nil key=other|
		       test sequence start key)
		      (|delete-if seq-type=list end=nil count=nil key=identity|
		       test sequence start)))))
      (if (simple-string-p sequence)
	  ;; seq-type=simple-string
	  ;; seq-type=simple-string test=given
	  ;; seq-type=simple-string
	  (if end
	      ;; seq-type=simple-string end=other
	      (if count
		  ;; seq-type=simple-string count=other end=other
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=other
		      (if key
			  ;;          seq-type=simple-string from-end=t count=other key=other end=other 
			  (|remove-if seq-type=simple-string from-end=t count=other key=other|
			   test sequence start end count key)
			  ;;          seq-type=simple-string from-end=t count=other key=identity end=other 
			  (|remove-if seq-type=simple-string from-end=t count=other key=identity|
			   test sequence start end count))
		      ;; seq-type=simple-string from-end=nil count=other end=other
		      (if key
			  ;;          seq-type=simple-string from-end=nil count=other key=other end=other 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=other|
			   test sequence start end count key)
			  ;;          seq-type=simple-string from-end=nil count=other key=identity end=other 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=identity|
			   test sequence start end count)))
		  ;; seq-type=simple-string count=nil end=other
		  ;; no need to test from-end
		  (if key
		      ;;          seq-type=simple-string count=nil key=other end=other 
		      (|remove-if seq-type=simple-string count=nil key=other|
		       test sequence start end key)
		      ;;          seq-type=simple-string count=nil key=identity end=other 
		      (|remove-if seq-type=simple-string count=nil key=identity|
		       test sequence start end)))
	      ;; seq-type=simple-string end=nil
	      (if count
		  ;; seq-type=simple-string count=other end=nil
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=nil
		      (if key
			  ;;          seq-type=simple-string from-end=t count=other key=other end=nil 
			  (|remove-if seq-type=simple-string from-end=t count=other key=other|
			   test sequence start (length sequence) count key)
			  ;;          seq-type=simple-string from-end=t count=other key=identity end=nil 
			  (|remove-if seq-type=simple-string from-end=t count=other key=identity|
			   test sequence start (length sequence) count))
		      ;; seq-type=simple-string from-end=nil count=other end=nil
		      (if key
			  ;;          seq-type=simple-string from-end=nil count=other key=other end=nil 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=other|
			   test sequence start (length sequence) count key)
			  ;;          seq-type=simple-string from-end=nil count=other key=identity end=nil 
			  (|remove-if seq-type=simple-string from-end=nil count=other key=identity|
			   test sequence start (length sequence) count)))
		  ;; seq-type=simple-string count=nil end=nil
		  ;; no need to test from-end
		  (if key
		      ;;          seq-type=simple-string count=nil key=other end=nil 
		      (|remove-if seq-type=simple-string count=nil key=other|
		       test sequence start (length sequence) key)
		      ;;          seq-type=simple-string count=nil key=identity end=nil 
		      (|remove-if seq-type=simple-string count=nil key=identity|
		       test sequence start (length sequence)))))
	  (if (simple-vector-p sequence)
	      ;; seq-type=simple-vector
	      ;; seq-type=simple-vector test=given
	      ;; seq-type=simple-vector
	      (if end
		  ;; seq-type=simple-vector end=other
		  (if count
		      ;; seq-type=simple-vector count=other end=other
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=other
			  (if key
			      ;;          seq-type=simple-vector from-end=t count=other key=other end=other 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=simple-vector from-end=t count=other key=identity end=other 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=identity|
			       test sequence start end count))
			  ;; seq-type=simple-vector from-end=nil count=other end=other
			  (if key
			      ;;          seq-type=simple-vector from-end=nil count=other key=other end=other 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=simple-vector from-end=nil count=other key=identity end=other 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=identity|
			       test sequence start end count)))
		      ;; seq-type=simple-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=simple-vector count=nil key=other end=other 
			  (|remove-if seq-type=simple-vector count=nil key=other|
			   test sequence start end key)
			  ;;          seq-type=simple-vector count=nil key=identity end=other 
			  (|remove-if seq-type=simple-vector count=nil key=identity|
			   test sequence start end)))
		  ;; seq-type=simple-vector end=nil
		  (if count
		      ;; seq-type=simple-vector count=other end=nil
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=nil
			  (if key
			      ;;          seq-type=simple-vector from-end=t count=other key=other end=nil 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=simple-vector from-end=t count=other key=identity end=nil 
			      (|remove-if seq-type=simple-vector from-end=t count=other key=identity|
			       test sequence start (length sequence) count))
			  ;; seq-type=simple-vector from-end=nil count=other end=nil
			  (if key
			      ;;          seq-type=simple-vector from-end=nil count=other key=other end=nil 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=simple-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if seq-type=simple-vector from-end=nil count=other key=identity|
			       test sequence start (length sequence) count)))
		      ;; seq-type=simple-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=simple-vector count=nil key=other end=nil 
			  (|remove-if seq-type=simple-vector count=nil key=other|
			   test sequence start (length sequence) key)
			  ;;          seq-type=simple-vector count=nil key=identity end=nil 
			  (|remove-if seq-type=simple-vector count=nil key=identity|
			   test sequence start (length sequence)))))
	      ;; seq-type=general-vector
	      (if end
		  ;; seq-type=general-vector end=other
		  (if count
		      ;; seq-type=general-vector count=other end=other
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=other
			  (if key
			      ;;          seq-type=general-vector from-end=t count=other key=other end=other 
			      (|remove-if seq-type=general-vector from-end=t count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=general-vector from-end=t count=other key=identity end=other 
			      (|remove-if seq-type=general-vector from-end=t count=other key=identity|
			       test sequence start end count))
			  ;; seq-type=general-vector from-end=nil count=other end=other
			  (if key
			      ;;          seq-type=general-vector from-end=nil count=other key=other end=other 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=other|
			       test sequence start end count key)
			      ;;          seq-type=general-vector from-end=nil count=other key=identity end=other 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=identity|
			       test sequence start end count)))
		      ;; seq-type=general-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=general-vector count=nil key=other end=other 
			  (|remove-if seq-type=general-vector count=nil key=other|
			   test sequence start end key)
			  ;;          seq-type=general-vector count=nil key=identity end=other 
			  (|remove-if seq-type=general-vector count=nil key=identity|
			   test sequence start end)))
		  ;; seq-type=general-vector end=nil
		  (if count
		      ;; seq-type=general-vector count=other end=nil
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=nil
			  (if key
			      ;;          seq-type=general-vector from-end=t count=other key=other end=nil 
			      (|remove-if seq-type=general-vector from-end=t count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=general-vector from-end=t count=other key=identity end=nil 
			      (|remove-if seq-type=general-vector from-end=t count=other key=identity|
			       test sequence start (length sequence) count))
			  ;; seq-type=general-vector from-end=nil count=other end=nil
			  (if key
			      ;;          seq-type=general-vector from-end=nil count=other key=other end=nil 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=other|
			       test sequence start (length sequence) count key)
			      ;;          seq-type=general-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if seq-type=general-vector from-end=nil count=other key=identity|
			       test sequence start (length sequence) count)))
		      ;; seq-type=general-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;          seq-type=general-vector count=nil key=other end=nil 
			  (|remove-if seq-type=general-vector count=nil key=other|
			   test sequence start (length sequence) key)
			  ;;          seq-type=general-vector count=nil key=identity end=nil 
			  (|remove-if seq-type=general-vector count=nil key=identity|
			   test sequence start (length sequence)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function delete-if-not

(defun |delete-if-not seq-type=list end=nil count=nil key=identity|
    (test-not list start)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  unless (funcall test-not (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if-not seq-type=list end=nil count=nil key=other|
    (test-not list start key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  unless (funcall test-not (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if-not seq-type=list end=other count=nil key=identity|
    (test-not list start end)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  unless (funcall test-not (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if-not seq-type=list end=other count=nil key=other|
    (test-not list start end key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop end-start)
	  unless (funcall test-not (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=nil end=nil count=other key=identity|
    (test-not list start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  unless (funcall test-not (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=nil end=nil count=other key=other|
    (test-not list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  unless (funcall test-not (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current)))
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=nil end=other count=other key=identity|
    (test-not list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  unless (funcall test-not (car current))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=nil end=other count=other key=other|
    (test-not list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Loop through the elements checking whether
    ;; they pass the test.
    ;; Loop invariant: trail points to the cell
    ;; imidiately preceding that pointed to by current.
    (loop until (null current)
	  until (zerop count)
	  until (zerop end-start)
	  unless (funcall test-not (funcall key (car current)))
	    do (setf current (cdr current))
	       (setf (cdr trail) current)
	       (decf count)
	  else
	    do (setf trail current)
	       (setf current (cdr current))
	  do (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=t end=nil count=other key=identity|
    (test-not list start count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=t end=nil count=other key=other|
    (test-not list start count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '()))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp)))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=t end=other count=other key=identity|
    (test-not list start end count)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not (car reversed-middle))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun |delete-if-not seq-type=list from-end=t end=other count=other key=other|
    (test-not list start end count key)
  (let* ((result (cons nil list))
	 (start-bis start)
	 (trail result)
	 (current list)
	 (reversed-middle '())
	 (end-start (- end start)))
    ;; First skip a prefix indicated by start
    (loop repeat start
	  until (null current)
	  do (setf trail current
		   current (cdr current))
	     (decf start-bis))
    ;; If we reached the end of the list before start-bis
    ;; became zero, then start is beyond the end of the
    ;; list.
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :start start
	     :sequence-length (- start start-bis)))
    ;; Now, reverse the sublist between start and end
    ;; and put the result in reversed-middle.
    (loop until (null current)
	  until (zerop end-start)
	  do (let ((temp (cdr current)))
	       (setf (cdr current) reversed-middle)
	       (setf reversed-middle current)
	       (setf current temp))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :end end
	     :sequence-length (+ start (- end end-start))
	     :in-sequence list))
    ;; The variable current now points to a tail
    ;; delimited by end, so we don't touch it.
    ;; Loop through the elements of reversed middle
    ;; skipping the ones to delete, and putting the others
    ;; back in the original order on the front of the tail
    ;; pointed to by current.
    (loop until (null reversed-middle)
	  until (zerop count)
	  when (funcall test-not (funcall key (car reversed-middle)))
	    do (let ((temp (cdr reversed-middle)))
		 (setf (cdr reversed-middle) current)
		 (setf current reversed-middle)
		 (setf reversed-middle temp))
	  else
	    do (setf reversed-middle (cdr reversed-middle))
	       (decf count))
    ;; There might be remaining elements on reversed-middle
    ;; that weren't deleted because we reached the count.
    ;; Copy them as before but without testing.
    (loop until (null reversed-middle)
	  do (let ((temp (cdr reversed-middle)))
	       (setf (cdr reversed-middle) current)
	       (setf current reversed-middle)
	       (setf reversed-middle temp)))
    ;; Finally, we are ready to connect the prefix pointed to
    ;; by trail to the remaining list pointed to by current.
    (setf (cdr trail) current)
    (cdr result)))

(defun delete-if-not (test-not sequence &key from-end (start 0) end count key)
  ;; FIXME test if it is a sequence at all.
  (if (listp sequence)
      ;; seq-type=list
      (if from-end
	  ;; seq-type=list from-end=t
	  ;; seq-type=list from-end=t
	  (if end
	      ;; seq-type=list from-end=t end=other
	      (if count
		  ;; seq-type=list from-end=t end=other count=other
		  (if key
		      ;;              seq-type=list from-end=t end=other count=other key=other
		      (|delete-if-not seq-type=list from-end=t end=other count=other key=other|
		       test-not sequence start end count key)
		      ;;              seq-type=list from-end=t end=other count=other key=identity
		      (|delete-if-not seq-type=list from-end=t end=other count=other key=identity|
		       test-not sequence start end count))
		  ;; seq-type=list from-end=t end=other count=nil
		  (if key
		      ;;              seq-type=list end=other count=nil key=other
		      (|delete-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      ;;              seq-type=list end=other count=nil key=identity
		      (|delete-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end)))
	      ;; seq-type=list from-end=t end=nil
	      (if count
		  ;; seq-type=list from-end=t end=nil count=other
		  (if key
		      ;;              seq-type=list from-end=t end=nil count=other key=other
		      (|delete-if-not seq-type=list from-end=t end=nil count=other key=other|
		       test-not sequence start count key)
		      ;;              seq-type=list from-end=t end=nil count=other key=identity
		      (|delete-if-not seq-type=list from-end=t end=nil count=other key=identity|
		       test-not sequence start count))
		  (if key
		      (|delete-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      (|delete-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end))))
	  ;; seq-type=list from-end=nil
	  ;; seq-type=list from-end=nil
	  (if end
	      ;; seq-type=list from-end=nil end=other
	      (if count
		  ;; seq-type=list from-end=nil end=other count=other
		  (if key
		      ;;              seq-type=list from-end=nil end=other count=other key=other
		      (|delete-if-not seq-type=list from-end=nil end=other count=other key=other|
		       test-not sequence start end count key)
		      ;;              seq-type=list from-end=nil end=other count=other key=identity
		      (|delete-if-not seq-type=list from-end=nil end=other count=other key=identity|
		       test-not sequence start end count))
		  ;; seq-type=list from-end=nil end=other count=nil
		  (if key
		      ;;              seq-type=list end=other count=nil key=other
		      (|delete-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      ;;              seq-type=list end=other count=nil key=identity
		      (|delete-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end)))
	      ;; seq-type=list from-end=nil end=nil
	      (if count
		  ;; seq-type=list from-end=nil end=nil count=other
		  (if key
		      ;;              seq-type=list from-end=nil end=nil count=other key=other
		      (|delete-if-not seq-type=list from-end=nil end=nil count=other key=other|
		       test-not sequence start count key)
		      ;;              seq-type=list from-end=nil end=nil count=other key=identity
		      (|delete-if-not seq-type=list from-end=nil end=nil count=other key=identity|
		       test-not sequence start count))
		  (if key
		      (|delete-if-not seq-type=list end=other count=nil key=other|
		       test-not sequence start end key)
		      (|delete-if-not seq-type=list end=other count=nil key=identity|
		       test-not sequence start end)))))
      (if (simple-string-p sequence)
	  ;; seq-type=simple-string
	  ;; seq-type=simple-string
	  (if end
	      ;; seq-type=simple-string end=other
	      (if count
		  ;; seq-type=simple-string count=other end=other
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=other
		      (if key
			  ;;              seq-type=simple-string from-end=t count=other key=other end=other
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=other|
			   test-not sequence start end count key)
			  ;;              seq-type=simple-string from-end=t count=other key=identity end=other 
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=identity|
			   test-not sequence start end count))
		      ;; seqr-type=simple-string from-end=nil count=other end=other
		      (if key
			  ;;              seq-type=simple-string from-end=nil count=other key=other end=other
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=other|
			   test-not sequence start end count key)
			  ;;              seq-type=simple-string from-end=nil count=other key=identity end=other 
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=identity|
			   test-not sequence start end count)))
		  ;; seq-type=simple-string count=nil end=other
		  ;; no need to test from-end
		  (if key
		      ;;              seq-type=simple-string count=nil key=other end=other
		      (|remove-if-not seq-type=simple-string count=nil key=other|
		       test-not sequence start end key)
		      ;;              seq-type=simple-string count=nil key=identity end=other 
		      (|remove-if-not seq-type=simple-string count=nil key=identity|
		       test-not sequence start end)))
	      ;; seq-type=simple-string end=nil
	      (if count
		  ;; seq-type=simple-string count=other end=nil
		  (if from-end
		      ;; seq-type=simple-string from-end=t count=other end=nil
		      (if key
			  ;;              seq-type=simple-string from-end=t count=other key=other end=nil
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=other|
			   test-not sequence start (length sequence) count key)
			  ;;              seq-type=simple-string from-end=t count=other key=identity end=nil 
			  (|remove-if-not seq-type=simple-string from-end=t count=other key=identity|
			   test-not sequence start (length sequence) count))
		      ;; seqr-type=simple-string from-end=nil count=other end=nil
		      (if key
			  ;;              seq-type=simple-string from-end=nil count=other key=other end=nil
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=other|
			   test-not sequence start (length sequence) count key)
			  ;;              seq-type=simple-string from-end=nil count=other key=identity end=nil 
			  (|remove-if-not seq-type=simple-string from-end=nil count=other key=identity|
			   test-not sequence start (length sequence) count)))
		  ;; seq-type=simple-string count=nil end=nil
		  ;; no need to test from-end
		  (if key
		      ;;              seq-type=simple-string count=nil key=other end=nil
		      (|remove-if-not seq-type=simple-string count=nil key=other|
		       test-not sequence start (length sequence) key)
		      ;;              seq-type=simple-string count=nil key=identity end=nil! 
		      (|remove-if-not seq-type=simple-string count=nil key=identity|
		       test-not sequence start (length sequence)))))
	  (if (simple-vector-p sequence)
	      ;; seq-type=simple-vector
	      ;; seq-type=simple-vector
	      (if end
		  ;; seq-type=simple-vector end=other
		  (if count
		      ;; seq-type=simple-vector count=other end=other
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=other
			  (if key
			      ;;              seq-type=simple-vector from-end=t count=other key=other end=other
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=simple-vector from-end=t count=other key=identity end=other 
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=identity|
			       test-not sequence start end count))
			  ;; seqr-type=simple-vector from-end=nil count=other end=other
			  (if key
			      ;;              seq-type=simple-vector from-end=nil count=other key=other end=other
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=simple-vector from-end=nil count=other key=identity end=other 
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=identity|
			       test-not sequence start end count)))
		      ;; seq-type=simple-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=simple-vector count=nil key=other end=other
			  (|remove-if-not seq-type=simple-vector count=nil key=other|
			   test-not sequence start end key)
			  ;;              seq-type=simple-vector count=nil key=identity end=other 
			  (|remove-if-not seq-type=simple-vector count=nil key=identity|
			   test-not sequence start end)))
		  ;; seq-type=simple-vector end=nil
		  (if count
		      ;; seq-type=simple-vector count=other end=nil
		      (if from-end
			  ;; seq-type=simple-vector from-end=t count=other end=nil
			  (if key
			      ;;              seq-type=simple-vector from-end=t count=other key=other end=nil
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=simple-vector from-end=t count=other key=identity end=nil 
			      (|remove-if-not seq-type=simple-vector from-end=t count=other key=identity|
			       test-not sequence start (length sequence) count))
			  ;; seqr-type=simple-vector from-end=nil count=other end=nil
			  (if key
			      ;;              seq-type=simple-vector from-end=nil count=other key=other end=nil
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=simple-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if-not seq-type=simple-vector from-end=nil count=other key=identity|
			       test-not sequence start (length sequence) count)))
		      ;; seq-type=simple-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=simple-vector count=nil key=other end=nil
			  (|remove-if-not seq-type=simple-vector count=nil key=other|
			   test-not sequence start (length sequence) key)
			  ;;              seq-type=simple-vector count=nil key=identity end=nil! 
			  (|remove-if-not seq-type=simple-vector count=nil key=identity|
			   test-not sequence start (length sequence)))))
	      ;; seq-type=general-vector
	      ;; seq-type=general-vector
	      (if end
		  ;; seq-type=general-vector end=other
		  (if count
		      ;; seq-type=general-vector count=other end=other
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=other
			  (if key
			      ;;              seq-type=general-vector from-end=t count=other key=other end=other
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=general-vector from-end=t count=other key=identity end=other 
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=identity|
			       test-not sequence start end count))
			  ;; seqr-type=general-vector from-end=nil count=other end=other
			  (if key
			      ;;              seq-type=general-vector from-end=nil count=other key=other end=other
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=other|
			       test-not sequence start end count key)
			      ;;              seq-type=general-vector from-end=nil count=other key=identity end=other 
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=identity|
			       test-not sequence start end count)))
		      ;; seq-type=general-vector count=nil end=other
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=general-vector count=nil key=other end=other
			  (|remove-if-not seq-type=general-vector count=nil key=other|
			   test-not sequence start end key)
			  ;;              seq-type=general-vector count=nil key=identity end=other 
			  (|remove-if-not seq-type=general-vector count=nil key=identity|
			   test-not sequence start end)))
		  ;; seq-type=general-vector end=nil
		  (if count
		      ;; seq-type=general-vector count=other end=nil
		      (if from-end
			  ;; seq-type=general-vector from-end=t count=other end=nil
			  (if key
			      ;;              seq-type=general-vector from-end=t count=other key=other end=nil
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=general-vector from-end=t count=other key=identity end=nil 
			      (|remove-if-not seq-type=general-vector from-end=t count=other key=identity|
			       test-not sequence start (length sequence) count))
			  ;; seqr-type=general-vector from-end=nil count=other end=nil
			  (if key
			      ;;              seq-type=general-vector from-end=nil count=other key=other end=nil
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=other|
			       test-not sequence start (length sequence) count key)
			      ;;              seq-type=general-vector from-end=nil count=other key=identity end=nil 
			      (|remove-if-not seq-type=general-vector from-end=nil count=other key=identity|
			       test-not sequence start (length sequence) count)))
		      ;; seq-type=general-vector count=nil end=nil
		      ;; no need to test from-end
		      (if key
			  ;;              seq-type=general-vector count=nil key=other end=nil
			  (|remove-if-not seq-type=general-vector count=nil key=other|
			   test-not sequence start (length sequence) key)
			  ;;              seq-type=general-vector count=nil key=identity end=nil! 
			  (|remove-if-not seq-type=general-vector count=nil key=identity|
			   test-not sequence start (length sequence)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function copy-seq

(defun copy-seq (sequence)
  (if (listp sequence)
      ;; It is safe to use for ... in here because loop uses endp to
      ;; test for the end, and that is exactly what we want for
      ;; copy-seq (as opposed to for copy-list).
      (loop for element in sequence
	    collect element)
      (let ((result (make-array (length sequence)
				:element-type (array-element-type sequence))))
	(loop for i from 0 below (length sequence)
	      do (setf (aref result i) (aref sequence i)))
	result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessor elt

(define-condition invalid-sequence-index (type-error)
  ((%in-sequence :initarg :in-sequence :reader in-sequence)))

(defun elt (sequence index)
  (unless (typep index 'unsigned-byte)
    (error 'invalid-sequence-index
	   :datum index
	   :expected-type '(integer 0)
	   :in-sequence sequence))
  (if (listp sequence)
      (loop with list = sequence
	    with save-index = index
	    until (endp list)
	    until (zerop index)
	    do (setf list (cdr list))
	       (decf save-index)
	    finally (if (null list)
			(error 'invalid-sequence-index
			       :datum index
			       :expected-type `(integer 0 ,(- index save-index))
			       :in-sequence sequence)
			(return (car list))))
      (if (>= index (length sequence))
	  (error 'invalid-sequence-index
		 :datum index
		 :expected-type `(integer 0 ,(1- (length sequence)))
		 :in-sequence sequence)
	  (aref sequence index))))

(defun (setf elt) (new-object sequence index)
  (unless (typep index 'unsigned-byte)
    (error 'invalid-sequence-index
	   :datum index
	   :expected-type '(integer 0)
	   :in-sequence sequence))
  (if (listp sequence)
      (loop with list = sequence
	    with save-index = index
	    until (endp list)
	    until (zerop index)
	    do (setf list (cdr list))
	       (decf save-index)
	    finally (if (null list)
			(error 'invalid-sequence-index
			       :datum index
			       :expected-type `(integer 0 ,(- index save-index))
			       :in-sequence sequence)
			(setf (car list) new-object)))
      (if (>= index (length sequence))
	  (error 'invalid-sequence-index
		 :datum index
		 :expected-type `(integer 0 ,(1- (length sequence)))
		 :in-sequence sequence)
	  (setf (aref sequence index) new-object)))
  new-object)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function count

(defun |count seq-type=general-vector from-end=nil key=identity test=eql|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eql item (aref vector i))))

(defun |count seq-type=general-vector from-end=nil key=identity test=eq|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eq item (aref vector i))))

(defun |count seq-type=general-vector from-end=nil key=identity test=other|
    (item vector start end test)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall test item (aref vector i))))

(defun |count seq-type=general-vector from-end=nil key=other test=eql|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eql item (funcall key (aref vector i)))))

(defun |count seq-type=general-vector from-end=nil key=other test=eq|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eq item (funcall key (aref vector i)))))

(defun |count seq-type=general-vector from-end=nil key=other test=other|
    (item vector start end key test)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall test item (funcall key (aref vector i)))))

(defun |count seq-type=general-vector from-end=t key=identity test=eql|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eql item (aref vector i))))

(defun |count seq-type=general-vector from-end=t key=identity test=eq|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eq item (aref vector i))))

(defun |count seq-type=general-vector from-end=t key=identity test=other|
    (item vector start end test)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall test item (aref vector i))))

(defun |count seq-type=general-vector from-end=t key=other test=eql|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eql item (funcall key (aref vector i)))))

(defun |count seq-type=general-vector from-end=t key=other test=eq|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eq item (funcall key (aref vector i)))))

(defun |count seq-type=general-vector from-end=t key=other test=other|
    (item vector start end key test)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall test item (funcall key (aref vector i)))))

(defun |count seq-type=general-vector from-end=nil key=identity test-not=eql|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eql item (aref vector i)))))

(defun |count seq-type=general-vector from-end=nil key=identity test-not=eq|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eq item (aref vector i)))))

(defun |count seq-type=general-vector from-end=nil key=identity test-not=other|
    (item vector start end test-not)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall test-not item (aref vector i)))))

(defun |count seq-type=general-vector from-end=nil key=other test-not=eql|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eql item (funcall key (aref vector i))))))

(defun |count seq-type=general-vector from-end=nil key=other test-not=eq|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eq item (funcall key (aref vector i))))))

(defun |count seq-type=general-vector from-end=nil key=other test-not=other|
    (item vector start end key test-not)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall test-not item (funcall key (aref vector i))))))

(defun |count seq-type=general-vector from-end=t key=identity test-not=eql|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eql item (aref vector i)))))

(defun |count seq-type=general-vector from-end=t key=identity test-not=eq|
    (item vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eq item (aref vector i)))))

(defun |count seq-type=general-vector from-end=t key=identity test-not=other|
    (item vector start end test-not)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall test-not item (aref vector i)))))

(defun |count seq-type=general-vector from-end=t key=other test-not=eql|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eql item (funcall key (aref vector i))))))

(defun |count seq-type=general-vector from-end=t key=other test-not=eq|
    (item vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eq item (funcall key (aref vector i))))))

(defun |count seq-type=general-vector from-end=t key=other test-not=other|
    (item vector start end key test-not)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall test-not item (funcall key (aref vector i))))))

(defun |count seq-type=simple-vector from-end=nil key=identity test=eql|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eql item (svref vector i))))

(defun |count seq-type=simple-vector from-end=nil key=identity test=eq|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eq item (svref vector i))))

(defun |count seq-type=simple-vector from-end=nil key=identity test=other|
    (item vector start end test)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall test item (svref vector i))))

(defun |count seq-type=simple-vector from-end=nil key=other test=eql|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eql item (funcall key (svref vector i)))))

(defun |count seq-type=simple-vector from-end=nil key=other test=eq|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eq item (funcall key (svref vector i)))))

(defun |count seq-type=simple-vector from-end=nil key=other test=other|
    (item vector start end key test)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall test item (funcall key (svref vector i)))))

(defun |count seq-type=simple-vector from-end=t key=identity test=eql|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eql item (svref vector i))))

(defun |count seq-type=simple-vector from-end=t key=identity test=eq|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eq item (svref vector i))))

(defun |count seq-type=simple-vector from-end=t key=identity test=other|
    (item vector start end test)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall test item (svref vector i))))

(defun |count seq-type=simple-vector from-end=t key=other test=eql|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eql item (funcall key (svref vector i)))))

(defun |count seq-type=simple-vector from-end=t key=other test=eq|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eq item (funcall key (svref vector i)))))

(defun |count seq-type=simple-vector from-end=t key=other test=other|
    (item vector start end key test)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall test item (funcall key (svref vector i)))))

(defun |count seq-type=simple-vector from-end=nil key=identity test-not=eql|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eql item (svref vector i)))))

(defun |count seq-type=simple-vector from-end=nil key=identity test-not=eq|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eq item (svref vector i)))))

(defun |count seq-type=simple-vector from-end=nil key=identity test-not=other|
    (item vector start end test-not)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall test-not item (svref vector i)))))

(defun |count seq-type=simple-vector from-end=nil key=other test-not=eql|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eql item (funcall key (svref vector i))))))

(defun |count seq-type=simple-vector from-end=nil key=other test-not=eq|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eq item (funcall key (svref vector i))))))

(defun |count seq-type=simple-vector from-end=nil key=other test-not=other|
    (item vector start end key test-not)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall test-not item (funcall key (svref vector i))))))

(defun |count seq-type=simple-vector from-end=t key=identity test-not=eql|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eql item (svref vector i)))))

(defun |count seq-type=simple-vector from-end=t key=identity test-not=eq|
    (item vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eq item (svref vector i)))))

(defun |count seq-type=simple-vector from-end=t key=identity test-not=other|
    (item vector start end test-not)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall test-not item (svref vector i)))))

(defun |count seq-type=simple-vector from-end=t key=other test-not=eql|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eql item (funcall key (svref vector i))))))

(defun |count seq-type=simple-vector from-end=t key=other test-not=eq|
    (item vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eq item (funcall key (svref vector i))))))

(defun |count seq-type=simple-vector from-end=t key=other test-not=other|
    (item vector start end key test-not)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall test-not item (funcall key (svref vector i))))))

(defun |count seq-type=simple-string from-end=nil key=identity test=eql|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eql item (schar vector i))))

(defun |count seq-type=simple-string from-end=nil key=identity test=eq|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eq item (schar vector i))))

(defun |count seq-type=simple-string from-end=nil key=identity test=other|
    (item vector start end test)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall test item (schar vector i))))

(defun |count seq-type=simple-string from-end=nil key=other test=eql|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eql item (funcall key (schar vector i)))))

(defun |count seq-type=simple-string from-end=nil key=other test=eq|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (eq item (funcall key (schar vector i)))))

(defun |count seq-type=simple-string from-end=nil key=other test=other|
    (item vector start end key test)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall test item (funcall key (schar vector i)))))

(defun |count seq-type=simple-string from-end=t key=identity test=eql|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eql item (schar vector i))))

(defun |count seq-type=simple-string from-end=t key=identity test=eq|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eq item (schar vector i))))

(defun |count seq-type=simple-string from-end=t key=identity test=other|
    (item vector start end test)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall test item (schar vector i))))

(defun |count seq-type=simple-string from-end=t key=other test=eql|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eql item (funcall key (schar vector i)))))

(defun |count seq-type=simple-string from-end=t key=other test=eq|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (eq item (funcall key (schar vector i)))))

(defun |count seq-type=simple-string from-end=t key=other test=other|
    (item vector start end key test)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall test item (funcall key (schar vector i)))))

(defun |count seq-type=simple-string from-end=nil key=identity test-not=eql|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eql item (schar vector i)))))

(defun |count seq-type=simple-string from-end=nil key=identity test-not=eq|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eq item (schar vector i)))))

(defun |count seq-type=simple-string from-end=nil key=identity test-not=other|
    (item vector start end test-not)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall test-not item (schar vector i)))))

(defun |count seq-type=simple-string from-end=nil key=other test-not=eql|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eql item (funcall key (schar vector i))))))

(defun |count seq-type=simple-string from-end=nil key=other test-not=eq|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (eq item (funcall key (schar vector i))))))

(defun |count seq-type=simple-string from-end=nil key=other test-not=other|
    (item vector start end key test-not)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall test-not item (funcall key (schar vector i))))))

(defun |count seq-type=simple-string from-end=t key=identity test-not=eql|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eql item (schar vector i)))))

(defun |count seq-type=simple-string from-end=t key=identity test-not=eq|
    (item vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eq item (schar vector i)))))

(defun |count seq-type=simple-string from-end=t key=identity test-not=other|
    (item vector start end test-not)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall test-not item (schar vector i)))))

(defun |count seq-type=simple-string from-end=t key=other test-not=eql|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eql item (funcall key (schar vector i))))))

(defun |count seq-type=simple-string from-end=t key=other test-not=eq|
    (item vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (eq item (funcall key (schar vector i))))))

(defun |count seq-type=simple-string from-end=t key=other test-not=other|
    (item vector start end key test-not)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall test-not item (funcall key (schar vector i))))))

(defun |count seq-type=list from-end=nil end=nil key=identity test=eql|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eql item element))))

(defun |count seq-type=list from-end=nil end=nil key=identity test=eq|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eq item element))))

(defun |count seq-type=list from-end=nil end=nil key=identity test=other|
    (item list start test)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (funcall test item element))))

(defun |count seq-type=list from-end=nil end=nil key=other test=eql|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eql item (funcall key element)))))

(defun |count seq-type=list from-end=nil end=nil key=other test=eq|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eq item (funcall key element)))))

(defun |count seq-type=list from-end=nil end=nil key=other test=other|
    (item list start key test)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (funcall test item (funcall key element)))))

(defun |count seq-type=list from-end=nil end=other key=identity test=eql|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eql item element)
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=identity test=eq|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eq item element)
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=identity test=other|
    (item list start end test)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (funcall test item element)
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=other test=eql|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eql item (funcall key element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=other test=eq|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eq item (funcall key element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=other test=other|
    (item list start end key test)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (funcall test item (funcall key element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=nil key=identity test-not=eql|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eql item element)))))

(defun |count seq-type=list from-end=nil end=nil key=identity test-not=eq|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eq item element)))))

(defun |count seq-type=list from-end=nil end=nil key=identity test-not=other|
    (item list start test-not)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (funcall test-not item element)))))

(defun |count seq-type=list from-end=nil end=nil key=other test-not=eql|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eql item (funcall key element))))))

(defun |count seq-type=list from-end=nil end=nil key=other test-not=eq|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eq item (funcall key element))))))

(defun |count seq-type=list from-end=nil end=nil key=other test-not=other|
    (item list start key test-not)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (funcall test-not item (funcall key element))))))

(defun |count seq-type=list from-end=nil end=other key=identity test-not=eql|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eql item element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=identity test-not=eq|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eq item element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=identity test-not=other|
    (item list start end test-not)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (funcall test-not item element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=other test-not=eql|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eql item (funcall key element)))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=other test-not=eq|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eq item (funcall key element)))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=nil end=other key=other test-not=other|
    (item list start end key test-not)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (funcall test-not item (funcall key element)))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=nil key=identity test=eql|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eql item element))))

(defun |count seq-type=list from-end=t end=nil key=identity test=eq|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eq item element))))

(defun |count seq-type=list from-end=t end=nil key=identity test=other|
    (item list start test)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((vector (coerce remaining 'vector)))
      (|count seq-type=simple-vector from-end=t key=identity test=other|
       item vector 0 (length vector) test))))

(defun |count seq-type=list from-end=t end=nil key=other test=eql|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eql item (funcall key element)))))

(defun |count seq-type=list from-end=t end=nil key=other test=eq|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (eq item (funcall key element)))))

(defun |count seq-type=list from-end=t end=nil key=other test=other|
    (item list start key test)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((vector (coerce remaining 'vector)))
      (|count seq-type=simple-vector from-end=t key=other test=other|
       item vector 0 (length vector) key test))))

(defun |count seq-type=list from-end=t end=other key=identity test=eql|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eql item element)
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=identity test=eq|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eq item element)
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=identity test=other|
    (item list start end test)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((length (length remaining)))
      (when (> (- end start) length)
	(error 'invalid-end-index
	       :datum end
	       :expected-type `(integer 0 ,(+ start length))
	       :in-sequence list
	       :in-sequence list))
      (let ((vector (make-array (- end start))))
	(loop for i from 0 below (- end start)
	      for element in remaining
	      do (setf (aref vector i) element))
	(|count seq-type=simple-vector from-end=t key=identity test=other|
	 item vector 0 (- end start) test)))))

(defun |count seq-type=list from-end=t end=other key=other test=eql|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eql item (funcall key element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=other test=eq|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (eq item (funcall key element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=other test=other|
    (item list start end key test)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((length (length remaining)))
      (when (> (- end start) length)
	(error 'invalid-end-index
	       :datum end
	       :expected-type `(integer 0 ,(+ start length))
	       :in-sequence list
	       :in-sequence list))
      (let ((vector (make-array (- end start))))
	(loop for i from 0 below (- end start)
	      for element in remaining
	      do (setf (aref vector i) element))
	(|count seq-type=simple-vector from-end=t key=other test=other|
	 item vector 0 (- end start) key test)))))

(defun |count seq-type=list from-end=t end=nil key=identity test-not=eql|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eql item element)))))

(defun |count seq-type=list from-end=t end=nil key=identity test-not=eq|
    (item list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eq item element)))))

(defun |count seq-type=list from-end=t end=nil key=identity test-not=other|
    (item list start test-not)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((vector (coerce remaining 'vector)))
      (|count seq-type=simple-vector from-end=t key=identity test-not=other|
       item vector 0 (length vector) test-not))))

(defun |count seq-type=list from-end=t end=nil key=other test-not=eql|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eql item (funcall key element))))))

(defun |count seq-type=list from-end=t end=nil key=other test-not=eq|
    (item list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (eq item (funcall key element))))))

(defun |count seq-type=list from-end=t end=nil key=other test-not=other|
    (item list start key test-not)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((vector (coerce remaining 'vector)))
      (|count seq-type=simple-vector from-end=t key=other test-not=other|
       item vector 0 (length vector) key test-not))))

(defun |count seq-type=list from-end=t end=other key=identity test-not=eql|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eql item element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=identity test-not=eq|
    (item list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eq item element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=identity test-not=other|
    (item list start end test-not)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((length (length remaining)))
      (when (> (- end start) length)
	(error 'invalid-end-index
	       :datum end
	       :expected-type `(integer 0 ,(+ start length))
	       :in-sequence list))
      (let ((vector (make-array (- end start))))
	(loop for i from 0 below (- end start)
	      for element in remaining
	      do (setf (aref vector i) element))
	(|count seq-type=simple-vector from-end=t key=identity test-not=other|
	 item vector 0 (- end start) test-not)))))

(defun |count seq-type=list from-end=t end=other key=other test-not=eql|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eql item (funcall key element)))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=other test-not=eq|
    (item list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (eq item (funcall key element)))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count seq-type=list from-end=t end=other key=other test-not=other|
    (item list start end key test-not)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (let ((length (length remaining)))
      (when (> (- end start) length)
	(error 'invalid-end-index
	       :datum end
	       :expected-type `(integer 0 ,(+ start length))
	       :in-sequence list))
      (let ((vector (make-array (- end start))))
	(loop for i from 0 below (- end start)
	      for element in remaining
	      do (setf (aref vector i) element))
	(|count seq-type=simple-vector from-end=t key=other test-not=other|
	 item vector 0 (- end start) key test-not)))))

(defun count (item sequence &key from-end (start 0) end key test test-not)
  (if (listp sequence)
      (if from-end
	  (if end
	      (if key
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=t end=other key=other test=eql|
			   item sequence start end key)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=t end=other key=other test=eq|
			       item sequence start end key)
			      (|count seq-type=list from-end=t end=other key=other test=other|
			       item sequence start end key test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=t end=other key=other test-not=eql|
			       item sequence start end key)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=t end=other key=other test-not=eq|
				   item sequence start end key)
				  (|count seq-type=list from-end=t end=other key=other test-not=other|
				   item sequence start end key test-not)))
			  (|count seq-type=list from-end=t end=other key=other test=eql|
			   item sequence start end key)))
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=t end=other key=identity test=eql|
			   item sequence start end)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=t end=other key=identity test=eq|
			       item sequence start end)
			      (|count seq-type=list from-end=t end=other key=identity test=other|
			       item sequence start end test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=t end=other key=identity test-not=eql|
			       item sequence start end)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=t end=other key=identity test-not=eq|
				   item sequence start end)
				  (|count seq-type=list from-end=t end=other key=identity test-not=other|
				   item sequence start end test-not)))
			  (|count seq-type=list from-end=t end=other key=identity test-not=other|
			   item sequence start end test-not))))
	      (if key
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=t end=nil key=other test=eql|
			   item sequence start key)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=t end=nil key=other test=eq|
			       item sequence start key)
			      (|count seq-type=list from-end=t end=nil key=other test=other|
			       item sequence start key test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=t end=nil key=other test-not=eql|
			       item sequence start key)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=t end=nil key=other test-not=eq|
				   item sequence start key)
				  (|count seq-type=list from-end=t end=nil key=other test-not=other|
				   item sequence start key test-not)))
			  (|count seq-type=list from-end=t end=nil key=other test=eql|
			   item sequence start key)))
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=t end=nil key=identity test=eql|
			   item sequence start)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=t end=nil key=identity test=eq|
			       item sequence start)
			      (|count seq-type=list from-end=t end=nil key=identity test=other|
			       item sequence start test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=t end=nil key=identity test-not=eql|
			       item sequence start)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=t end=nil key=identity test-not=eq|
				   item sequence start)
				  (|count seq-type=list from-end=t end=nil key=identity test-not=other|
				   item sequence start test-not)))
			  (|count seq-type=list from-end=t end=nil key=identity test-not=other|
			   item sequence start test-not)))))
	  (if end
	      (if key
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=nil end=other key=other test=eql|
			   item sequence start end key)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=nil end=other key=other test=eq|
			       item sequence start end key)
			      (|count seq-type=list from-end=nil end=other key=other test=other|
			       item sequence start end key test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=nil end=other key=other test-not=eql|
			       item sequence start end key)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=nil end=other key=other test-not=eq|
				   item sequence start end key)
				  (|count seq-type=list from-end=nil end=other key=other test-not=other|
				   item sequence start end key test-not)))
			  (|count seq-type=list from-end=nil end=other key=other test=eql|
			   item sequence start end key)))
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=nil end=other key=identity test=eql|
			   item sequence start end)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=nil end=other key=identity test=eq|
			       item sequence start end)
			      (|count seq-type=list from-end=nil end=other key=identity test=other|
			       item sequence start end test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=nil end=other key=identity test-not=eql|
			       item sequence start end)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=nil end=other key=identity test-not=eq|
				   item sequence start end)
				  (|count seq-type=list from-end=nil end=other key=identity test-not=other|
				   item sequence start end test-not)))
			  (|count seq-type=list from-end=nil end=other key=identity test=eql|
			   item sequence start end))))
	      (if key
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=nil end=nil key=other test=eql|
			   item sequence start key)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=nil end=nil key=other test=eq|
			       item sequence start key)
			      (|count seq-type=list from-end=nil end=nil key=other test=other|
			       item sequence start key test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=nil end=nil key=other test-not=eql|
			       item sequence start key)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=nil end=nil key=other test-not=eq|
				   item sequence start key)
				  (|count seq-type=list from-end=nil end=nil key=other test-not=other|
				   item sequence start key test-not)))
			  (|count seq-type=list from-end=nil end=nil key=other test=eql|
			   item sequence start key)))
		  (if test
		      (if (or (eq test #'eql) (eq test 'eql))
			  (|count seq-type=list from-end=nil end=nil key=identity test=eql|
			   item sequence start)
			  (if (or (eq test #'eq) (eq test 'eq))
			      (|count seq-type=list from-end=nil end=nil key=identity test=eq|
			       item sequence start)
			      (|count seq-type=list from-end=nil end=nil key=identity test=other|
			       item sequence start test)))
		      (if test-not
			  (if (or (eq test-not #'eql) (eq test-not 'eql))
			      (|count seq-type=list from-end=nil end=nil key=identity test-not=eql|
			       item sequence start)
			      (if (or (eq test-not #'eq) (eq test-not 'eq))
				  (|count seq-type=list from-end=nil end=nil key=identity test-not=eq|
				   item sequence start)
				  (|count seq-type=list from-end=nil end=nil key=identity test-not=other|
				   item sequence start test-not)))
			  (|count seq-type=list from-end=nil end=nil key=identity test=eql|
			   item sequence start))))))
      (if (simple-string-p sequence)
          (if from-end
              (if key
                  (if test
                      (if (or (eq test #'eql) (eq test 'eql))
                          (|count seq-type=simple-string from-end=t key=other test=eql|
			   item sequence start (or end (length sequence)) key)
                          (if (or (eq test #'eq) (eq test 'eq))
                              (|count seq-type=simple-string from-end=t key=other test=eq|
			       item sequence start (or end (length sequence)) key)
                              (|count seq-type=simple-string from-end=t key=other test=other|
			       item sequence start (or end (length sequence)) key test)))
                      (if test-not
                          (if (or (eq test-not #'eql) (eq test-not 'eql))
                              (|count seq-type=simple-string from-end=t key=other test-not=eql|
			       item sequence start (or end (length sequence)) key)
                              (if (or (eq test-not #'eq) (eq test-not 'eq))
                                  (|count seq-type=simple-string from-end=t key=other test-not=eq|
				   item sequence start (or end (length sequence)) key)
                                  (|count seq-type=simple-string from-end=t key=other test-not=other|
				   item sequence start (or end (length sequence)) key test-not)))
                          (|count seq-type=simple-string from-end=t key=other test=eql|
			   item sequence start (or end (length sequence)) key)))
                  (if test
                      (if (or (eq test #'eql) (eq test 'eql))
                          (|count seq-type=simple-string from-end=t key=identity test=eql|
			   item sequence start (or end (length sequence)))
                          (if (or (eq test #'eq) (eq test 'eq))
                              (|count seq-type=simple-string from-end=t key=identity test=eq|
			       item sequence start (or end (length sequence)))
                              (|count seq-type=simple-string from-end=t key=identity test=other|
			       item sequence start (or end (length sequence)) test)))
                      (if test-not
                          (if (or (eq test-not #'eql) (eq test-not 'eql))
                              (|count seq-type=simple-string from-end=t key=identity test-not=eql|
			       item sequence start (or end (length sequence)))
                              (if (or (eq test-not #'eq) (eq test-not 'eq))
                                  (|count seq-type=simple-string from-end=t key=identity test-not=eq|
				   item sequence start (or end (length sequence)))
                                  (|count seq-type=simple-string from-end=t key=identity test-not=other|
				   item sequence start (or end (length sequence)) test-not)))
                          (|count seq-type=simple-string from-end=t key=identity test=eql|
			   item sequence start (or end (length sequence))))))
              (if key
                  (if test
                      (if (or (eq test #'eql) (eq test 'eql))
                          (|count seq-type=simple-string from-end=nil key=other test=eql|
			   item sequence start (or end (length sequence)) key)
                          (if (or (eq test #'eq) (eq test 'eq))
                              (|count seq-type=simple-string from-end=nil key=other test=eq|
			       item sequence start (or end (length sequence)) key)
                              (|count seq-type=simple-string from-end=nil key=other test=other|
			       item sequence start (or end (length sequence)) key test)))
                      (if test-not
                          (if (or (eq test-not #'eql) (eq test-not 'eql))
                              (|count seq-type=simple-string from-end=nil key=other test-not=eql|
			       item sequence start (or end (length sequence)) key)
                              (if (or (eq test-not #'eq) (eq test-not 'eq))
                                  (|count seq-type=simple-string from-end=nil key=other test-not=eq|
				   item sequence start (or end (length sequence)) key)
                                  (|count seq-type=simple-string from-end=nil key=other test-not=other|
				   item sequence start (or end (length sequence)) key test-not)))
                          (|count seq-type=simple-string from-end=nil key=other test=eql|
			   item sequence start (or end (length sequence)) key)))
                  (if test
                      (if (or (eq test #'eql) (eq test 'eql))
                          (|count seq-type=simple-string from-end=nil key=identity test=eql|
			   item sequence start (or end (length sequence)))
                          (if (or (eq test #'eq) (eq test 'eq))
                              (|count seq-type=simple-string from-end=nil key=identity test=eq|
			       item sequence start (or end (length sequence)))
                              (|count seq-type=simple-string from-end=nil key=identity test=other|
			       item sequence start (or end (length sequence)) test)))
                      (if test-not
                          (if (or (eq test-not #'eql) (eq test-not 'eql))
                              (|count seq-type=simple-string from-end=nil key=identity test-not=eql|
			       item sequence start (or end (length sequence)))
                              (if (or (eq test-not #'eq) (eq test-not 'eq))
                                  (|count seq-type=simple-string from-end=nil key=identity test-not=eq|
				   item sequence start (or end (length sequence)))
                                  (|count seq-type=simple-string from-end=nil key=identity test-not=other|
				   item sequence start (or end (length sequence)) test-not)))
                          (|count seq-type=simple-string from-end=nil key=identity test=eql|
			   item sequence start (or end (length sequence)))))))
          (if (simple-vector-p sequence)
              (if from-end
                  (if key
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=simple-vector from-end=t key=other test=eql|
			       item sequence start (or end (length sequence)) key)
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=simple-vector from-end=t key=other test=eq|
				   item sequence start (or end (length sequence)) key)
                                  (|count seq-type=simple-vector from-end=t key=other test=other|
				   item sequence start (or end (length sequence)) key test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=simple-vector from-end=t key=other test-not=eql|
				   item sequence start (or end (length sequence)) key)
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=simple-vector from-end=t key=other test-not=eq|
				       item sequence start (or end (length sequence)) key)
                                      (|count seq-type=simple-vector from-end=t key=other test-not=other|
				       item sequence start (or end (length sequence)) key test-not)))
                              (|count seq-type=simple-vector from-end=t key=other test=eql|
			       item sequence start (or end (length sequence)) key)))
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=simple-vector from-end=t key=identity test=eql|
			       item sequence start (or end (length sequence)))
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=simple-vector from-end=t key=identity test=eq|
				   item sequence start (or end (length sequence)))
                                  (|count seq-type=simple-vector from-end=t key=identity test=other|
				   item sequence start (or end (length sequence)) test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=simple-vector from-end=t key=identity test-not=eql|
				   item sequence start (or end (length sequence)))
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=simple-vector from-end=t key=identity test-not=eq|
				       item sequence start (or end (length sequence)))
                                      (|count seq-type=simple-vector from-end=t key=identity test-not=other|
				       item sequence start (or end (length sequence)) test-not)))
                              (|count seq-type=simple-vector from-end=t key=identity test=eql|
			       item sequence start (or end (length sequence))))))
                  (if key
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=simple-vector from-end=nil key=other test=eql|
			       item sequence start (or end (length sequence)) key)
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=simple-vector from-end=nil key=other test=eq|
				   item sequence start (or end (length sequence)) key)
                                  (|count seq-type=simple-vector from-end=nil key=other test=other|
				   item sequence start (or end (length sequence)) key test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=simple-vector from-end=nil key=other test-not=eql|
				   item sequence start (or end (length sequence)) key)
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=simple-vector from-end=nil key=other test-not=eq|
				       item sequence start (or end (length sequence)) key)
                                      (|count seq-type=simple-vector from-end=nil key=other test-not=other|
				       item sequence start (or end (length sequence)) key test-not)))
                              (|count seq-type=simple-vector from-end=nil key=other test=eql|
			       item sequence start (or end (length sequence)) key)))
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=simple-vector from-end=nil key=identity test=eql|
			       item sequence start (or end (length sequence)))
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=simple-vector from-end=nil key=identity test=eq|
				   item sequence start (or end (length sequence)))
                                  (|count seq-type=simple-vector from-end=nil key=identity test=other|
				   item sequence start (or end (length sequence)) test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=simple-vector from-end=nil key=identity test-not=eql|
				   item sequence start (or end (length sequence)))
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=simple-vector from-end=nil key=identity test-not=eq|
				       item sequence start (or end (length sequence)))
                                      (|count seq-type=simple-vector from-end=nil key=identity test-not=other|
				       item sequence start (or end (length sequence)) test-not)))
                              (|count seq-type=simple-vector from-end=nil key=identity test=eql|
			       item sequence start (or end (length sequence)))))))
              (if from-end
                  (if key
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=general-vector from-end=t key=other test=eql|
			       item sequence start (or end (length sequence)) key)
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=general-vector from-end=t key=other test=eq|
				   item sequence start (or end (length sequence)) key)
                                  (|count seq-type=general-vector from-end=t key=other test=other|
				   item sequence start (or end (length sequence)) key test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=general-vector from-end=t key=other test-not=eql|
				   item sequence start (or end (length sequence)) key)
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=general-vector from-end=t key=other test-not=eq|
				       item sequence start (or end (length sequence)) key)
                                      (|count seq-type=general-vector from-end=t key=other test-not=other|
				       item sequence start (or end (length sequence)) key test-not)))
                              (|count seq-type=general-vector from-end=t key=other test=eql|
			       item sequence start (or end (length sequence)) key)))
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=general-vector from-end=t key=identity test=eql|
			       item sequence start (or end (length sequence)))
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=general-vector from-end=t key=identity test=eq|
				   item sequence start (or end (length sequence)))
                                  (|count seq-type=general-vector from-end=t key=identity test=other|
				   item sequence start (or end (length sequence)) test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=general-vector from-end=t key=identity test-not=eql|
				   item sequence start (or end (length sequence)))
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=general-vector from-end=t key=identity test-not=eq|
				       item sequence start (or end (length sequence)))
                                      (|count seq-type=general-vector from-end=t key=identity test-not=other|
				       item sequence start (or end (length sequence)) test-not)))
                              (|count seq-type=general-vector from-end=t key=identity test=eql|
			       item sequence start (or end (length sequence))))))
                  (if key
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=general-vector from-end=nil key=other test=eql|
			       item sequence start (or end (length sequence)) key)
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=general-vector from-end=nil key=other test=eq|
				   item sequence start (or end (length sequence)) key)
                                  (|count seq-type=general-vector from-end=nil key=other test=other|
				   item sequence start (or end (length sequence)) key test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=general-vector from-end=nil key=other test-not=eql|
				   item sequence start (or end (length sequence)) key)
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=general-vector from-end=nil key=other test-not=eq|
				       item sequence start (or end (length sequence)) key)
                                      (|count seq-type=general-vector from-end=nil key=other test-not=other|
				       item sequence start (or end (length sequence)) key test-not)))
                              (|count seq-type=general-vector from-end=nil key=other test=eql|
			       item sequence start (or end (length sequence)) key)))
                      (if test
                          (if (or (eq test #'eql) (eq test 'eql))
                              (|count seq-type=general-vector from-end=nil key=identity test=eql|
			       item sequence start (or end (length sequence)))
                              (if (or (eq test #'eq) (eq test 'eq))
                                  (|count seq-type=general-vector from-end=nil key=identity test=eq|
				   item sequence start (or end (length sequence)))
                                  (|count seq-type=general-vector from-end=nil key=identity test=other|
				   item sequence start (or end (length sequence)) test)))
                          (if test-not
                              (if (or (eq test-not #'eql) (eq test-not 'eql))
                                  (|count seq-type=general-vector from-end=nil key=identity test-not=eql|
				   item sequence start (or end (length sequence)))
                                  (if (or (eq test-not #'eq) (eq test-not 'eq))
                                      (|count seq-type=general-vector from-end=nil key=identity test-not=eq|
				       item sequence start (or end (length sequence)))
                                      (|count seq-type=general-vector from-end=nil key=identity test-not=other|
				       item sequence start (or end (length sequence)) test-not)))
                              (|count seq-type=general-vector from-end=nil key=identity test=eql|
			       item sequence start (or end (length sequence)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function count-if

(defun |count-if seq-type=general-vector from-end=nil key=identity|
    (predicate vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall predicate (aref vector i))))

(defun |count-if seq-type=general-vector from-end=nil key=other|
    (predicate vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall predicate (funcall key (aref vector i)))))

(defun |count-if seq-type=general-vector from-end=t key=identity|
    (predicate vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall predicate (aref vector i))))

(defun |count-if seq-type=general-vector from-end=t key=other|
    (predicate vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall predicate (funcall key (aref vector i)))))

(defun |count-if seq-type=simple-vector from-end=nil key=identity|
    (predicate vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall predicate (svref vector i))))

(defun |count-if seq-type=simple-vector from-end=nil key=other|
    (predicate vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall predicate (funcall key (svref vector i)))))

(defun |count-if seq-type=simple-vector from-end=t key=identity|
    (predicate vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall predicate (svref vector i))))

(defun |count-if seq-type=simple-vector from-end=t key=other|
    (predicate vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall predicate (funcall key (svref vector i)))))

(defun |count-if seq-type=simple-string from-end=nil key=identity|
    (predicate vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall predicate (schar vector i))))

(defun |count-if seq-type=simple-string from-end=nil key=other|
    (predicate vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (funcall predicate (funcall key (schar vector i)))))

(defun |count-if seq-type=simple-string from-end=t key=identity|
    (predicate vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall predicate (schar vector i))))

(defun |count-if seq-type=simple-string from-end=t key=other|
    (predicate vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (funcall predicate (funcall key (schar vector i)))))

(defun |count-if seq-type=list from-end=nil end=nil key=identity|
    (predicate list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (funcall predicate element))))

(defun |count-if seq-type=list from-end=nil end=nil key=other|
    (predicate list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (funcall predicate (funcall key element)))))

(defun |count-if seq-type=list from-end=nil end=other key=identity|
    (predicate list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (funcall predicate element)
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count-if seq-type=list from-end=nil end=other key=other|
    (predicate list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (funcall predicate (funcall key element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count-if seq-type=list from-end=t end=nil key=identity|
    (predicate list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (funcall predicate element))))

(defun |count-if seq-type=list from-end=t end=nil key=other|
    (predicate list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (funcall predicate (funcall key element)))))

(defun |count-if seq-type=list from-end=t end=other key=identity|
    (predicate list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (funcall predicate element)
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count-if seq-type=list from-end=t end=other key=other|
    (predicate list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (funcall predicate (funcall key element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun count-if (predicate sequence &key from-end (start 0) end key)
  (if (listp sequence)
      (if from-end
	  (if end
	      (if key
		  (|count-if seq-type=list from-end=t end=other key=other|
		   predicate sequence start end key)
		  (|count-if seq-type=list from-end=t end=other key=identity|
		   predicate sequence start end))
	      (if key
		  (|count-if seq-type=list from-end=t end=nil key=other|
		   predicate sequence start key)
		  (|count-if seq-type=list from-end=t end=nil key=identity|
		   predicate sequence start)))
	  (if end
	      (if key
		  (|count-if seq-type=list from-end=nil end=other key=other|
		   predicate sequence start end key)
		  (|count-if seq-type=list from-end=nil end=other key=identity|
		   predicate sequence start end))
	      (if key
		  (|count-if seq-type=list from-end=nil end=nil key=other|
		   predicate sequence start key)
		  (|count-if seq-type=list from-end=nil end=nil key=identity|
		   predicate sequence start))))
      (if (simple-string-p sequence)
          (if from-end
              (if key
		  (|count-if seq-type=simple-string from-end=t key=other|
		   predicate sequence start (or end (length sequence)) key)
		  (|count-if seq-type=simple-string from-end=t key=identity|
		   predicate sequence start (or end (length sequence))))
              (if key
		  (|count-if seq-type=simple-string from-end=nil key=other|
		   predicate sequence start (or end (length sequence)) key)
		  (|count-if seq-type=simple-string from-end=nil key=identity|
		   predicate sequence start (or end (length sequence)))))
          (if (simple-vector-p sequence)
              (if from-end
                  (if key
		      (|count-if seq-type=simple-vector from-end=t key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if seq-type=simple-vector from-end=t key=identity|
		       predicate sequence start (or end (length sequence))))
                  (if key
		      (|count-if seq-type=simple-vector from-end=nil key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if seq-type=simple-vector from-end=nil key=identity|
		       predicate sequence start (or end (length sequence)))))
              (if from-end
                  (if key
		      (|count-if seq-type=general-vector from-end=t key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if seq-type=general-vector from-end=t key=identity|
		       predicate sequence start (or end (length sequence))))
                  (if key
		      (|count-if seq-type=general-vector from-end=nil key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if seq-type=general-vector from-end=nil key=identity|
		       predicate sequence start (or end (length sequence)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function count-if-not

(defun |count-if-not seq-type=general-vector from-end=nil key=identity|
    (predicate vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall predicate (aref vector i)))))

(defun |count-if-not seq-type=general-vector from-end=nil key=other|
    (predicate vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall predicate (funcall key (aref vector i))))))

(defun |count-if-not seq-type=general-vector from-end=t key=identity|
    (predicate vector start end)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall predicate (aref vector i)))))

(defun |count-if-not seq-type=general-vector from-end=t key=other|
    (predicate vector start end key)
  (declare (type vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall predicate (funcall key (aref vector i))))))

(defun |count-if-not seq-type=simple-vector from-end=nil key=identity|
    (predicate vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall predicate (svref vector i)))))

(defun |count-if-not seq-type=simple-vector from-end=nil key=other|
    (predicate vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall predicate (funcall key (svref vector i))))))

(defun |count-if-not seq-type=simple-vector from-end=t key=identity|
    (predicate vector start end)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall predicate (svref vector i)))))

(defun |count-if-not seq-type=simple-vector from-end=t key=other|
    (predicate vector start end key)
  (declare (type simple-vector vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall predicate (funcall key (svref vector i))))))

(defun |count-if-not seq-type=simple-string from-end=nil key=identity|
    (predicate vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall predicate (schar vector i)))))

(defun |count-if-not seq-type=simple-string from-end=nil key=other|
    (predicate vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i from start below end
	count (not (funcall predicate (funcall key (schar vector i))))))

(defun |count-if-not seq-type=simple-string from-end=t key=identity|
    (predicate vector start end)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall predicate (schar vector i)))))

(defun |count-if-not seq-type=simple-string from-end=t key=other|
    (predicate vector start end key)
  (declare (type simple-string vector)
	   (type fixnum start end))
  (unless (<= 0 start (length vector))
    (error 'invalid-start-index
	   :datum start
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (unless (<= 0 end (length vector))
    (error 'invalid-end-index
	   :datum end
	   :expected-type `(integer 0 ,(length vector))
	   :in-sequence vector))
  (loop for i downfrom (1- end) to start
	count (not (funcall predicate (funcall key (schar vector i))))))

(defun |count-if-not seq-type=list from-end=nil end=nil key=identity|
    (predicate list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (funcall predicate element)))))

(defun |count-if-not seq-type=list from-end=nil end=nil key=other|
    (predicate list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (funcall predicate (funcall key element))))))

(defun |count-if-not seq-type=list from-end=nil end=other key=identity|
    (predicate list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (funcall predicate element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count-if-not seq-type=list from-end=nil end=other key=other|
    (predicate list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (funcall predicate (funcall key element)))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count-if-not seq-type=list from-end=t end=nil key=identity|
    (predicate list start)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (funcall predicate element)))))

(defun |count-if-not seq-type=list from-end=t end=nil key=other|
    (predicate list start key)
  (let ((remaining list)
	(start-bis start))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  count (not (funcall predicate (funcall key element))))))

(defun |count-if-not seq-type=list from-end=t end=other key=identity|
    (predicate list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (funcall predicate element))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun |count-if-not seq-type=list from-end=t end=other key=other|
    (predicate list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    (loop until (null remaining)
	  until (zerop start-bis)
	  do (setf remaining (cdr remaining))
	     (decf start-bis))
    (when (plusp start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    (loop for element in remaining
	  until (zerop end-start)
	  count (not (funcall predicate (funcall key element)))
	  do (decf end-start)
	  finally (when (plusp end-start)
		    (error 'invalid-end-index
			   :datum end
			   :expected-type `(integer 0 ,(- end end-start))
			   :in-sequence list)))))

(defun count-if-not (predicate sequence &key from-end (start 0) end key)
  (if (listp sequence)
      (if from-end
	  (if end
	      (if key
		  (|count-if-not seq-type=list from-end=t end=other key=other|
		   predicate sequence start end key)
		  (|count-if-not seq-type=list from-end=t end=other key=identity|
		   predicate sequence start end))
	      (if key
		  (|count-if-not seq-type=list from-end=t end=nil key=other|
		   predicate sequence start key)
		  (|count-if-not seq-type=list from-end=t end=nil key=identity|
		   predicate sequence start)))
	  (if end
	      (if key
		  (|count-if-not seq-type=list from-end=nil end=other key=other|
		   predicate sequence start end key)
		  (|count-if-not seq-type=list from-end=nil end=other key=identity|
		   predicate sequence start end))
	      (if key
		  (|count-if-not seq-type=list from-end=nil end=nil key=other|
		   predicate sequence start key)
		  (|count-if-not seq-type=list from-end=nil end=nil key=identity|
		   predicate sequence start))))
      (if (simple-string-p sequence)
          (if from-end
              (if key
		  (|count-if-not seq-type=simple-string from-end=t key=other|
		   predicate sequence start (or end (length sequence)) key)
		  (|count-if-not seq-type=simple-string from-end=t key=identity|
		   predicate sequence start (or end (length sequence))))
              (if key
		  (|count-if-not seq-type=simple-string from-end=nil key=other|
		   predicate sequence start (or end (length sequence)) key)
		  (|count-if-not seq-type=simple-string from-end=nil key=identity|
		   predicate sequence start (or end (length sequence)))))
          (if (simple-vector-p sequence)
              (if from-end
                  (if key
		      (|count-if-not seq-type=simple-vector from-end=t key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if-not seq-type=simple-vector from-end=t key=identity|
		       predicate sequence start (or end (length sequence))))
                  (if key
		      (|count-if-not seq-type=simple-vector from-end=nil key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if-not seq-type=simple-vector from-end=nil key=identity|
		       predicate sequence start (or end (length sequence)))))
              (if from-end
                  (if key
		      (|count-if-not seq-type=general-vector from-end=t key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if-not seq-type=general-vector from-end=t key=identity|
		       predicate sequence start (or end (length sequence))))
                  (if key
		      (|count-if-not seq-type=general-vector from-end=nil key=other|
		       predicate sequence start (or end (length sequence)) key)
		      (|count-if-not seq-type=general-vector from-end=nil key=identity|
		       predicate sequence start (or end (length sequence)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function merge

(defun |merge seq-type-1=list seq-type-2=list result-type=list key=identity|
    (l1 l2 predicate)
  (cond ((null l1) l2)
	((null l2) l1)
	(t (let (head)
	     (if (funcall predicate (car l2) (car l1))
		 (let ((temp (cdr l2)))
		   (setf head l2
			 l2 temp))
		 (let ((temp (cdr l1)))
		   (setf head l1
			 l1 temp)))
	     (let ((tail head))
	       (loop until (or (null l1) (null l2))
		     do (if (funcall predicate (car l2) (car l1))
			    (let ((temp (cdr l2)))
			      (setf (cdr tail) l2
				    l2 temp))
			    (let ((temp (cdr l1)))
			      (setf (cdr tail) l1
				    l1 temp)))
			(setf tail (cdr tail)))
	       (setf (cdr tail)
		     (if (null l1) l2 l1)))
	     head))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function sort

(defun |sort seq-type=list length=2| (list predicate)
  (let ((temp (cdr list)))
    (when (funcall predicate (car temp) (car list))
      (rotatef (car temp) (car list))))
  list)

(defun |sort seq-type=list length=3| (list predicate)
  (let* ((temp list)
	 (a (pop temp))
	 (b (pop temp))
	 (c (pop temp)))
    (if (funcall predicate a b)
	(cond ((funcall predicate b c)
	       ;; already sorted
	       nil)
	      ((funcall predicate c a)
	       (setf temp list)
	       (setf (car temp) c
		     temp (cdr temp)
		     (car temp) a
		     temp (cdr temp)
		     (car temp) b))
	      (t
	       (setf temp (cdr list))
	       (setf (car temp) c
		     temp (cdr temp)
		     (car temp) b)))
	(cond ((funcall predicate c b)
	       (setf temp list)
	       (setf (car temp) c
		     temp (cddr temp)
		     (car temp) a))
	      ((funcall predicate a c)
	       (setf temp list)
	       (setf (car temp) b
		     temp (cdr temp)
		     (car temp) a))
	      (t
	       (setf temp list)
	       (setf (car temp) b
		     temp (cdr temp)
		     (car temp) c
		     temp (cdr temp)
		     (car temp) a))))
    list))

(defun |sort seq-type=simple-vector length=3|
    (vector predicate start)
  (let* ((a (svref vector start))
	 (b (svref vector (1+ start)))
	 (c (svref vector (+ start 2))))
    (if (funcall predicate a b)
	(cond ((funcall predicate b c)
	       ;; already sorted
	       nil)
	      ((funcall predicate c a)
	       (setf (svref vector start) c
		     (svref vector (1+ start)) a
		     (svref vector (+ start 2)) b))
	      (t
	       (setf (svref vector (1+ start)) c
		     (svref vector (+ start 2)) b)))
	(cond ((funcall predicate c b)
	       (setf (svref vector start) c
		     (svref vector (+ start 2)) a))
	      ((funcall predicate a c)
	       (setf (svref vector start) b
		     (svref vector (1+ start)) a))
	      (t
	       (setf (svref vector start) b
		     (svref vector (1+ start)) c
		     (svref vector (+ start 2)) a))))))

(defun |sort seq-type=list key=identity|
    (list predicate)
  (labels ((sort-with-length (list length)
	     (case length
	       ((0 1) list)
	       (2 (let ((temp (cdr list)))
		    (when (funcall predicate (car temp) (car list))
		      (rotatef (car temp) (car list))))
		  list)
	       (3 (|sort seq-type=list length=3| list predicate))
	       (t (let* ((l1 (floor length 2))
			 (l2 (- length l1))
			 (middle (nthcdr (1- l1) list))
			 (second (cdr middle)))
		    (setf (cdr middle) nil)
		    (|merge seq-type-1=list seq-type-2=list result-type=list key=identity|
		     (sort-with-length list l1)
		     (sort-with-length second l2)
		     predicate))))))
    (sort-with-length list (length list))))

(defun |sort seq-type=simple-vector key=identity|
    (vector predicate)
  (declare (type simple-vector vector))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (labels ((sort-interval (start end)
	     (declare (type fixnum start end))
	     (case (- end start)
	       ((0 1) nil)
	       (2 (when (funcall predicate
				 (svref vector (1+ start))
				 (svref vector start))
		    (rotatef (svref vector (1+ start))
			     (svref vector start))))
	       (3 (|sort seq-type=simple-vector length=3|
		   vector predicate start))
	       (t
		  (let* ((middle (floor (+ start end) 2))
			 (pivot (svref vector middle)))
		    ;; Exclude the pivot element in order
		    ;; to make sure each part is strictly
		    ;; smaller than the whole. 
		    (rotatef (svref vector middle)
			     (svref vector (1- end)))
		    (let ((i start)
			  (j (- end 2)))
		      (declare (type fixnum i j))
		      (loop while (<= i j)
			    do (loop while (and (<= i j)
						(not (funcall predicate
							      pivot
							      (svref vector i))))
				     do (incf i))
			       (loop while (and (<= i j)
						(not (funcall predicate
							      (svref vector j)
							      pivot)))
				     do (decf j))
			       (when (< i j)
				 (rotatef (svref vector i) (svref vector j))
				 (incf i)
				 (decf j)))
		      (setf (svref vector (1- end))
			    (svref vector i))
		      (setf (svref vector i) pivot)
		      (sort-interval start i)
		      (sort-interval (1+ i) end)))))
	     nil))
    (sort-interval 0 (length vector)))
  vector)
	     
(defun sort (sequence predicate &key key)
  (if (listp sequence)
      (if key
	  nil
	  (|sort seq-type=list key=identity| sequence predicate))
      (if (simple-vector-p sequence)
	  (if key
	      nil
	      (|sort seq-type=simple-vector key=identity| sequence predicate))
	  nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function nsubstitute

(defun |nsubstitute seq-type=list end=nil test=eql count=nil key=identity|
    (newitem olditem list start)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (eql olditem (car remaining))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test=eql count=nil key=other|
    (newitem olditem list start key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (eql olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test=eql count=other key=identity|
    (newitem olditem list start count)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (eql olditem (car remaining))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test=eql count=other key=other|
    (newitem olditem list start count key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (eql olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test=eq count=nil key=identity|
    (newitem olditem list start)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (eq olditem (car remaining))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test=eq count=nil key=other|
    (newitem olditem list start key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (eq olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test=eq count=t key=identity|
    (newitem olditem list start count)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (eq olditem (car remaining))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test=eq count=other key=other|
    (newitem olditem list start count key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (eq olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test=other count=nil key=identity|
    (newitem olditem list test start)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (funcall test olditem (car remaining))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test=other count=nil key=other|
    (newitem olditem list test start key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (funcall test olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test=other count=t key=identity|
    (newitem olditem list test start count)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (funcall test olditem (car remaining))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test=other count=other key=other|
    (newitem olditem list test start count key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (funcall test olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test-not=eql count=nil key=identity|
    (newitem olditem list start)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (not (eql olditem (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test-not=eql count=nil key=other|
    (newitem olditem list start key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (not (eql olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test-not=eql count=t key=identity|
    (newitem olditem list start count)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (not (eql olditem (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test-not=eql count=other key=other|
    (newitem olditem list start count key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (not (eql olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test-not=eq count=nil key=identity|
    (newitem olditem list start)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (not (eq olditem (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test-not=eq count=nil key=other|
    (newitem olditem list start key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (not (eq olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test-not=eq count=t key=identity|
    (newitem olditem list start count)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (not (eq olditem (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test-not=eq count=other key=other|
    (newitem olditem list start count key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (not (eq olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test-not=other count=nil key=identity|
    (newitem olditem list test-not start)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (not (funcall test-not olditem (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=nil test-not=other count=nil key=other|
    (newitem olditem list test-not start key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  when (not (funcall test-not olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test-not=other count=t key=identity|
    (newitem olditem list test-not start count)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (not (funcall test-not olditem (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list from-end=nil end=nil test-not=other count=other key=other|
    (newitem olditem list test-not start count key)
  (let ((remaining list)
	(start-bis start))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop count)
	  when (not (funcall test-not olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))))
  list)

(defun |nsubstitute seq-type=list end=other test=eql count=nil key=identity|
    (newitem olditem list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (eql olditem (car remaining))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test=eql count=nil key=other|
    (newitem olditem list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (eql olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test=eql count=t key=identity|
    (newitem olditem list start end count)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (eql olditem (car remaining))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test=eql count=other key=other|
    (newitem olditem list start end count key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (eql olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test=eq count=nil key=identity|
    (newitem olditem list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (eq olditem (car remaining))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test=eq count=nil key=other|
    (newitem olditem list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (eq olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test=eq count=t key=identity|
    (newitem olditem list start end count)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (eq olditem (car remaining))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test=eq count=other key=other|
    (newitem olditem list start end count key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (eq olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test=other count=nil key=identity|
    (newitem olditem list test start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (funcall test olditem (car remaining))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test=other count=nil key=other|
    (newitem olditem list test start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (funcall test olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test=other count=t key=identity|
    (newitem olditem list test start end count)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (funcall test olditem (car remaining))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test=other count=other key=other|
    (newitem olditem list test start end count key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (funcall test olditem (funcall key (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test-not=eql count=nil key=identity|
    (newitem olditem list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (not (eql olditem (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test-not=eql count=nil key=other|
    (newitem olditem list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (not (eql olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test-not=eql count=t key=identity|
    (newitem olditem list start end count)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (not (eql olditem (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test-not=eql count=other key=other|
    (newitem olditem list start end count key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (not (eql olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test-not=eq count=nil key=identity|
    (newitem olditem list start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (not (eq olditem (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test-not=eq count=nil key=other|
    (newitem olditem list start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (not (eq olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test-not=eq count=t key=identity|
    (newitem olditem list start end count)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (not (eq olditem (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test-not=eq count=other key=other|
    (newitem olditem list start end count key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (not (eq olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test-not=other count=nil key=identity|
    (newitem olditem list test-not start end)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (not (funcall test-not olditem (car remaining)))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list end=other test-not=other count=nil key=other|
    (newitem olditem list test-not start end key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  when (not (funcall test-not olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test-not=other count=t key=identity|
    (newitem olditem list test-not start end count)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (not (funcall test-not olditem (car remaining)))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

(defun |nsubstitute seq-type=list from-end=other end=other test-not=other count=other key=other|
    (newitem olditem list test-not start end count key)
  (let ((remaining list)
	(start-bis start)
	(end-start (- end start)))
    ;; skip a prefix indicated by start
    (loop until (zerop start-bis)
	  until (endp remaining)
	  do (setf remaining (cdr remaining)))
    (unless (zerop start-bis)
      (error 'invalid-start-index
	     :datum start
	     :expected-type `(integer 0 ,(- start start-bis))
	     :in-sequence list))
    ;; We can't use loop for ... on, becaue it uses atom for testing the end
    (loop until (endp remaining)
	  until (zerop end-start)
	  until (zerop count)
	  when (not (funcall test-not olditem (funcall key (car remaining))))
	    do (setf (car remaining) newitem)
	       (decf count)
	  do (setf remaining (cdr remaining))
	     (decf end-start))
    (when (plusp end-start)
      (error 'invalid-end-index
	     :datum end
	     :expected-type `(integer 0 ,(- end end-start))
	     :in-sequence list)))
  list)

