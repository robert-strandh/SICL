(cl:in-package #:sicl-sequences-small)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility functions

(defun check-vector-bounding-indexes (vector start end)
  (when (not (typep start `(integer 0 (,(length vector)))))
    (error 'invalid-start-index
           :datum start
           :expected-type `(integer 0 (,(length vector)))
           :sequence vector))
  (when (not (typep end `(or null (integer 0 (,(length vector))))))
    (error 'invalid-start-index
           :datum start
           :expected-type `(or null (integer 0 (,(length vector))))
           :sequence vector))
  (when (and (integerp end)
             (< end start))
    (error 'invalid-bouding-indexes
           :datum (cons start end)
           :expected-type `(bounding-indexes ,(length vector))
           :sequence vector)))

(defun check-list-bounding-indexes (list start end)
  (unless (typep start '(integer 0))
    (error 'invalid-start-index
           :datum start
           :expected-type '(integer 0)
           :sequence list))
  (unless (typep end '(or null (integer 0)))
    (error 'invalid-end-index
           :datum end
           :expected-type '(or null (integer 0))
           :sequence list))
  (when (and (integerp start) (integerp end) (> start end))
    (error 'invalid-bouding-indexes
           :datum (cons start end)
           :expected-type 'bounding-indexes
           :sequence list)))

;;; Skip a prefix of a certain length, and check that the list
;;; doesn't end with an atom, and that it is at least as long as
;;; the prefix length.
(defun skip-prefix (list start)
  (let ((to-skip start)
        (rest list))
    (tagbody
     again
       ;; Check whether we are done skipping the prefix.
       (when (zerop to-skip)
         (go out))
       ;; Not done, so we must skip some more.  Check that
       ;; we still have a CONS cell
       (if (consp rest)
           ;; We are fine.  Skip one cell and loop.
           (progn (setf rest (cdr rest))
                  (decf to-skip)
                  (go again))
           ;; No more CONS cells, but more to skip.  Problem!!!
           (if (null rest)
               (error 'invalid-start-index
                      :datum start
                      :expected-type `(integer 0 ,(- start to-skip))
                      :sequence list)
               (error 'list-as-sequence-must-be-proper
                      :datum list
                      :expected-type 'proper-list)))
     out)
    rest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function LENGTH

(defun length (sequence)
  (declare (type sequence sequence))
  (if (vectorp sequence)
      (if (array-has-fill-pointer-p sequence)
          (fill-pointer sequence)
          (array-dimension sequence 0))
      (let ((length (proper-list-length sequence)))
        (when (null length)
          (error 'list-as-sequence-must-be-proper
                 :datum sequence
                 :expected-type 'proper-list))
        length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Process the elements of the active interval of a sequence.

(defun |process-elements seq-type=vector from-end=false|
    (vector action start end)
  (check-vector-bounding-indexes vector start end)
  (when (null end)
    (setf end (length vector)))
  (let ((i start))
    (tagbody
     again
       (when (>= i end)
         (go out))
       (funcall action i)
       (incf i)
       (go again)
     out)))

(defun |process-elements seq-type=vector from-end=true|
    (vector action start end)
  (check-vector-bounding-indexes vector start end)
  (when (null end)
    (setf end (length vector)))
  (let ((i (1- end)))
    (tagbody
     again
       (when (< i start)
         (go out))
       (funcall action i)
       (decf i)
       (go again)
     out)))

(defun |process-elements seq-type=list from-end=false|
    (list action start end)
  (check-list-bounding-indexes list start end)
  (let* ((rest (skip-prefix list start))
         ;; To ensure termination, we must check for circularity.
         (slow rest))
    (if (null end)
        ;; Process until the end of the list.
        (tagbody
         again
           ;; Make a single step.
           (if (consp rest)
               ;; Not finished.
               (progn (funcall action rest)
                      (setf rest (cdr rest)))
               ;; Finished, but why?
               (if (null rest)
                   ;; End of the list, we are done.
                   (go out)
                   ;; We reached a non NIL atom.  Problem!
                   (error 'list-as-sequence-must-be-proper
                          :datum list
                          :expected-type 'proper-list)))
           ;; Make another step.
           (if (consp rest)
               ;; Not finished.
               (progn (funcall action rest)
                      (setf rest (cdr rest)))
               ;; Finished, but why?
               (if (null rest)
                   ;; End of the list, we are done.
                   (go out)
                   ;; We reached a non NIL atom.  Problem!
                   (error 'list-as-sequence-must-be-proper
                          :datum list
                          :expected-type 'proper-list)))
           ;; Make a single step with the slow pointer
           (setf slow (cdr slow))
           ;; Check for circularity.
           (when (eq rest slow)
             (error 'list-as-sequence-must-be-proper
                    :datum list
                    :expected-type 'proper-list))
           ;; No circularity.  Loop.
           (go again)
         out)
        ;; We have a numeric value for END.
        (let ((remaining-to-process (- end start)))
          (tagbody
           again
             ;; Make one step.
             (if (zerop remaining-to-process)
                 ;; We are done.
                 (go out)
                 ;; Not done.  Check that there are remaining elements.
                 (if (consp rest)
                     ;; Yes, more elements.
                     (progn (funcall action rest)
                            (setf rest (cdr rest))
                            (decf remaining-to-process))
                     ;; No more elements.  Problem!
                     (if (null rest)
                         (error 'invalid-end-index
                                :datum end
                                :expected-type
                                `(or null (integer 0 ,(- end remaining-to-process)))
                                :sequence list)
                         (error 'list-as-sequence-must-be-proper
                                :datum list
                                :expected-type 'proper-list))))
             ;; Make another step.
             (if (zerop remaining-to-process)
                 ;; We are done.
                 (go out)
                 ;; Not done.  Check that there are remaining elements.
                 (if (consp rest)
                     ;; Yes, more elements.
                     (progn (funcall action rest)
                            (setf rest (cdr rest))
                            (decf remaining-to-process))
                     ;; No more elements.  Problem!
                     (if (null rest)
                         (error 'invalid-end-index
                                :datum end
                                :expected-type
                                `(or null (integer 0 ,(- end remaining-to-process)))
                                :sequence list)
                         (error 'list-as-sequence-must-be-proper
                                :datum list
                                :expected-type 'proper-list))))
             (setf slow (cdr slow))
             ;; Check for circularity.
             (when (eq rest slow)
               (error 'list-as-sequence-must-be-proper
                      :datum list
                      :expected-type 'proper-list))
             ;; No circularity.  Loop.
             (go again)
             out)
          (unless (listp rest)
            (error 'list-as-sequence-must-be-proper
                   :datum list
                   :expected-type 'proper-list))))))

;;; When we traverse a list from the end, we use the recursion stack
;;; to visit elements during backtrack.  However, because the stacks
;;; might have limited depth, we make sure we only use a fixed number
;;; of recursive calls.  This parameter indicates how many recursive
;;; calls we are allowed to use.  In fact, we will probably use up to
;;; 4 times as many recursions as that.  Implementations should set
;;; this as large as possible, but it should be significantly smaller
;;; than any hard limit on the recursion depth to allow for our
;;; traversal to be invoked when the stack already has some
;;; invocations on it.
(defparameter *max-recursion-depth* 100)

;;; The basic traversal technique is as follows.  We divide the list
;;; into chunks such that there are no more than c <= m chunks and
;;; each chunk has the a size of m^k where m is the "maximum"
;;; recursion depth allowed and k is the smallest nonnegative integer
;;; that makes c <= m.  We then handle each chunk on the backtrack
;;; side of a recursive call, so that the last chunk is handled first.
;;; Each chunk is then handled the same way, but this time with a
;;; sub-chunk size of m^(k-1), etc, until the sub-chunk size is 1 at
;;; which point we call a function traverse-list-1 which was passed in
;;; as an argument.
(defun traverse-list (action list length)
  (labels ((traverse-list-step-1 (list length)
             (if (zerop length)
                 nil
                 (progn (traverse-list-step-1 (cdr list) (1- length))
                        (funcall action list)))))
    (let ((max-recursion-depth *max-recursion-depth*))
      (labels ((aux (list length step)
                 (cond ((> (ceiling length step) max-recursion-depth)
                        (aux list length (* step max-recursion-depth)))
                       ((= step 1)
                        (traverse-list-step-1 list length))
                       ((<= length step)
                        (aux list length (/ step max-recursion-depth)))
                       (t
                        (aux (nthcdr step list) (- length step) step)
                        (aux list step (/ step max-recursion-depth))))))
        (aux list length 1)))))

(defun |process-elements seq-type=list from-end=true|
    (list action start end)
  (check-list-bounding-indexes list start end)
  ;; Determine a numeric value for END and make some checks.
  (if (null end)
      (let ((length (proper-list-length list)))
        (when (null length)
          (error 'list-as-sequence-must-be-proper
                 :datum list
                 :expected-type 'proper-list))
        (when (> start length)
          (error 'invalid-start-index
                 :datum start
                 :expected-type `(integer 0 (,length))
                 :sequence list))
        (setf end length))
      ;; Check that we have at least END CONS cells.
      ;; Maybe later attempt to detect circularity.
      (let ((count end)
            (rest list))
        (tagbody
         again
           (when (zerop count)
             (go out))
           (if (consp rest)
               (setf rest (cdr rest))
               (if (null rest)
                   (error 'invalid-end-index
                          :datum end
                          :expected-type `(integer 0 ,(- end count))
                          :sequence list)
                   (error 'list-as-sequence-must-be-proper
                          :datum list
                          :expected-type 'proper-list)))
         out)))
  (traverse-list (nthcdr start list) (- end start)))

(defun select-process-function (sequence from-end)
  (if (vectorp sequence)
      (if from-end
          #'|process-elements seq-type=vector from-end=true|
          #'|process-elements seq-type=vector from-end=false|)
      (if from-end
          #'|process-elements seq-type=list from-end=true|
          #'|process-elements seq-type=list from-end=false|)))

(defun make-predicate-from-tests (item test test-p test-not test-not-p key)
  (when (null key)
    (setf key #'identity))
  (cond ((and test-p test-not-p)
         (error 'both-test-and-test-not-given))
        (test-p
         (lambda (element)
           (funcall test item (funcall key element))))
        (test-not-p
         (lambda (element)
           (not (funcall test-not item (funcall key element)))))
        (t
         (lambda (element)
           (eql item (funcall key element))))))

(defun make-predicate-from-predicate (predicate key)
  (when (null key)
    (setf key #'identity))
  (lambda (element) (funcall predicate (funcall key element))))

(defun make-predicate-from-inverse-predicate (predicate key)
  (when (null key)
    (setf key #'identity))
  (lambda (element) (not (funcall predicate (funcall key element)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COUNT, COUNT-IF, COUNT-IF-NOT

(defun count-common (sequence predicate from-end start end)
  (let ((count 0))
    (let ((action (if (vectorp sequence)
                      (lambda (index)
                        (when (funcall predicate (aref sequence index))
                          (incf count)))
                      (lambda (cell)
                        (when (funcall predicate (car cell))
                          (incf count))))))
      (funcall (select-process-function sequence from-end)
               sequence action start end))
    count))

(defun count (item sequence
              &key
              from-end
              (start 0)
              end
              key
              (test nil test-p)
              (test-not nil test-not-p))
  (count-common
   sequence
   (make-predicate-from-tests item test test-p test-not test-not-p key)
   from-end start end))

(defun count-if  (predicate sequence &key from-end (start 0) end key)
  (count-common
   sequence
   (make-predicate-from-predicate predicate key)
   from-end start end))

(defun count-if-not  (predicate sequence &key from-end (start 0) end key)
  (count-common
   sequence
   (make-predicate-from-negative-predicate predicate key)
   from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FIND, FIND-IF, FIND-IF-NOT

(defun find-common (sequence predicate from-end start end)
  (let ((action (if (vectorp sequence)
                    (lambda (index)
                      (when (funcall predicate (aref sequence index))
                        (return-from find-common (aref sequence index)))
                      (when (funcall predicate (car cell))
                        (return-from find-common (car cell)))))))
      (funcall (select-process-function sequence from-end)
               sequence action start end))
    nil)

(defun find (item sequence
              &key
              from-end
              (start 0)
              end
              key
              (test nil test-p)
              (test-not nil test-not-p))
  (find-common
   sequence
   (make-predicate-from-tests item test test-p test-not test-not-p key)
   from-end start end))

(defun find-if  (predicate sequence &key from-end (start 0) end key)
  (find-common
   sequence
   (make-predicate-from-predicate predicate key)
   from-end start end))

(defun find-if-not  (predicate sequence &key from-end (start 0) end key)
  (find-common
   sequence
   (make-predicate-from-negative-predicate predicate key)
   from-end start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; POSITION, POSITION-IF, POSITION-IF-NOT

(defun position-common (sequence predicate from-end start end)
  (let ((action (if (vectorp sequence)
                    (lambda (index)
                      (when (funcall predicate (aref sequence index))
                        (return-from position-common (aref sequence index)))
                      (when (funcall predicate (car cell))
                        (return-from position-common (car cell)))))))
      (funcall (select-process-function sequence from-end)
               sequence action start end))
    nil)

(defun position (item sequence
              &key
              from-end
              (start 0)
              end
              key
              (test nil test-p)
              (test-not nil test-not-p))
  (position-common
   sequence
   (make-predicate-from-tests item test test-p test-not test-not-p key)
   from-end start end))

(defun position-if  (predicate sequence &key from-end (start 0) end key)
  (position-common
   sequence
   (make-predicate-from-predicate predicate key)
   from-end start end))

(defun position-if-not  (predicate sequence &key from-end (start 0) end key)
  (position-common
   sequence
   (make-predicate-from-negative-predicate predicate key)
   from-end start end))

