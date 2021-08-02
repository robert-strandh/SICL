(cl:in-package #:cleavir-code-utilities)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tools for checking for list structure.

;;; For any object, return its structure as a list as two values: the
;;; first value contains the number of unique CONS cells in the list,
;;; and the second value is one of the keywords :proper, :dotted, and
;;; :circular.  For an atom, 0 and :dotted is returned.  
;;;
;;; This function is useful for processing code because lists
;;; representing code are not often very long, so the method used is
;;; fast and appropriate, and because we often need to check that such
;;; lists are proper, but the simple method would go into an infinite
;;; computation if the list is circular, whereas we would like to give
;;; an error message in that case.
(defun list-structure (object)
  ;; First we attempt to just traverse the list as usual,
  ;; assuming that it is fairly short.  If we reach the end,
  ;; then that's great, and we return the result.
  (loop for remaining = object then (cdr remaining)
        for count from 0 to 100
        while (consp remaining)
        finally (when (atom remaining)
                  (return-from list-structure
                    (values count
                            (if (null remaining)
                                :proper
                                :dotted)))))
  ;; Come here if the list has more than a few CONS cells.  We
  ;; traverse it again, this time entering each CONS cell in a hash
  ;; table.  Stop when we reach the end of the list, or when we see
  ;; the same CONS cell twice. 
  (let ((table (make-hash-table :test #'eq)))
    (loop for remaining = object then (cdr remaining)
          while (consp remaining)
          until (gethash remaining table)
          do (setf (gethash remaining table) t)
          finally (return (values (hash-table-count table)
                                  (if (null remaining)
                                      :proper
                                      (if (atom remaining)
                                          :dotted
                                          :circular)))))))

;;; Check that an object is a proper list.  Return true if the object
;;; is a proper list.  Return false if the object is an atom other
;;; than NIL or if the list is dotted or circular.
(defun proper-list-p (object)
  (cond  ((null object) t)
         ((atom object) nil)
         (t (let ((slow object)
                  (fast (cdr object)))
              (declare (type cons slow))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from proper-list-p
                     (if (null fast) t nil)))
                 (when (eq fast slow)
                   (return-from proper-list-p nil))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from proper-list-p
                     (if (null fast) t nil)))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (go again))))))
             
;;; Check that an object is a proper list, and if so, return the
;;; number of cons cells in the list.  Return false if the object is
;;; an atom other than NIL or if the list is dotted or circular.  If
;;; the object is NIL, 0 is returned.
(defun proper-list-length (object)
  (cond  ((null object) 0)
         ((atom object) nil)
         (t (let ((slow object)
                  (fast (cdr object))
                  (count 1))
              (declare (type cons slow))
              ;; We assume that the implementation is such that a
              ;; fixnum is able to hold the maximum number of CONS
              ;; cells possible in the heap.
              (declare (type fixnum count))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from proper-list-length
                     (if (null fast) count nil)))
                 (when (eq fast slow)
                   (return-from proper-list-length nil))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from proper-list-length
                     (if (null fast) (1+ count) nil)))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (incf count 2)
                 (go again))))))
             
;;; If all we want is to know whether some object is a dotted list, we
;;; use the method of the slow and the fast pointer.  This function
;;; returns true if the object is an atom other than NIL (the
;;; degenerate case of a dotted list) or if the list is terminated by
;;; some atom other than NIL.  It returns false if the object is NIL,
;;; if the object is a list terminated by NIL, or of the object is a
;;; circular list.
(defun dotted-list-p (object)
  (cond  ((null object) nil)
         ((atom object) 0)
         (t (let ((slow object)
                  (fast (cdr object)))
              (declare (type cons slow))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from dotted-list-p
                     (if (null fast) nil t)))
                 (when (eq fast slow)
                   (return-from dotted-list-p nil))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from dotted-list-p
                     (if (null fast) nil t)))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (go again))))))
             
;;; Check that an object is a dotted list, and if so, return the
;;; number of cons cells in the list.  Return false if the object is
;;; NIL, if the object is a list terminated by NIL, or of the object
;;; is a circular list.  Return 0 if the object is an atom other than
;;; NIL (the degenerate case of a dotted list).
(defun dotted-list-length (object)
  (cond  ((null object) nil)
         ((atom object) 0)
         (t (let ((slow object)
                  (fast (cdr object))
                  (count 1))
              (declare (type cons slow))
              ;; We assume that the implementation is such that a
              ;; fixnum is able to hold the maximum number of CONS
              ;; cells possible in the heap.
              (declare (type fixnum count))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from dotted-list-length
                     (if (null fast) nil count)))
                 (when (eq fast slow)
                   (return-from dotted-list-length nil))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from dotted-list-length
                     (if (null fast) nil (1+ count))))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (incf count 2)
                 (go again))))))
             
(defun proper-or-dotted-list-length (object)
  (cond  ((atom object) 0)
         (t (let ((slow object)
                  (fast (cdr object))
                  (count 1))
              (declare (type cons slow))
              ;; We assume that the implementation is such that a
              ;; fixnum is able to hold the maximum number of CONS
              ;; cells possible in the heap.
              (declare (type fixnum count))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from proper-or-dotted-list-length count))
                 (when (eq fast slow)
                   (return-from proper-or-dotted-list-length nil))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from proper-or-dotted-list-length (1+ count)))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (incf count 2)
                 (go again))))))
             
;;; If all we want is to know whether some object is a circular list,
;;; we use the method of the slow and the fast pointer.  This function
;;; returns true if the object is a circular list, and false for any
;;; other object.
(defun circular-list-p (object)
  (cond  ((atom object) nil)
         (t (let ((slow object)
                  (fast (cdr object)))
              (declare (type cons slow))
              (tagbody
               again
                 (unless (consp fast)
                   (return-from circular-list-p nil))
                 (when (eq fast slow)
                   (return-from circular-list-p t))
                 (setq fast (cdr fast))
                 (unless (consp fast)
                   (return-from circular-list-p nil))
                 (setq fast (cdr fast))
                 (setq slow (cdr slow))
                 (go again))))))
