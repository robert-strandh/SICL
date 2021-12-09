(defpackage #:cliff-click
  (:use #:common-lisp)
  (:shadow #:hash-table
           #:make-hash-table
           #:gethash
           #:remhash))

(cl:in-package #:cliff-click)

(defclass hash-table ()
  ((%empty :initform (list nil) :reader empty)
   (%contents :initform (make-array #.(expt 2 18)) :reader contents)))

(defmethod initialize-instance :after ((object hash-table) &key &allow-other-keys)
  (loop with empty = (empty object)
        with contents = (contents object)
        for i from 0 below (length contents)
        do (setf (svref contents i) empty)))

(defun make-hash-table ()
  (make-instance 'hash-table))

(defun gethash (key hash-table)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop with index = (ash (logand (sxhash key) #.(1- (expt 2 17))) 1)
        with empty = (empty hash-table)
        with contents = (contents hash-table)
        for stored-key = (svref contents index)
        do (cond ((eq stored-key empty)
                  (return-from gethash (values nil nil)))
                 ((eq stored-key key)
                  (let ((stored-value (svref contents (1+ index))))
                    (if (eq stored-value empty)
                        (return-from gethash (values nil nil))
                        (return-from gethash (values stored-value t)))))
                 (t
                  (setq index (logand (+ index 2) #.(1- (expt 2 17))))))))

(defun (setf gethash) (value key hash-table)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop with index = (ash (logand (sxhash key) #.(1- (expt 2 17))) 1)
        with empty = (empty hash-table)
        with contents = (contents hash-table)
        for stored-key = (svref contents index)
        do (cond ((eq stored-key empty)
                  (setf (svref contents index) key)
                  (setf (svref contents (1+ index)) value)
                  (return-from gethash value))
                 ((eq stored-key key)
                  (setf (svref contents (1+ index)) value)
                  (return-from gethash value))
                 (t
                  (setq index (logand (+ index 2) #.(1- (expt 2 17))))))))

(defun remhash (key hash-table)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop with index = (ash (logand (sxhash key) #.(1- (expt 2 17))) 1)
        with empty = (empty hash-table)
        with contents = (contents hash-table)
        for stored-key = (svref contents index)
        do (cond ((eq stored-key empty)
                  (return-from remhash nil))
                 ((eq stored-key key)
                  (if (eq (svref contents (1+ index)) empty)
                      (return-from remhash nil)
                      (progn (setf (svref contents (1+ index)) empty)
                             (return-from remhash t))))
                 (t
                  (setq index (logand (+ index 2) #.(1- (expt 2 17))))))))

;;; The length of the contents vector is (expt 2 18) so there are
;;; (expt 2 17) possibly entries.  We want to test the algorithm with
;;; a fill degree of 50%, so we only want (expt 2 16) keys.
(defparameter *keys*
  (let ((keys (make-array #.(expt 2 16))))
    (loop for i from 0 below #.(expt 2 16)
          do (setf (svref keys i) (gensym)))
    keys))

(defun random-test (n)
  (let ((h1 (make-hash-table))
        (h2 (cl:make-hash-table :test #'eq)))
    (loop repeat n
          for operation = (random 3)
          for key = (svref *keys* (random #.(expt 2 16)))
          for result1 = (multiple-value-list
                         (case operation
                           (0 (gethash key h1))
                           (1 (setf (gethash key h1) t))
                           (2 (remhash key h1))))
          for result2 = (multiple-value-list
                         (case operation
                           (0 (cl:gethash key h2))
                           (1 (setf (cl:gethash key h2) t))
                           (2 (cl:remhash key h2))))
          do (assert (equal result1 result2)))))
