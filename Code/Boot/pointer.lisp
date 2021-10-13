(cl:in-package #:sicl-boot)

(defparameter *ersatz-object-table*
  (make-hash-table :test #'eq))

(defun compute-pointer (object)
  (etypecase object
    ((integer #.(- (expt 2 62)) #.(1- (expt 2 62)))
     (values (ash object 1) '()))
    (character
     ;; FIXME: use a target-specific table instead of realying on the
     ;; host.
     (values (+ (ash (char-code object) 5) #x00011) '()))
    (cons
     (let* ((address (sicl-allocator:allocate-dyad))
            (result (+ address 1)))
       (setf (gethash object *ersatz-object-table*) result)
       (values result
               (list (cons address (car object))
                     (cons (+ address 8) (cdr object))))))
    ;; FIXME: add more types
    ))

(defun pointer (object)
  (multiple-value-bind (result work-list-items)
      (compute-pointer object)
    ;; The work list is an association list where the CAR of each
    ;; element is an address (i.e. a fixnum) and the CDR is an ersatz
    ;; object that should be written to that address.
    (let ((work-list work-list-items))
      (loop until (null work-list)
            do (destructuring-bind (address . object)
                   (pop work-list)
                 (multiple-value-bind (result work-list-items)
                     (compute-pointer object)
                   (setf work-list (append work-list-items work-list))
                   (setf (sicl-memory:memory-unsigned address 64) result)))))
    result))
