(cl:in-package #:sicl-boot)

(defparameter *ersatz-object-table*
  (make-hash-table :test #'eq))

(defun compute-pointer (object)
  (typecase object
    ((integer 0 #.(1- (expt 2 62)))
     (values (ash object 1) '()))
    ((integer #.(- (expt 2 62)) -1)
     (values (ash (logand object #.(1- (expt 2 63))) 1)))
    (character
     ;; FIXME: use a target-specific table instead of realying on the
     ;; host.
     (values (+ (ash (char-code object) 5) #x00011) '()))
    (otherwise
     (let ((result (gethash object *ersatz-object-table*)))
       (if (not (null result))
           result
           (etypecase object
             (cons
              (let* ((address (sicl-allocator:allocate-dyad))
                     (result (+ address 1)))
                (setf (gethash object *ersatz-object-table*) result)
                (values result
                        (list (cons address (car object))
                              (cons (+ address 8) (cdr object))))))
             (string
              (let* ((ms (env:fdefinition (env:client *e5*) *e5* 'make-string))
                     (sa (env:fdefinition (env:client *e5*) *e5* '(setf aref)))
                     (ersatz-string (funcall ms (length object))))
                (loop for i from 0 below (length object)
                      do (funcall sa (aref object i) i))
                (multiple-value-bind (result work-list-items)
                    (compute-pointer ersatz-string)
                  (setf (gethash object *ersatz-object-table*) result)
                  (remhash ersatz-string *ersatz-object-table*)
                  (values result work-list-items))))
             (symbol
              (let* ((mi (env:fdefinition (env:client *e5*) *e5* 'make-instance))
                     (ersatz-symbol
                       (funcall mi
                                :name (symbol-name object)
                                ;; FIXME: Pass a package object.
                                :package nil)))
                (multiple-value-bind (result work-list-items)
                    (compute-pointer ersatz-symbol)
                  (setf (gethash object *ersatz-object-table*) result)
                  (remhash ersatz-symbol *ersatz-object-table*)
                  (values result work-list-items))))
             ;; FIXME: add more types
             ))))))

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
