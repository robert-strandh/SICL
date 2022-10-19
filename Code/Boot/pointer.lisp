(cl:in-package #:sicl-boot)

;;; The keys in this table are host objects.  Such an object can be an
;;; instance of HEADER which means that it is an ersatz object, or it
;;; can be an ordinary host object that represents a target object,
;;; such as a symbol or a CONS cell.  The value associated with a key
;;; is a non-negative integer representing the pointer value of the
;;; corresponding object.
(defparameter *host-object-to-pointer-table*
  (make-hash-table :test #'eq))

(defparameter *ersatz-object-table*
  (make-hash-table :test #'eq))

(defun host-char-to-target-code (char)
  #+sb-unicode
  (char-code char)
  #-sb-unicode
  (if (eql char #\Newline)
      10  ; ASCII 10, LF
      (let ((position (position char " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")))
        (if (not (null position))
            (+ 32 position)
            #xfffd)))) ; U+FFFD REPLACEMENT CHARACTER

;;; Both HEADER and RACK have been allocated in the simulated heap.
;;; RACK-ADDRESS is the address in the simulated heap of the rack.
;;; This function creates work-list items for the mandatory prefix of
;;; the rack. Then it calls the generic function TRAVERSE in E5 to
;;; generate more work-list items, and it returns a list of all
;;; work-list items created.
(defun handle-ersatz-object (header rack rack-address)
  (let ((traverse
          (env:fdefinition *client* *e5* 'traverse))
        ;; The first element of the prefix is the stamp which is a
        ;; fixnum represented as a host integer.
        (stamp-item
          (cons rack-address (aref rack 0)))
        ;; The second element of the prefix is the list of effective
        ;; slot definitions, represented as a host list.
        (effective-slot-item
          (cons (+ rack-address 8) (aref rack 1)))
        ;; The third element of the prefix is the hash code for the
        ;; standard object.
        (hash-item
          (cons (+ rack-address 16) (aref rack 2))))
    (list* stamp-item
           effective-slot-item
           hash-item
           (funcall traverse header rack rack-address))))

;;; Allocate the header and the rack of an ersatz object in the
;;; simulated heap.  Write the tagged rack pointer into the second
;;; word of the header.  Return three values: The pointer to the
;;; header, i.e., to the object itself, the raw address of the rack,
;;; and a work-list item for writing the class object into the first
;;; word of the header.
(defun allocate-ersatz-object (object)
  (let* ((header-address (sicl-allocator:allocate-dyad))
         (class (slot-value object '%class))
         (rack (slot-value object '%rack))
         (rack-size (length rack))
         (rack-address (sicl-allocator:allocate-chunk rack-size)))
    ;; Since the rack is not an object in itself, we need to write the
    ;; address of the rack in the memory location corresponding to the
    ;; second word of the header.
    (setf (sicl-memory:memory-unsigned (+ header-address 8) 64)
          (+ rack-address 7))
    (values (+ header-address 5)
            rack-address
            ;; An item to write the class object into the first
            ;; word of the header
            (cons header-address class))))

(defgeneric compute-pointer (object))

(defmethod compute-pointer ((object integer))
  (assert (<= #.(- (expt 2 62)) object #.(1- (expt 2 62))))
  (values (if (minusp object)
              (ash (logand object #.(1- (expt 2 63))) 1)
              (ash object 1))
          '()))

(defmethod compute-pointer ((object character))
  (values (+ (ash (host-char-to-target-code object) 5) #x00011)
          '()))

(defmethod compute-pointer ((object cons))
  (let* ((address (sicl-allocator:allocate-dyad))
         (pointer (1+ address)))
    (setf (gethash object *host-object-to-pointer-table*) pointer)
    (values pointer
            (list (cons address (car object))
                  (cons (+ address 8) (cdr object))))))

(defun obsolete-compute-pointer (object)
  (typecase object
    ((integer 0 #.(1- (expt 2 62)))
     (values (ash object 1) '()))
    ((integer #.(- (expt 2 62)) -1)
     (values (ash (logand object #.(1- (expt 2 63))) 1)))
    (character
     (values (+ (ash (host-char-to-target-code object) 5) #x00011) '()))
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
                    (obsolete-compute-pointer ersatz-string)
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
                    (obsolete-compute-pointer ersatz-symbol)
                  (setf (gethash object *ersatz-object-table*) result)
                  (remhash ersatz-symbol *ersatz-object-table*)
                  (values result work-list-items))))
             (header
              (let* ((header-address (sicl-allocator:allocate-dyad))
                     (rack (slot-value object '%rack))
                     (rack-size (length rack))
                     (rack-address (sicl-allocator:allocate-chunk rack-size))
                     (rack-pointer (+ rack-address 7))
                     (fun (env:fdefinition (env:client *e5*) *e5* 'trace-prefix))
                     (prefix-size (funcall fun object))
                     (result (+ header-address 5))
                     (header-item (cons header-address
                                        (slot-value object '%class)))
                     (rack-items
                       (loop for i from 0 below prefix-size
                             for address from rack-address by 8
                             collect (cons address (aref rack i)))))
                ;; Set the rack slot in the header to point to the rack.
                (setf (sicl-memory:memory-unsigned (+ header-address 8) 64)
                      rack-pointer)
                (values result (cons header-item rack-items))))))))))

(defun write-pointer-to-address (address pointer)
  (setf (sicl-memory:memory-unsigned address 64)
        pointer))

(defun process-work-list-items (work-list-items)
  ;; The work list is a list of work-list items.  A work-list item is
  ;; a CONS cell where the CAR is an address (i.e. a fixnum), and the
  ;; CDR is a host object (which can be an ersatz object).  The item
  ;; represents an instruction that the pointer of the object should
  ;; be written to the address.
  (let ((work-list work-list-items))
    (loop until (null work-list)
          do (destructuring-bind (address . object)
                 (pop work-list)
               ;; It is possible that OBJECT already has a pointer
               ;; associated with it.
               (let ((pointer (gethash object *host-object-to-pointer-table*)))
                 (if (null pointer)
                     ;; No luck, we need to compute the pointer.
                     (multiple-value-bind (pointer work-list-items)
                         (compute-pointer object)
                       ;; Computing the pointer may result in more
                       ;; work-list items, so we prepend them to the
                       ;; work-list.
                       (setf work-list (append work-list-items work-list))
                       ;; And write the resulting pointer to the
                       ;; address.
                       (write-pointer-to-address address pointer))
                     ;; We are in luck.  A pointer for the object
                     ;; already exists.  Just write the pointer to the
                     ;; address.
                     (write-pointer-to-address address pointer)))))))

(defun pointer (object)
  ;; Check whether we have already allocated OBJECT in the heap.
  (let ((existing-pointer (gethash object *host-object-to-pointer-table*)))
    (if (null existing-pointer)
        ;; We need to allocate the object and compute the pointer.
        (multiple-value-bind (pointer work-list-items)
            (compute-pointer object)
          ;; Computing the pointer may have resulted in a bunch of
          ;; work-list items that must be processed.
          (process-work-list-items work-list-items)
          ;; Once the work-list has been processed, we are done and we
          ;; can return the computed pointer.
          pointer)
        ;; We are in luck.  We already have a pointer for the object,
        ;; so just return it.
        existing-pointer)))
