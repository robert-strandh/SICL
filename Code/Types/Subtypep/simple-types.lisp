(defclass table ()
  ((%bit-positions :initform (make-hash-table :test #'eq) :reader bit-positions)
   (%individual-objects :initform (make-hash-table :test #'eql) :reader individual-objects)
   (%next-bit-position :initform 0 :accessor next-bit-position)))

;;; Assume there are no DEFTYPE-defined types in type-specifier.
(defgeneric scan-for-simple-types (type-specifier table))

(defgeneric scan-for-simple-types-compound (head body table))

(defun scan-body-for-simple-types (body table)
  (loop for type-specifier in body
        do (scan-for-simple-types type-specifier table)))

(defmethod scan-for-simple-types-compound (head body table)
  nil)

(defmethod scan-for-simple-types-compound ((head (eql 'and)) body table)
  (scan-body-for-simple-types body table))

(defmethod scan-for-simple-types-compound ((head (eql 'or)) body table)
  (scan-body-for-simple-types body table))

(defmethod scan-for-simple-types-compound ((head (eql 'not)) body table)
  (scan-for-simple-types (first body) table))

(defmethod scan-for-simple-types ((type-specifier cons) table)
  (scan-for-simple-types-compound
   (first type-specifier) (rest type-specifier) table))

(defmethod scan-for-simple-types ((type-specifier symbol) table)
  (scan-for-simple-types (find-class type-specifier) table))

(defmethod scan-for-simple-types ((type-specifier class) table)
  (loop for class in (sicl-host-mop:class-precedence-list type-specifier)
        unless (nth-value 1 (gethash class (bit-positions table)))
          do (setf (gethash class (bit-positions table))
                   (logior (gethash class (bit-positions table) 0)
                           (ash 1 (prog1 (next-bit-position table)
                                    (incf (next-bit-position table))))))))
  
(defgeneric scan-for-individual-objects (type-specifier table))

(defgeneric scan-for-individual-objects-compound (head body table))

(defmethod scan-for-individual-objects (type-specifier table)
  nil)

(defmethod scan-for-individual-objects ((type-specifier cons) table)
  (scan-for-individual-objects-compound
   (first type-specifier) (rest type-specifier) table))

(defun scan-body-for-individual-objects (body table)
  (loop for type-specifier in body
        do (scan-for-individual-objects type-specifier table)))

(defmethod scan-for-individual-objects-compound ((head (eql 'and)) body table)
  (scan-body-for-individual-objects body table))

(defmethod scan-for-individual-objects-compound ((head (eql 'or)) body table)
  (scan-body-for-individual-objects body table))

(defmethod scan-for-individual-objects-compound ((head (eql 'not)) body table)
  (scan-for-individual-objects (first body) table))

(defun ensure-individual-object (object table)
  (unless (typep object '(or real array))
    (unless (nth-value 1 (gethash object (individual-objects table)))
      (let ((bit-position (prog1 (next-bit-position table)
                            (incf (next-bit-position table)))))
        (setf (gethash object (individual-objects table))
              bit-position)
        (loop for class being the hash-keys of (bit-positions table)
              when (typep object class)
                do (setf (gethash class (bit-positions table))
                         (logior (gethash class (bit-positions table) 0)
                                 (ash 1 bit-position))))))))

(defmethod scan-for-individual-objects-compound ((head (eql 'eql)) body table)
  (ensure-individual-object (first body) table))

(defmethod scan-for-individual-objects-compound ((head (eql 'member)) body table)
  (loop for object in body
        do (ensure-individual-object object table)))

(defun find-simple-types (type-specifier)
  (let ((table (make-instance 'table)))
    (scan-for-simple-types type-specifier table)
    table))

(defgeneric bits-from-type-specifier (type-specifier table))

(defmethod bits-from-type-specifier ((type-specifier symbol) table)
  (bits-from-type-specifier (find-class type-specifier) table))

(defmethod bits-from-type-specifier ((type-specifier class) table)
  (gethash type-specifier (bit-positions table)))

(defgeneric bits-from-type-specifier-compound (head body table))

(defmethod bits-from-type-specifier ((type-specifier cons) table)
  (bits-from-type-specifier-compound
   (first type-specifier) (rest type-specifier) table))

(defmethod bits-from-type-specifier-compound (head body table)
  0)

(defmethod bits-from-type-specifier-compound ((head (eql 'and)) body table)
  (loop with result = -1
        for type-specifier in body
        do (setf result
                 (logand result
                         (bits-from-type-specifier type-specifier table)))
        finally (return result)))
  
(defmethod bits-from-type-specifier-compound ((head (eql 'or)) body table)
  (loop with result = 0
        for type-specifier in body
        do (setf result
                 (logior result
                         (bits-from-type-specifier type-specifier table)))
        finally (return result)))

(defmethod bits-from-type-specifier-compound ((head (eql 'not)) body table)
  (lognot (bits-from-type-specifier (first body) table)))

(defmethod bits-from-type-specifier-compound ((head (eql 'member)) body table)
  (loop with result = 0
        for object in body
        do (setf result
                 (logior result
                         (gethash object (individual-objects table))))
        finally (return result)))

(defmethod bits-from-type-specifier-compound ((head (eql 'eql)) body table)
  (gethash (first body) (individual-objects table)))
