(in-package #:cleavir-liveness)

;;; Class for holding liveness information.
(defclass liveness ()
  ((%before :initarg :before :reader before)
   (%after :initarg :after :reader after)))

;;; Traverse/domain for kildall.
;;; Lists SEEM to be okay
(defclass liveness-traverse
    (cleavir-kildall:iterate-mixin
     cleavir-kildall:start-everywhere-mixin)
  ())

(defmethod cleavir-kildall:make-pool
    ((s liveness-traverse) instruction)
  (declare (ignore instruction))
  nil)

(defmacro do-all-predecessors ((name instruction) &body body)
  `(loop for ,name in (cleavir-ir:predecessors ,instruction)
         do (tagbody ,@body)))

(defvar *input-sets*)
(defvar *output-sets*)

;;; Given a hash table whose values are an unbroken range of positive
;;; fixnums including zero, return a vector version of the inverse
;;; mapping of the table.  Like: {"hello" -> 1, "world" -> 0} becomes
;;; #("world" "hello").
(defun hash-to-vector (hash)
  (declare (type hash-table hash)
           (optimize (speed 3)))
  (let ((vector (make-array (hash-table-count hash))))
    (loop for index fixnum being the hash-values in hash
            using (hash-key datum)
          do (setf (aref vector index) datum))
    vector))

(defun insert-sorted (item list)
  (declare (optimize speed)
           (type fixnum item))
  (cond ((null list) (list item))
        ((< item (the fixnum (car list))) (list* item list))
        ((= item (the fixnum (car list))) list)
        (t
         (loop for sub of-type cons on list
               when (or (null (rest sub))
                        (< item (the fixnum (car (rest sub)))))
                 do (setf (cdr sub) (cons item (cdr sub)))
                 and return list
               else when (= item (the fixnum (car (rest sub))))
                      return list))))

(defmethod cleavir-kildall:kildall :around
    ((specialization liveness-traverse) initial &key)
  (let ((*input-sets* (make-hash-table :test #'eq))
        (*output-sets* (make-hash-table :test #'eq))
        (ids (make-hash-table :test #'eq)))
    (let ((next-id 0))
      (declare (type fixnum next-id))
      (cleavir-ir:map-instructions-arbitrary-order
       (lambda (i)
         (labels ((register (datum)
                    (multiple-value-bind (id p)
                        (gethash datum ids)
                      (cond (p id)
                            (t (prog1
                                   (setf (gethash datum ids) next-id)
                                 (if (= next-id most-positive-fixnum)
                                     (error "BUG: Ran out of IDs in liveness?!")
                                     (incf next-id)))))))
                  (getlist (data)
                    (loop with result = nil
                          for datum in data
                          when (cleavir-ir:variable-p datum)
                            do (setf result (insert-sorted (register datum) result))
                          finally (return result))))
           (setf (gethash i *input-sets*)
                 (getlist (cleavir-ir:inputs i))
                 (gethash i *output-sets*)
                 (getlist (cleavir-ir:outputs i)))))
       initial))
    (let ((idvec (hash-to-vector ids))
          (results (call-next-method)))
      (maphash
       (lambda (k v)
         (setf (gethash k results)
               (mapcar (lambda (id) (aref idvec id)) v)))
       results)
      results)))

(defmethod cleavir-kildall:transfer
    ((specialization liveness-traverse) instruction)
  (let* ((pool (cleavir-kildall:maybe-instruction-pool s instruction))
         (inputs (gethash instruction *input-sets*))
         (outputs (gethash instruction *output-sets*))
         (new (sset-union inputs (sset-difference pool outputs))))
    (do-all-predecessors (pred instruction)
      (let ((old (cleavir-kildall:maybe-instruction-pool s pred)))
        (unless (subssetp new old)
          (setf (cleavir-kildall:dictionary-pool pred)
                (sset-union new old))
          (cleavir-kildall:add-work pred))))))

(defun liveness (initial-instruction)
  (let* ((s (make-instance 'liveness-traverse))
         (after (cleavir-kildall:kildall s initial-instruction))
         (before (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (let* ((after-set (cleavir-kildall:instruction-pool i after))
              (before-set
                (union (set-difference
                        after-set
                        (remove-if-not #'cleavir-ir:variable-p
                                       (cleavir-ir:outputs i)))
                       (remove-if-not #'cleavir-ir:variable-p
                                      (cleavir-ir:inputs i)))))
         (setf (gethash i before) before-set)))
     initial-instruction)
    (make-instance 'liveness :before before :after after)))

(defun live-before (liveness node)
  (gethash node (before liveness)))

(defun live-after (liveness node)
  (cleavir-kildall:instruction-pool node (after liveness)))
