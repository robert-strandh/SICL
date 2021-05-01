(cl:in-package #:sicl-array)

(defmethod row-major-aref (object index)
  (error 'object-must-be-an-array
         :datum object
         :expected-type 'array))

(defmethod row-major-aref :before ((array array) index)
  (unless (and (numberp index)
               (>= index 0)
               (< index (array-total-size array)))
    (error 'row-major-index-must-be-non-negative-and-less-than-total-size
           :array array
           :datum index
           :expected-type `(integer 0 (,(array-total-size array))))))

(defmethod (setf row-major-aref) :before (new-element (array array) index)
  (declare (ignore new-element))
  (unless (and (numberp index)
               (>= index 0)
               (< index (array-total-size array)))
    (error 'row-major-index-must-be-non-negative-and-less-than-total-size
           :array array
           :datum index
           :expected-type `(integer 0 (,(array-total-size array))))))

(defmethod row-major-aref ((array array-t) index)
  (cleavir-primop:aref array
                       index
                       t
                       t
                       t))

(defmethod (setf row-major-aref) (new-element (array array-t) index)
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              t
                              t
                              t)
         new-element))

(defmethod row-major-aref ((array array-double-float) index)
  (cleavir-primop:aref array
                       index
                       double-float
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-double-float) index)
  (unless (typep new-element 'double-float)
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type 'double-float))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              double-float
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-single-float) index)
  (cleavir-primop:aref array
                       index
                       single-float
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-single-float) index)
  (unless (typep new-element 'single-float)
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type 'single-float))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              single-float
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-signed-byte-64) index)
  (cleavir-primop:aref array
                       index
                       (signed-byte 64)
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-signed-byte-64) index)
  (unless (typep new-element '(signed-byte 64))
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type '(signed-byte 64)))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              (signed-byte 64)
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-unsigned-byte-64) index)
  (cleavir-primop:aref array
                       index
                       (unsigned-byte 64)
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-unsigned-byte-64) index)
  (unless (typep new-element '(unsigned-byte 64))
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type '(unsigned-byte 64)))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              (unsigned-byte 64)
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-signed-byte-32) index)
  (cleavir-primop:aref array
                       index
                       (signed-byte 32)
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-signed-byte-32) index)
  (unless (typep new-element '(signed-byte 32))
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type '(signed-byte 32)))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              (signed-byte 32)
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-unsigned-byte-32) index)
  (cleavir-primop:aref array
                       index
                       (unsigned-byte 32)
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-unsigned-byte-32) index)
  (unless (typep new-element '(unsigned-byte 32))
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type '(unsigned-byte 32)))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              (unsigned-byte 32)
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-unsigned-byte-8) index)
  (cleavir-primop:aref array
                       index
                       (unsigned-byte 8)
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-unsigned-byte-8) index)
  ;; FIXME ucomment when we have a DEFTYPE form for UNSIGNED-BYTE.
  ;; (unless (typep new-element '(unsigned-byte 8))
  ;;   ;; FIXME: signal a more specific condition.
  ;;   (error 'type-error
  ;;          :datum new-element
  ;;          :expected-type '(unsigned-byte 8)))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              (unsigned-byte 8)
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-bit) index)
  (cleavir-primop:aref array
                       index
                       bit
                       t
                       nil))

(defmethod (setf row-major-aref) (new-element (array array-bit) index)
  (unless (typep new-element 'bit)
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type 'bit))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              bit
                              t
                              nil)
         new-element))

(defmethod row-major-aref ((array array-character) index)
  (cleavir-primop:aref array
                       index
                       character
                       t
                       t))

(defmethod (setf row-major-aref) (new-element (array array-character) index)
  (unless (typep new-element 'character)
    ;; FIXME: signal a more specific condition.
    (error 'type-error
           :datum new-element
           :expected-type 'character))
  (progn (cleavir-primop:aset array
                              index
                              new-element
                              character
                              t
                              t)
         new-element))
