(cl:in-package #:sicl-hash-table)

(defclass hash-table ()
  ((%test :initform #'eql
          :accessor %%hash-table-test
          :reader %hash-table-test)
   (%rehash-size :initarg :rehash-size
                 :initform 1.5
                 :reader hash-table-rehash-size)
   (%rehash-threshold :initarg :rehash-threshold
                      :initform 0.8
                      :reader hash-table-rehash-threshold)))

(defgeneric hash-table-p (object)
  (:method (object) nil)
  (:method ((object hash-table)) t))

(defmethod print-object ((table hash-table) stream)
  (print-unreadable-object (table stream :type t :identity t)
    (format stream ":test ~s size ~d/~d"
            (hash-table-test table)
            (hash-table-count table)
            (hash-table-size table))))
