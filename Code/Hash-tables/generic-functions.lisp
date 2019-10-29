(cl:in-package #:sicl-hash-table)

(defgeneric hash-table-p (object)
  (:method (object) nil)
  (:method ((object hash-table)) t))

(defgeneric hash-table-count (hash-table))

(defgeneric hash-table-rehash-size (hash-table))

(defgeneric hash-table-rehash-threshold (hash-table))

(defgeneric hash-table-size (hash-table))

(defgeneric hash-table-test (hash-table))

(defgeneric gethash (key hash-table &optional default))

(defgeneric (setf gethash) (new-value key hash-table &optional default))

(defgeneric remhash (key hash-table))

(defgeneric maphash (function hash-table))

(defgeneric clrhash (hash-table))

;;; Internal
(defgeneric make-hash-table-iterator (hash-table))
