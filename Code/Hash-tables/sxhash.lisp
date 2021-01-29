(cl:in-package #:sicl-hash-table)

(defconstant +fnv-prime+ 1099511628211)

(declaim (inline %fnv-1a fnv-1a)
         (optimize (speed 3)))
(defun %fnv-1a (last-hash byte)
  (declare ((unsigned-byte 8) byte)
           ((unsigned-byte 62) last-hash))
  (ldb (byte 62 0)
       (logxor (* last-hash +fnv-prime+)
               byte)))

(defun fnv-1a (last-hash &rest bytes)
  (declare ((unsigned-byte 64) last-hash))
  (reduce #'%fnv-1a bytes :initial-value last-hash))

(define-compiler-macro fnv-1a (last-hash &rest bytes)
  (reduce (lambda (last-form argument)
            `(%fnv-1a ,last-form ,argument))
          bytes
          :initial-value last-hash))

(defun eq-hash (last-hash object)
  (typecase object
    ;; Instances of a built-in-class can't change-class, so we can gather some
    ;; entropy from their classes at the very least.
    (cons      (fnv-1a last-hash 1))
    (character (let ((code (char-code object)))
                 (fnv-1a last-hash
                         2
                         (ldb (byte 8 0) code)
                         (ldb (byte 8 8) code))))
    (integer   (fnv-1a last-hash
                       3
                       (ldb (byte 8 0) object)
                       (ldb (byte 8 8) object)))
    (float     (multiple-value-bind (significand exponent)
                   (integer-decode-float object)
                 (fnv-1a last-hash
                         4
                         (ldb (byte 8 0) significand)
                         (ldb (byte 8 8) significand)
                         (ldb (byte 8 0) exponent))))
    ;; The element-type of an array won't change.
    (array     (fnv-1a (equal-hash last-hash (array-element-type object))
                       2))
    (symbol    (equal-hash last-hash (symbol-name object)))
    (t last-hash)))

(defvar *depth* 3)
(defun equal-hash (last-hash object)
  (when (zerop *depth*)
    (return-from equal-hash last-hash))
  (typecase object
    (cons   (let ((*depth* (1- *depth*)))
              (equal-hash (equal-hash last-hash (car object))
                          (cdr object))))
    (symbol (equal-hash (fnv-1a last-hash 1)
                        (symbol-name object)))
    (string (loop for char across object
                  for position below 16
                  for hash = last-hash then (equal-hash last-hash char)
                  finally (return hash)))
    (t      (eq-hash last-hash object))))

(defun equalp-hash (last-hash object)
  (when (zerop *depth*)
    (return-from equalp-hash last-hash))
  (typecase object
    (cons      (let ((*depth* (1- *depth*)))
                 (equalp-hash (equalp-hash last-hash (car object))
                              (cdr object))))
    (symbol    (equal-hash (fnv-1a last-hash 1)
                           (symbol-name object)))
    (character (eq-hash last-hash (char-downcase object)))
    (string    (loop for char across object
                     for position below 16
                     for hash = last-hash then (equalp-hash hash char)
                     finally (return hash)))
    (array     (loop with size = (array-total-size object)
                     for position below size
                     for hash = last-hash
                       then (equalp-hash last-hash
                                         (row-major-aref object position))
                     finally (return hash)))
    (t (eq-hash last-hash object))))

(defvar *sxhash-offset* 14695981039346656037)
(defun sxhash (object)
  (ldb (byte 62 0)
       (equal-hash *sxhash-offset* object)))

(defvar *standard-hash-functions*
  `((eq     . ,#'eq-hash)
    (eql    . ,#'eq-hash)
    (equal  . ,#'equal-hash)
    (equalp . ,#'equalp-hash)))
(defun find-hash-function (name)
  (let ((pair (assoc name *standard-hash-functions*)))
    (if (null pair)
        (error "No hash function found for the test ~s" name)
        (cdr pair))))
