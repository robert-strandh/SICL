(cl:in-package #:sicl-sequence)

(define-sealable-generic-function copy-seq
    (sequence))

(define-sealable-generic-function elt
    (sequence index)
  (:argument-precedence-order sequence))

(define-sealable-generic-function (setf elt)
    (value sequence index)
  (:argument-precedence-order sequence))

(define-sealable-generic-function fill
    (sequence item &key start end)
  (:argument-precedence-order sequence))

(define-sealable-generic-function make-sequence-like
    (sequence length &key initial-element initial-contents)
  (:argument-precedence-order sequence))

(define-sealable-generic-function adjust-sequence
    (sequence length &key initial-element initial-contents)
  (:argument-precedence-order sequence))

(define-sealable-generic-function subseq
    (sequence start &optional end)
  (:argument-precedence-order sequence))

(define-sealable-generic-function (setf subseq)
    (value sequence start &optional end)
  (:argument-precedence-order sequence))

(define-sealable-generic-function map-into
    (result-sequence function &rest sequences)
  (:argument-precedence-order result-sequence))

(define-sealable-generic-function reduce
    (function sequence &key key from-end start end initial-value)
  (:argument-precedence-order sequence))

(define-sealable-generic-function count
    (item sequence &key from-end start end key test test-not)
  (:argument-precedence-order sequence))

(define-sealable-generic-function count-if
    (predicate sequence &key from-end start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function count-if-not
    (predicate sequence &key from-end start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function length
    (sequence))

(define-sealable-generic-function reverse
    (sequence))

(define-sealable-generic-function nreverse
    (sequence))

(define-sealable-generic-function sort
    (sequence predicate &key key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function stable-sort
    (sequence predicate &key key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function find
    (item sequence &key from-end test test-not start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function find-if
    (predicate sequence &key from-end start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function find-if-not
    (predicate sequence &key from-end start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function position
    (item sequence &key from-end test test-not start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function position-if
    (predicate sequence &key from-end start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function position-if-not
    (predicate sequence &key from-end start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function search
    (sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2)
  (:argument-precedence-order sequence-1 sequence-2))

(define-sealable-generic-function mismatch
    (sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2)
  (:argument-precedence-order sequence-1 sequence-2))

(define-sealable-generic-function replace
    (sequence-1 sequence-2 &key start1 end1 start2 end2)
  (:argument-precedence-order sequence-1 sequence-2))

(define-sealable-generic-function substitute
    (newitem olditem sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function substitute-if
    (newitem predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function substitute-if-not
    (newitem predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function nsubstitute
    (newitem olditem sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function nsubstitute-if
    (newitem predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function nsubstitute-if-not
    (newitem predicate sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function remove
    (item sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function remove-if
    (test sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function remove-if-not
    (test sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function delete
    (item sequence &key from-end test test-not start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function delete-if
    (test sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function delete-if-not
    (test sequence &key from-end start end count key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function remove-duplicates
    (sequence &key from-end test test-not start end key)
  (:argument-precedence-order sequence))

(define-sealable-generic-function delete-duplicates
    (sequence &key from-end test test-not start end key)
  (:argument-precedence-order sequence))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions

(define-sealable-generic-function make-sequence-reader
    (sequence start end from-end terminate)
  (:argument-precedence-order sequence))

(define-sealable-generic-function make-sequence-writer
    (sequence start end from-end terminate)
  (:argument-precedence-order sequence))
