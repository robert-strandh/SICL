(cl:in-package #:cleavir-type-descriptors)

(deftype unboxed-descriptor ()
  '(cons (eql unboxed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DESCRIPTOR-UNBOX and DESCRIPTOR-BOX.
;;;
;;; (unboxed [descriptor]) is a descriptor, but not one that will
;;; ever be returned by approximate- or canonicalize-type.
;;;
;;; -UNBOX returns the descriptor for the unboxed version of the
;;; given descriptor, while -BOX returns the underlying descriptor
;;; of an (unboxed [x]) descriptor.
;;;
;;; Performing lattice operations on these indicates wrong HIR.

(defun descriptor-unbox (descriptor)
  (assert (not (and (consp descriptor)
		    (eq (first descriptor) 'unboxed))))
  `(unboxed ,descriptor))

(defun descriptor-box (descriptor)
  (assert (and (consp descriptor)
	       (eq (first descriptor) 'unboxed)))
  (second descriptor))
