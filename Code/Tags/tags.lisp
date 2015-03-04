(cl:in-package #:sicl.tags)

;;; Number of bytes in a machine word.
(cl:defparameter +machine-word-length+ 64)

;;; The basic type that raw memory contains. 
(cl:deftype machine-word ()
  `(cl:unsigned-byte ,+machine-word-length+))

;;; Number of bits used for first-order tag
(cl:defparameter +number-of-low-tag-bits+ 3)

;;; A mask of 1s corresponding to the low tag bits
(cl:defparameter +low-tag-mask+
  (cl:1- (cl:round (cl:expt 2 +number-of-low-tag-bits+))))

(cl:defun tag-bits (word)
  (cl:check-type object machine-word)
  (cl:logand word +low-tag-mask+))

(cl:defun raw-pointer (word)
  (cl:check-type object machine-word)
  (cl:logand word (cl:not +low-tag-mask+)))
