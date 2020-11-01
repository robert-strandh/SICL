(cl:in-package #:sicl-arithmetic)

;;; When two fixnums are added and there is an overflow, the result is
;;; a fixnum that is negative when the addition of two positive
;;; fixnums resulted in the overflow, and that is positive when the
;;; addition of two negative fixnums resulted in undeflow.  This
;;; function takes such a resulting fixnum and converts it to a bignum
;;; with the correct sign and magnitude according to the sign and
;;; magnitude of the resulting fixnum.

(defun convert-fixnum-to-bignum (fixnum)
  (let ((bignum (make-instance (if (minusp fixnum)
                                   'positive-fixnum
                                   'negative-fixnum)
                  :limb-count 2))
        (least-significant-limb
          (logand fixnum #.(- (expt 2 62) 1))))
    (cleavir-primop:nook-write bignum 3 least-significant-limb)
    (cleavir-primop:nook-write bignum 4 1)
    bignum))
