(cl:in-package #:sicl-arithmetic)

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
