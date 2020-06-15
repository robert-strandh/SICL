(cl:in-package #:sicl-arithmetic)

(defun convert-fixnum-to-bignum (fixnum)
  (let ((bignum (make-instance 'bignum :limb-count 2))
        (least-significant-limb
          (logand fixnum #.(- (expt 2 62) 1))))
    (cleavir-primop:nook-write bignum 3 least-significant-limb)
    (cleavir-primop:nook-write bignum 4 1))
  (unless (minusp fixnum)
    (setf (sign-and-limb-count bignum) -2)))

        
      
