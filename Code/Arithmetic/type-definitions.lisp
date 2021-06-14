(cl:in-package #:sicl-arithmetic)

(deftype mod (n)
  `(integer 0 (,n)))

(deftype unsigned-byte (&optional s)
  (if (eq s '*)
      `(integer 0 *)
      `(integer 0 ,(1- (expt 2 s)))))

(deftype signed-byte (&optional s)
  (if (eq s '*)
      `(integer * *)
      `(integer ,(- (expt 2 (1- s))) ,(1- (expt 2 (1- s))))))

(deftype bit ()
  '(unsigned-byte 1))
