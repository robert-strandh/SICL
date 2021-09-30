(cl:in-package #:sicl-random)

(defmethod seed-random-state ((state pcg32-random) (seed fixnum))
  ;; This algorithm has something they call streams. Supporting this is not
  ;; something I'm going to bother to do since it can't properly be exposed
  ;; through CL's PRNG functions. I set it to 27 here since that's what the
  ;; PCG32 demo seems to do if it's not given a different number. If you find
  ;; this inadequate, feel free to improve it.
  (setf (state-num state) 0
        (increment state) 27)
  (read-random-state state)
  (incf (state-num state) seed)
  (read-random-state state))

(defmethod read-random-state ((state pcg32-random))
  (let ((old-state (state-num state))
        (increment (increment state)))
    (declare (type (unsigned-byte 64) old-state increment))
    (setf (state-num state) (ldb (byte 64 0)
                                 (+ (logior increment 1)
                                    (* old-state
                                       6364136223846793005))))
    (let ((xor-shifted (ldb (byte 32 0)
                            (ash (logxor (ash old-state -18) old-state)
                                 -27)))
          (rotation (ldb (byte 32 0) (ash old-state -59))))
      (declare (type (unsigned-byte 32) xor-shifted rotation))
      (ldb (byte 32 0)
           (logior (ash xor-shifted (- rotation))
                   (ash xor-shifted (logand (- rotation) 31)))))))

(defmethod copy-random-state ((state pcg32-random))
  (make-instance 'pcg32-random
                 :increment (increment state)
                 :state-num (state-num state)))

(defmethod print-object ((object pcg32-random) stream)
  (format stream "#.(make-instance 'sicl-random::pcg32-random :increment ~D :state-num ~S)"
          (increment object) (state-num object)))
