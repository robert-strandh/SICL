(cl:in-package #:cleavir-partial-inlining)

(defmethod inline-one-instruction :around
    (enclose-instruction
     call-instruction
     enter-instruction
     successor-instruction
     mapping)
  (let ((copy (find-in-mapping mapping successor-instruction)))
    (if (null copy)
        '()
        (call-next-method))))

(defun local-location-p (location)
  (eq (gethash location *location-ownerships*)
      *original-enter-instruction*))

(defun translate-location (location mapping)
  (let ((new (find-in-mapping mapping location)))
    (when (and (null new)
               (local-location-p location))
      (setf new (cleavir-ir:new-temporary))
      (add-to-mapping mapping location new))
    new))
