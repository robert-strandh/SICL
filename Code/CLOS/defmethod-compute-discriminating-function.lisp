(in-package #:sicl-clos)

;;; Usually, when this function is called, the call history is going
;;; to be empty, so the initial cache that is computed is going to
;;; consist of a call to HANDLE-CACHE-MISS.  But we don't want to
;;; exclude the possibility of the call history containing entries, so
;;; we make an expclicit call to COMPUTE-CACHE, rather than just
;;; initializing the cache to #'HANDLE-CACHE-MISS.
(defmethod compute-discriminating-function
    ((generic-function standard-generic-function))
  ;; It is important that the cache function not close over any local
  ;; variables, because we are going to install cache functions that
  ;; are the result of compiling lambda expressions in a null lexical
  ;; environment.
  (let* ((specializer-profile (specializer-profile generic-function))
	 (call-history (call-history generic-function))
	 (cache (compute-cache-function specializer-profile call-history)))
    (setf (set-cache generic-function)
	  (lambda (new-cache) (setf cache new-cache)))
    (lambda (&rest args)
      (funcall cache args generic-function))))
