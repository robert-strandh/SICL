(cl:in-package #:sicl-clos)

;;;; Whenever a generic function is initialized or reinitialized,
;;;; whenever a method is added or removed from a generic function, we
;;;; invalidate the discriminating function by setting it to a
;;;; function that, when called, recomputes a valid discriminating
;;;; function from the call history and then attempts to call the
;;;; generic function again with the same arguments.

(defun invalidate-discriminating-function (generic-function)
  (set-funcallable-instance-function
   generic-function
   (lambda (&rest arguments)
     (set-funcallable-instance-function
      generic-function
      (compute-discriminating-function generic-function))
     (apply generic-function arguments))))
