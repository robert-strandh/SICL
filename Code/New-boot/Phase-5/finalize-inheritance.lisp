(cl:in-package #:sicl-new-boot-phase-5)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun finalize-inheritance (client e3 e4)
  (let ((find-class 
          (clo:fdefinition client e4 'find-class))
        (class-finalized-p
          (clo:fdefinition client e3 @clostrophilia:class-finalized-p))
        (finalize-inheritance
          (clo:fdefinition client e3 @clostrophilia:finalize-inheritance)))
    (declare (ignore class-finalized-p))
    (loop for symbol in (list 'standard-class)
          for class = (funcall find-class symbol)
          do (funcall finalize-inheritance class))))
