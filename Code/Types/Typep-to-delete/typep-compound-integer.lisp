(cl:in-package #:sicl-type)

(defun typep-compound-integer (object rest)
  (unless (typep object 'integer)
    (return-from typep-compound-integer nil))
  (when (null rest)
    ;; The type specifier is (INTEGER), so since OBJECT is an integer,
    ;; we are done.
    (return-from typep-compound-integer t))
  ;; The type specifier is (INTEGER <lower-bound> . ...).
  (let ((lower-bound (first rest)))
    (cond ((typep lower-bound 'integer)
           (when (< object lower-bound)
             (return-from typep-compound-integer nil)))
          ((consp lower-bound)
           (when (<= object (first lower-bound))
             (return-from typep-compound-integer nil)))
          (t
           nil)))
  (when (null (rest rest))
    ;; The type specifier is (INTEGER <lower-bound>), so since we have
    ;; checked that the lower bound is OK, we are done.
    (return-from typep-compound-integer t))
  (let ((upper-bound (second rest)))
    (cond ((typep upper-bound 'integer)
           (<= object upper-bound))
          ((consp upper-bound)
           (< object (first upper-bound)))
          (t
           t))))
