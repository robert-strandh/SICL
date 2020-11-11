(cl:in-package #:sicl-type)

(defun typep-compound-float (object head rest)
  (unless (same-float-type-p (type-of object) head)
    (return-from typep-compound-float nil))
  ;; OBJECT is definitely a float of the right type.
  (when (null rest)
    ;; The type specifier is (...-FLOAT), so since OBJECT is a float
    ;; of the right type, we are done.
    (return-from typep-compound-float t))
  ;; The type specifier is (...-FLOAT <lower-bound> . ...).
  (let ((lower-bound (first rest)))
    (cond ((floatp lower-bound)
	   (when (< object lower-bound)
	     (return-from typep-compound-float nil)))
	  ((consp lower-bound)
	   (when (<= object (car lower-bound))
	     (return-from typep-compound-float nil)))
	  (t
	   nil)))
  (when (null (rest rest))
    ;; The type specifier is (...-FLOAT <lower-bound>), so since we
    ;; have checked that the lower bound is OK, we are done.
    (return-from typep-compound-float t))
  (let ((upper-bound (second rest)))
    (cond ((floatp upper-bound)
	   (<= object upper-bound))
	  ((consp upper-bound)
	   (< object (car upper-bound)))
	  (t
	   t))))
  
(defmethod typep-compound (object (head (eql 'short-float)) rest)
  nil)

(defmethod typep-compound (object (head (eql 'short-float)) rest)
  (typep-compound-float object head rest))

(defmethod typep-compound (object (head (eql 'single-float)) rest)

(defmethod typep-compound (object (head (eql 'single-float)) rest)
  (typep-compound-float object head rest))

(defmethod typep-compound ((object double-float) (head (eql 'double-float)) rest)
  (typep-compound-float object head rest))

(defmethod typep-compound (object (head (eql 'long-float)) rest)
  nil)

(defmethod typep-compound ((object double-float) (head (eql 'long-float)) rest)
  (typep-compound-float object head rest))
