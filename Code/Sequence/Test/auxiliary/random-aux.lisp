;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Jun  8 06:56:15 2003
;;;; Contains: Aux. functions and macros used for randomization



(declaim (special +standard-chars+ *cl-symbols-vector*))

(defvar *maximum-random-int-bits*
  (max 36 (1+ (integer-length most-positive-fixnum))))

(defun random-from-seq (seq)
  "Generate a random member of a sequence."
  (let ((len (length seq)))
    (assert (> len 0))
    (elt seq (random len))))

(defmacro random-case (&body cases)
  (let ((len (length cases)))
    (assert (> len 0))
    `(case (random ,len)
       ,@(loop for i from 0 for e in cases collect `(,i ,e))
       (t (error "Can't happen?! (in random-case)~%")))))

(defmacro rcase (&body cases)
  "Usage: (RCASE (<weight> <form>+)+), where <weight> is a positive real
   indicating the relative probability of executing the associated implicit
   progn."
  (assert cases)
  (let* ((weights (mapcar #'car cases))
         (cumulative-weights (let ((sum 0))
                               (loop for w in weights collect (incf sum w))))
         (total (car (last cumulative-weights)))
         (r (gensym)))
    (assert (every #'plusp weights))
    (when (typep total 'ratio) (setf total (coerce total 'double-float)))
    `(let ((,r (random ,total)))
       (cond
        ,@(loop for case in (butlast cases)
                for cw in cumulative-weights
                collect `((< ,r ,cw) ,@(cdr case)))
        (t ,@(cdar (last cases)))))))

(defmacro rselect (cumulative-frequency-array &rest cases)
  (let ((len (length cases))
        (a (gensym "A"))
        (max (gensym "MAX"))
        (r (gensym "R"))
        (p (gensym "P"))
        (done (gensym "DONE")))
    (assert (> len 0))
    `(let ((,a ,cumulative-frequency-array))
       (assert (eql ,len (length ,a)))
       (let* ((,max (aref ,a ,(1- len)))
              (,r (random ,max)))
         (block ,done
          ,@(loop for i from 0
                  for c in cases
                  collect
                  `(let ((,p (aref ,a ,i)))
                     (when (< ,r ,p) (return-from ,done ,c))))
          (error "Should not happen!"))))))

(defun make-random-integer-range (&optional var)
  "Generate a list (LO HI) of integers, LO <= HI.  This is used
   for generating integer types."
  (declare (ignore var))
  (rcase
   (1 (flet ((%r () (let ((r (ash 1 (1+ (random *maximum-random-int-bits*)))))
                      (- (random r) (floor (/ r 2))))))
        (let ((x (%r))
              (y (%r)))
          (list (min x y) (max x y)))))
   (1 (let* ((b (ash 1 (1+ (random *maximum-random-int-bits*))))
             (b2 (floor (/ b 2))))
        (let ((x (- (random b) b2))
              (y (- (random b) b2)))
          (list (min x y) (max x y)))))))

(defun random-nonnegative-real ()
  (if (coin 3)
      (random-case
       (/ (random 10000) (1+ (random 1000)))
       (/ (random 1000000) (1+ (random 100000)))
       (/ (random 100000000) (1+ (random 10000000)))
       (/ (random 1000000000000) (1+ (random 10000000))))
    (random (random-case
             1000
             100000
             10000000
             1000000000
             (expt 2.0s0 (random 15))
             (expt 2.0f0 (random 32))
             (expt 2.0d0 (random 32))
             (expt 2.0l0 (random 32))))))

(defun make-random-integer ()
  (let ((r (ash 1 (1+ (random *maximum-random-int-bits*)))))
    (rcase
     (6 (- (random r) (floor (/ r 2))))
     (1 (- r (random (min 10 r))))
     (1 (+ (floor (/ r 2)) (random (min 10 r)))))))

(defun make-random-rational ()
  (let* ((r (ash 1 (1+ (random *maximum-random-int-bits*))))
         (n (random r)))
    (assert (>= r 2))
    (let ((d (loop for x = (random r) unless (zerop x) do (return x))))
      (if (coin) (/ n d) (- (/ n d))))))

(defun make-random-nonnegative-rational ()
  (let* ((r (ash 1 (1+ (random *maximum-random-int-bits*))))
         (n (random r)))
    (assert (>= r 2))
    (let ((d (loop for x = (random r) unless (zerop x) do (return x))))
      (/ n d))))

(defun make-random-positive-rational ()
  (let* ((r (ash 1 (1+ (random *maximum-random-int-bits*))))
         (n (1+ (random r))))
    (assert (>= r 2))
    (let ((d (loop for x = (random r) unless (zerop x) do (return x))))
      (/ n d))))

(defun make-random-bounded-rational (upper-limit lower-inclusive upper-inclusive)
  (assert (rationalp upper-limit))
  (assert (not (minusp upper-limit)))
  (cond
   ((= upper-limit 0) 0)
   ((<= upper-limit 1/1000000)
    (/ (make-random-bounded-rational (* 1000000 upper-limit) lower-inclusive upper-inclusive)
       1000000))
   ((>= upper-limit 1000000)
    (* (random 1000000)
       (make-random-bounded-rational (/ upper-limit 1000000) lower-inclusive upper-inclusive)))
   (t
    (assert (< 1/1000000 upper-limit 1000000))
    (let ((x 0))
      (loop do (setq x (* upper-limit (rational (random 1.0))))
            while (or (and (not lower-inclusive) (zerop x))
                      (and (not upper-inclusive) (= x upper-limit)))
            finally (return x))))))

(defun make-random-float ()
  (rcase
   (1 (random most-positive-short-float))
   (1 (random most-positive-single-float))
   (1 (random most-positive-double-float))
   (1 (random most-positive-long-float))))

(defun make-random-symbol ()
  (rcase
   (3 (random-from-seq #(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
   (2 (random-from-seq *cl-symbols-vector*))
   (1 (gensym))))

(defun random-real ()
  (if (coin) (random-nonnegative-real)
    (- (random-nonnegative-real))))

(defun random-fixnum ()
  (+ (random (1+ (- most-positive-fixnum most-negative-fixnum)))
     most-negative-fixnum))

(defun random-thing (n)
  (if (<= n 1)
      (random-leaf)
    (rcase
     (1 (apply #'cons (mapcar #'random-thing (random-partition (1- n) 2))))
     (1 (apply #'vector (mapcar #'random-thing
                                (random-partition (1- n) (max 10 (1- n))))))
     )))

(defparameter *use-random-byte* t)
(defparameter *random-readable* nil)

(defun make-random-string (size-spec &key simple)
  (let*
      ((size (if (eql size-spec '*) (random 30) size-spec))
       (use-random-byte nil)
       (etype 'character)
       (s (random-case
           (progn
             (setf use-random-byte *use-random-byte*)
             (make-string size :element-type 'character))
           (progn
             (setf use-random-byte *use-random-byte*)
             (make-array size :element-type 'character
                         :initial-element #\a))
           (make-array size :element-type (setf etype (if *random-readable* 'character 'standard-char))
                       :adjustable (and (not simple) (not *random-readable*) (rcase (3 nil) (1 t)))
                       :fill-pointer (and (not simple) (not *random-readable*) (rcase (3 nil) (1 (random (1+ size)))))
                       :initial-element #\a)
           (make-array size :element-type (setf etype (if *random-readable* 'character 'base-char))
                       :adjustable (and (not simple) (not *random-readable*) (rcase (3 nil) (1 t)))
                       :fill-pointer (and (not simple) (not *random-readable*) (rcase (3 nil) (1 (random (1+ size)))))
                       :initial-element #\a))))
    (if (coin)
        (dotimes (i size)
          (setf (char s i) (elt #(#\a #\b #\A #\B) (random 4))))
      (dotimes (i size)
        (setf (char s i)
              (or (and (eql etype 'character)
                       use-random-byte
                       (or (code-char (random (min char-code-limit (ash 1 16))))
                           (code-char (random 256))))
                  (elt "abcdefghijklmnopqrstuvwyxzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
                       (random 62))))))
    (when (and (not simple) (not *random-readable*) (coin 5))
      (let ((len (+ (random (1+ size)) size)))
        (setq s (make-random-string len))
        (setq etype (array-element-type s))
        (setq s (make-array size
                            :element-type etype
                            :displaced-to s
                            :displaced-index-offset (random (1+ (- len size)))))))

    s))

(defun random-leaf ()
  (rcase
   (1 (let ((k (ash 1 (1+ (random 40)))))
        (random-from-interval k (- k))))
   (1 (random-from-seq +standard-chars+))
   (1 (random-real))
   (1 (make-random-string (random 20)))
   (1 (gensym))
   (1 (make-symbol (make-random-string (random 20))))
   (1 (random-from-seq *cl-symbols-vector*))))

(defun random-from-interval (upper &optional (lower (- upper)))
  (+ (random (- upper lower)) lower))

(defun coin (&optional (n 2))
  "Flip an n-sided coin."
  (eql (random n) 0))

;;; Randomly permute a sequence
(defun random-permute (seq)
  (setq seq (copy-seq seq))
  (let ((len (length seq)))
    (loop for i from len downto 2
          do (let ((r (random i)))
               (rotatef (elt seq r) (elt seq (1- i))))))
  seq)

(defun binomial-distribution-test (n fn)
  (let* ((count (loop repeat n count (funcall fn)))
         (sigma (/ (sqrt n) 2.0))
         (bound (* sigma 6))
         (expected (/ n 2.0)))
    (<= (- expected bound)
        count
        (+ expected bound))))

(defun random-partition* (n p)
  "Partition n into p numbers, each >= 0.  Return list of numbers."
  (assert (<= 1 p))
  (cond
   ((= p 1) (list n))
   ((= n 0) (make-list p :initial-element 0))
   (t (let* ((r (random p))
             (n1 (random (1+ n))))
        (cond
         ((= r 0)
          (cons n1 (random-partition* (- n n1) (1- p))))
         ((= r (1- p))
          (append (random-partition* (- n n1) (1- p)) (list n1)))
         (t
          (let* ((n2 (random (1+ (- n n1))))
                 (n3 (- n n1 n2)))
            (append (random-partition* n2 r)
                    (list n1)
                    (random-partition* n3 (- p 1 r))))))))))

(defun random-partition (n p)
  "Partition n into p numbers, each >= 1 (if possible.)"
  (cond
   ((<= n p)
    (make-list p :initial-element 1))
   (t (mapcar #'1+ (random-partition* (- n p) p)))))


;;; Random method combination
;;; Methods in this method combination take a single method qualifier,
;;; which is a positive integer.  Each method is invoked
;;; with probability proportional to its qualifier.
;;;
;;; Inside a method, a throw to the symbol FAIL causes
;;; the application to repeat.  This enables methods to abort
;;; and retry the random selection process.

(defun positive-integer-qualifier-p (qualifiers)
  (typep qualifiers '(cons (integer 1) null)))

(define-method-combination randomized nil ((method-list positive-integer-qualifier-p))
  (assert method-list)
  (let ((clauses (mapcar #'(lambda (method)
                             (let ((weight (car (method-qualifiers method))))
                               `(,weight (call-method ,method))))
                         method-list)))
  `(loop (catch 'fail (return (rcase ,@clauses))))))

