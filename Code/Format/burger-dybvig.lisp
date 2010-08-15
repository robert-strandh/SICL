;;; First some background.
;;; 
;;; Let us say we are dealing with IEEE 754-2008 binary64 floats to
;;; get a more concrete example.  Such a number uses 1-bit field
;;; containing the representation of the sign of the number, a 52 bit
;;; field representing the mantissa of the number, and a 11-bit field
;;; representing the exponent.  Notice that we distinguish between on
;;; the one hand the sign, the mantissa, and the exponent of the
;;; number, and on the other hand the representation of those
;;; quantities in the memory of the computer.  It is sometimes
;;; convenient to view the contents of the representation as
;;; nonnegative integers, so here we shall use s, m, and e to mean the
;;; sign, the mantissa, and the, exponent of the number.  And we shall
;;; use S, M, and E to mean the contents of the corresponding fields
;;; seen as nonnegative integers.  For instance, the relation between
;;; s and S is that when s is +, S is 0 and when s is - then S is 1.
;;; The other two are more complicated as explained below.
;;; 
;;; There are many possible combinations of mantissa and exponent for
;;; a given number, but the IEEE standard imposes a canonical
;;; representation in the memory of the computer for a given number.
;;; This canonical representation varies according to the size of the
;;; number represented.
;;;
;;; If the number is greater than or equal to 2^-1022 and strictly
;;; less than 2^1024, it uses what is known as a "normalized"
;;; representation.  In that case, the mantissa m is always a number
;;; such that 0.5 <= m < 1.0.  A binary number like that, can be
;;; written 0.1xxxxx..., i.e, the first digit after the binary point
;;; is always a 1.  That digit can therefore be omitted from the
;;; representation, and only the xxxxx... part is stored in the field
;;; used to represent the mantissa.  This allows for a mantissa with
;;; 53-bit precision to be stored in a 52-bit field.  In other words,
;;; the relationship between m and M is that M = (m - 1/2) * 2^53, and
;;; thus m = M/2^53 + 1/2.  So for instance, a representation that has
;;; a 1 followd by 51 0s (E = 2^51) represents an m of 2^51/2^53 + 1/2
;;; = 3/4.  For the normalized representation E = e + 1022.  All this
;;; gives that the smallest normalized positive number (2^-1022 which
;;; is normalized to m = 1/2 and e = -2^1021) is reprsented with M = 0
;;; and E = 1.  The largest normalized positive number has M
;;; containing only 1s, so M = 2^52 - 1 so m is slightly less than 1
;;; and E = 2046 and therefore e = 1022.
;;;
;;; The Common Lisp function decode-float returns m, e, and s as
;;; described above when the number is represented in normalized form. 
;;;
;;; When the absolute value of a number is less than 2^-1022, the
;;; representation is switched to something called "denormalized".
;;; This representation allows for all numbers from 2^-1022
;;; and down to 0 to be represented with increasing loss of precision.
;;; In this representation E = 0.  The mantissa represented is M =
;;; 2^52 * m, and thus m = M/2^52.  The exponent is e = E - 1022
;;;
;;; The Common Lisp function decode-float does *not* return m and e as
;;; described above when the respresentation is denormalized.
;;; Instead, it scales m and e so that 0.5 <= m < 1.0 always holds.
;;;
;;; Positive and negative 0 is represented with M = 0, and E = 0.
;;; This is a natural extension of the denormalized form.  The Common
;;; Lisp function decode-float returns 0 for m in this case. 
;;;
;;; Positive and negative infinites are represented with M = 0 and 
;;; E = 2047.

;;; Consider again the normalized representation.  Whereas the number
;;; represented is uniquely determined by S, M, and E, it is not the
;;; case that m and e as describe above are the only possible
;;; interpretations.  In fact, the number represented is (-1)^S x 2^(E
;;; - 1022) x (M/2^53 + 1/2).  If we divide the middle term by 2^53
;;; and multiply the last one with the same number, we get (-1)^S x
;;; 2^(E - 1075) x (M + 2^52).  These three quantities, namely (M +
;;; 2^52), (E - 1075), and S are are all integers, and we shall assume
;;; below that we have a function that will return those quantities.
;;; For the denormalized representation, in order to obtain integers
;;; from the quantities M/2^52 and E - 1022, it suffises to scal by
;;; 2^52, rather than with 2^53 as was the case with normalized
;;; numbers.  We shall assume that our hypothetical function returns M
;;; and E - 1074.  Notice the difference in the value of the exponent.
;;; With our hypothetical function, we can construct a number m * 2^e
;;; that exactly represents the floating point number as a rational
;;; number.

;;; So what does it mean to print floating-point numbers accurately?
;;; The definition we shall use is the same as that of Burger and
;;; Dybvig.  First let's consider a variable field printed
;;; representation, i.e, we are allowed to have as many digits of
;;; output as needed.  In this case we would like to find the printed
;;; representation with the fewest number of mantissa digits that,
;;; when read back by an accurate floating-point read routine, will
;;; generate the same number as the one we started with.  By an
;;; "accurate floating-point read routine" we mean one that generates
;;; the floating-point value that is closest to the exact (rational)
;;; value represented by the printed representation. 

;;; The main idea in the paper by Burger and Dybvig is to use exact
;;; rational arithmetic to determine how close the printed
;;; representation is to the floating-point value we are trying to
;;; print.  For a particular floating-point number v, they define the
;;; "predecessor" v- as the largest floating-point number which is
;;; smaller than v, and the "successor" v+ as the smallest
;;; floating-point number which is larger than v.  So for instance, if
;;; v is normalized and the corresponding M field does not contain
;;; only 1s, then v+ is obtained by adding a 1 to M(v) and decoding
;;; the result.  Let's see how this works out with our hypothetical
;;; function imagined above.  For a normalized number, this function
;;; returns m = M + 2^52 and e = E - 1075 (the sign is not important).
;;; If M does not have all 1s in it, then for v+, our hypothetical
;;; function returns m+ = (M + 1) + 2^52 and e+ = E - 1075 = e.
;;; Therefore, if v = m * 2^e, then v+ = ((M + 1) + 2^52) * 2^e.  Now
;;; v+ = (M + 2^52 + 1) * 2^e = (m + 1) * 2^e, so we can compute v+
;;; from what our hypothetical function returned by just adding 1 to
;;; the value of m.  This computation is similar if both v and v+ are
;;; denormalized numbers.  The only potential problem is when v is
;;; denormalized and v+ is normalized.  In other words, v is the
;;; largest possible denormalized number and v+ is the smallest
;;; possible normalized number.  In this case m = 2^52 - 1 and the
;;; value of e is -1074.  For v+, our hypothetical function would
;;; return m+ = 2^52 and e+ = -1074.  Again, we can construct v+ by
;;; using m+ = m + 1 and the same e.  Constructing v- from v is more
;;; delicate.  If v is a normalized number with M not being all 0s,
;;; then, we can construct v- from v by subtracting 1 from m.  The
;;; same thing holds if v is denormalized (we treat v = 0 as a special
;;; case).  There only potential problem occurs when v is normalized
;;; and M contains only 0s.  In that case, if v- is also normalized,
;;; our hypothetical function returns m = 2^52 and e for v, whereas it
;;; would have returned m- = 2^53 - 1 and e- = e - 1 for v-.  How can
;;; we compute v- from m and e only?  Simple, just compute v- as (m *
;;; 2 - 1) * 2^(e-1) or m * 2 * 2^(e-1) - 2^(e-1) which is m * 2^e -
;;; 2^(e-1) or v - 2^(e-1).  When v is the smallest normalized number,
;;; then v- is the largest denormalized number.  In that case m =
;;; 2^52, e = -1074, m- = 2^52 - 1 and e- = -1074, so the only special
;;; case here is when v is a normalized number other than the smallest
;;; one such that m is 2^52.

;;; To summarize:
;;;   Let F be our hypothetical function, returning m and e.
;;;   In order to compute v+ from v:
;;;     Let m,e <- F(v)
;;;     return v + 2^e
;;;   In order to compute v- from v:
;;;     Let m,e <- F(v)
;;;     If v > the smallest normalized float in the representation
;;;        and m is a single 1 followed by 0s
;;;     then return v - 2^(e-1)
;;;     else return v - 2^e

;;; As it turns out, we are out of luck.  No function like F exists in
;;; the Common Lisp standard.  The closest one is integer-decode-float
;;; but it is allowed by the standard to introduce some arbitrary
;;; scaling between m and e, ans long as they are both integers and as
;;; long as v = m * 2^e holds.  Interestingly, the special case cited
;;; above still works, i.e., computing v - 2^(e-1) works,
;;; independently of this scaling.  However, adding and subtracting 1
;;; from m is no longer valid if m is scaled.  But if we compute
;;; (m + 1) * 2^e as v + 2^e and (m - 1) * 2^e as v - 2^e, we are
;;; independent of any scaling between m and e. 

;;; The second problem we have is that the test to determine the
;;; special case for v- depends on the which floating-point
;;; representation we have.  We would like to avoid testing for all
;;; possible floating-point precisions, especially since
;;; implementations might want to exted the format to others.  But if
;;; we examine the procedure above for computing v- a little closer it
;;; can be summarized like this:
;;;
;;; if 2^(e-1) is too small, i.e, v - 2^(e-1) would give the same number
;;; back if converted to a float, then use 2^e instead.  So we simply
;;; try with 2^(e-1) first, and if that doesn't work, use 2^e instead. 
;;; This method is independent of the precision of the floating-point 
;;; number, and depends only on IEEE-style representation of normalized
;;; and denormalized numbers. 

;;; Once we have v+ and v- as above, we can compute the average between
;;; v- and v and call it low, and the average between v and v+ and call
;;; it high.  If we have a sequence of decimal digits that represent a
;;; rational value greater than low and smaller than high, then we know
;;; that an accurate floating-point reading routine will read it back
;;; to the exact same value as v. 

;;; The following functions needs to make some assumption about the
;;; way floats are represented.  We shall assume IEEE-style
;;; representation in the form of normalized and denormalized numbers
;;; as described above.

;;; Given a floating-point number x, compute its successor.
;;; The number x must be strictly smaller than the maximum 
;;; floating-point number, and strictly greater than 0. 
(defun successor (x)
  (multiple-value-bind (m e)
      (decode-float x)
    (declare (ignore m))
  (+ x (scale-float (float 1 x) (- e (float-precision x))))))

;;; Given a floating-point number x, compute its predecessor The
;;; number x must be smaller than or equal to the maximum
;;; floating-point number, and strictly greater than 0.
(defun predecessor (x)
  (let ((p (float-precision x)))
    (multiple-value-bind (m e)
	(decode-float x)
      (declare (ignore m))
      (if (and (= (decode-float x) 0.5)
	       (/= (- x (scale-float (float 1 x) (- e 1 p))) x))
	  (- x (scale-float (float 1 x) (- e 1 p)))
	  (- x (scale-float (float 1 x) (- e p)))))))

;;; This function goes through all single floats and checks that for a
;;; given float x, the successor of the predecessor x is x, as defined
;;; by the two precedging functions. 
(defun test-all ()
  (loop for x = most-positive-single-float then y
	for y = (predecessor x)
	for i from 0
	until (= y 0)
	do (when (zerop (mod i 1000000))
	     (print x *trace-output*)
	     (finish-output *trace-output*))
	do (assert (= x (successor y)))))

;;; Given a rational r, return the smallest integer k such that 
;;; r < 10^k.  We use the floating-point logarithmic function
;;; to find an approximate value of k, then we find the exact
;;; one by a small search around the appriximation.
(defun scale (r)
  (let* ((try (1- (ceiling (log r 10))))
	 (expt (expt 10 try)))
    (loop while (<= r expt)
	  do (decf try)
	     (setf expt (/ expt 10)))
    (loop until (<= r expt)
	  do (incf try)
	     (setf expt (* expt 10)))
    try))

;;; This is a trivial and inefficient implementation of
;;; the first algorithm in the Burger-Dybvig paper.  It is used
;;; for testing-purposes only.  It returns two values, a list
;;; of numbers that represent the individual digits of the result, 
;;; and the scale factor. 

(defun burger-dybvig-1 (x)
  (let* ((v (rational x))
	 (v- (rational (predecessor x)))
	 (v+ (rational (successor x)))
	 (low (/ (+ v v-) 2))
	 (high (/ (+ v v+) 2))
	 (scale (scale high)))
    (loop with value = 0
	  with result = '()
	  for factor = 1/10 then (/ factor 10)
	  for q = (/ v (expt 10 scale)) then (- (* q 10) (floor (* q 10)))
	  for d = (floor (* q 10))
	  do (let ((low-out (* (+ value (* factor d)) (expt 10 scale)))
		   (high-out (* (+ value (* factor (1+ d))) (expt 10 scale))))
	       (cond ((and (> low-out low)
			   (>= high-out high))
		      (push d result) (loop-finish))
		     ((and (<= low-out low)
			   (< high-out high))
		      (push (1+ d) result) (loop-finish))
		     ((and (> low-out low)
			   (< high-out high))
		      (cond ((< (abs (- v low-out))
				(abs (- v high-out)))
			     (push d result))
			    ((< (abs (- v high-out))
				(abs (- v low-out)))
			     (push (1+ d) result))
			    (t ;; beak the tie
			     (push (1+ d) result)))
		      (loop-finish))
		     (t
		      (push d result) (incf value (* factor d)))))
	  finally (return (values (nreverse result) scale)))))

;;; This function uses a very direct method to generate floating
;;; point digits.  It uses floating-point arithmetic, so it may
;;; not be accurate, but it is an order of magnitude faster than
;;; the Burger & Dybvig algorithm.   It would be very easy, and
;;; not too costly to check whether this function generates
;;; a result that will read back in accurately.  In many
;;; cases it would not be the shortest possible external
;;; representation with that property, but that might be good
;;; enough for some people.  For that reason, we might suggest
;;; an extension to the CL standard where a special variable
;;; is used to control whether fast printing should be used
;;; instead. 
(defun generate-digits-using-fp-arithmetic (x)
  (let* ((v- (predecessor x))
	 (v+ (successor x))
	 (s (scale (/ (+ (rational x) (rational v+)) 2)))
	 (scale (expt 10 (1- s))) 
	 (d- (- (/ (/ (- x v-) 2d0) scale)))
	 (d+ (/ (/ (- v+ x) 2d0) scale))
	 (scaled (/ x scale)))
    (values (loop with result = '()
		  do (let ((digit (floor scaled)))
		       (cond ((< (- scaled digit) d+)
			      (push digit result)
			      (loop-finish))
			     ((> (- scaled (1+ digit)) d-)
			      (push (1+ digit) result)
			      (loop-finish))
			     (t
			      (push digit result)
			      (setf scaled (* 10 (- scaled digit)))
			      (setf d- (* d- 10))
			      (setf d+ (* d+ 10)))))
		  finally (return (nreverse result)))
	    s)))

(defun int-1 (x)
  (let* ((v- (predecessor x))
	 (v+ (successor x))
	 (s (scale (/ (+ (rational x) (rational v+)) 2)))
	 (scale (expt 10 (1- s))) 
	 (d- (- (/ (/ (rational (- x v-)) 2) scale)))
	 (d+ (/ (/ (rational (- v+ x)) 2) scale))
	 (scaled (/ (rational x) scale)))
    (values (loop with result = '()
		  do (let ((digit (floor scaled)))
		       (cond ((< (- scaled digit) d+)
			      (push digit result)
			      (loop-finish))
			     ((> (- scaled (1+ digit)) d-)
			      (push (1+ digit) result)
			      (loop-finish))
			     (t
			      (push digit result)
			      (setf scaled (* 10 (- scaled digit)))
			      (setf d- (* d- 10))
			      (setf d+ (* d+ 10)))))
		  finally (return (nreverse result)))
	    s)))

;;; This is a direct implementation of the second algorithm of the 
;;; Burger & Dybvig paper.  It is not modeled after their Scheme code,
;;; but reimplements the algorithm they present in Common Lisp.
(defun burger-dybvig-2 (x)
  (multiple-value-bind (f e)
      (integer-decode-float x)
    ;; adjust mantissa and exponent
    (let ((diff (- (float-precision x) (float-digits x))))
      (setf f (* f (expt 2 diff)))
      (decf e diff))
    (let (r s m+ m-)
      (if (>= e 0)
	  (progn (if (= (decode-float x) 0.5)
		     (setf m- (expt 2 e)
			   m+ (* m- 2)
			   s 4
			   r (* f m+  2))
		     (setf m- (expt 2 e)
			   m+ m-
			   s 2
			   r (* f m+ 2))))
	  (progn (if (and (= (decode-float x) 0.5)
			  (= (float-precision x)
			     (float-precision (predecessor x))))
		     (setf m- 1
			   m+ 2
			   s (* (expt 2 (- 1 e)) 2)
			   r (* f 4))
		     (setf m- 1
			   m+ 1
			   s (* (expt 2 (- e)) 2)
			   r (* f 2)))))
      (let ((k (scale (/ (+ r m+) s))))
	(if (>= k 0)
	    (setf s (* s (expt 10 k)))
	    (let ((coeff (expt 10 (- k))))
	      (setf r (* r coeff)
		    m+ (* m+ coeff)
		    m- (* m- coeff))))
	(loop with result = '()
	      do (multiple-value-bind (quotient remainder)
		     (floor (* r 10) s)
		   (setf r remainder
			 m+ (* m+ 10)
			 m- (* m- 10))
		   (if (and (>= r m-) (<= (+ r m+) s))
		       (push quotient result)
		       (progn (push (+ quotient
				       (if (< r m-)
					   (if (> (+ r m+) s)
					       ;; break the tie
					       (if (< (* 2 r) s) 0 1)
					       0)
					   1))
				    result)
			      (loop-finish))))
	      finally (return (values (nreverse result) k)))))))

;;; Test that the two implemetations above give the same result
;;; for all single floats.  Running this test may take a few days
;;; on a good 64-bit machine.
(defun test-dybvig-2 ()
  (loop for x = (predecessor most-positive-single-float) then (predecessor x)
	for i from 0
	do (when (zerop (mod i 1000000))
	     (format *trace-output* "~s~%" x)
	     (finish-output *trace-output*))
	do (multiple-value-bind (d1 k1)
	       (burger-dybvig-1 x)
	     (multiple-value-bind (d2 k2)
		 (burger-dybvig-2 x)
	       (when (not (and (equal d1 d2)
			       (= k1 k2)))
		 (format *trace-output* "no: ~s~%" x))))))
