(cl:in-package #:sicl-string)

(defmacro with-canonicalized-and-checked-strings
    (((string1-var string1-form)
      (start1-var start1-form)
      (end1-var end1-form)
      (string2-var string2-form)
      (start2-var start2-form)
      (end2-var end2-form))
     &body body)
  `(let ((,string1-var (string ,string1-form))
         (,start1-var ,start1-form)
         (,end1-var ,end1-form)
         (,string2-var (string ,string2-form))
         (,start2-var ,start2-form)
         (,end2-var ,end2-form))
     (with-checked-bounding-indices
         ((,string1-var ,string1-var)
          (,start1-var ,start1-var)
          (,end1-var ,end1-var)))
       (with-checked-bounding-indices
           ((,string2-var ,string2-var)
            (,start2-var ,start2-var)
            (,end2-var ,end2-var))
         ,@body)))

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char=
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (null pos))))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char-equal
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (null pos))))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char=
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (not (null pos)))))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char-equal
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (not (null pos)))))

;;; For each of the following functions, we determine 4 cases
;;; according to the return value of the call to MISMATCH.
;;;
;;; 1. MISMATCH returned NIL.  Then the two intervals are of the same
;;;    length and every position contains the same character, so the
;;;    two are equal.
;;;
;;; 2. The position returned is equal to END1.  Then every character
;;;    in the interval in STRING1 matches the corresponding character
;;;    in the interval of STRING2, but the interaval of STRING2 has
;;;    more characters, so STRING1 is a prefix of STRING2.
;;;
;;; 3. The position returned is such that END2 was reached.  Then
;;;    every character in the interval in STRING2 matches the
;;;    corresponding character in the interval of STRING1, but the
;;;    interaval of STRING1 has more characters, so STRING2 is a
;;;    prefix of STRING1.
;;;
;;; 4. The position returned corresponds to a position inside both
;;;    intervals.  Then we compare the two characters in the
;;;    respective positions.

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char=
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) nil)
            ((= pos end1) t)
            ((= (- pos start1) (- end2 start2)) nil)
            (t (char< (char string1 pos)
                      (char string2 (+ (- pos start1) start2))))))))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char-equal
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) nil)
            ((= pos end1) t)
            ((= (- pos start1) (- end2 start2)) nil)
            (t (char-lessp (char string1 pos)
                           (char string2 (+ (- pos start1) start2))))))))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char=
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) nil)
            ((= pos end1) nil)
            ((= (- pos start1) (- end2 start2)) t)
            (t (char> (char string1 pos)
                      (char string2 (+ (- pos start1) start2))))))))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char-equal
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) nil)
            ((= pos end1) nil)
            ((= (- pos start1) (- end2 start2)) t)
            (t (char-greaterp (char string1 pos)
                              (char string2 (+ (- pos start1) start2))))))))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char=
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) t)
            ((= pos end1) t)
            ((= (- pos start1) (- end2 start2)) nil)
            (t (char<= (char string1 pos)
                       (char string2 (+ (- pos start1) start2))))))))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char-equal
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) t)
            ((= pos end1) t)
            ((= (- pos start1) (- end2 start2)) nil)
            (t (char-not-greaterp (char string1 pos)
                                  (char string2 (+ (- pos start1) start2))))))))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char=
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) t)
            ((= pos end1) nil)
            ((= (- pos start1) (- end2 start2)) t)
            (t (char>= (char string1 pos)
                       (char string2 (+ (- pos start1) start2))))))))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (with-canonicalized-and-checked-strings
      ((string1 string1) (start1 start1) (end1 end1)
       (string2 string2) (start2 start2) (end2 end2))
    (let ((pos (mismatch string1 string2
                         :test #'char-equal
                         :start1 start1 :end1 end1
                         :start2 start2 :end2 end2)))
      (cond ((null pos) t)
            ((= pos end1) nil)
            ((= (- pos start1) (- end2 start2)) t)
            (t (char-not-lessp (char string1 pos)
                               (char string2 (+ (- pos start1) start2))))))))
