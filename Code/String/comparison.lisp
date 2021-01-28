(cl:in-package #:sicl-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities.

(defun first-mismatch-simple-simple-char=
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-string string1)
                    (type simple-string string2)
                    (type fixnum start1 end1 start2 end2)
                    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
          for j of-type fixnum from start2 below end2
          unless (char= (schar string1 i) (schar string2 j))
            return i
          finally (return i))))

(defun first-mismatch-simple-simple-char-equal
    (string1 string2 start1 end1 start2 end2)
  (assert (simple-string-p string1))
  (assert (>= start1 0))
  (assert (<= end1 (length string1)))
  (assert (<= start1 end1))
  (assert (simple-string-p string2))
  (assert (>= start2 0))
  (assert (<= end2 (length string2)))
  (assert (<= start2 end2))
  (locally (declare (type simple-string string1)
                    (type simple-string string2)
                    (type fixnum start1 end1 start2 end2)
                    (optimize (speed 3) (safety 0) (debug 0)))
    (loop for i of-type fixnum from start1 below end1
          for j of-type fixnum from start2 below end2
          unless (char-equal (schar string1 i) (schar string2 j))
            return i
          finally (return i))))

(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (and (= (- end1 start1) (- end2 start2))
         (= (first-mismatch-simple-simple-char=
             string1 string2 start1 end1 start2 end2)
            end1))))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (and (= (- end1 start1) (- end2 start2))
         (= (first-mismatch-simple-simple-char-equal
             string1 string2 start1 end1 start2 end2)
            end1))))

(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char=
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.
             (if (= (- end1 start1) (- end2 start2))
                 ;; We also reached the end of STRING2, so the strings
                 ;; are equal.  Therefore STRING1 is not less than
                 ;; STRING2.
                 nil
                 ;; There are more characters left in STRING2, so
                 ;; STRING1 is a prefix of STRING2.
                 pos))
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             nil)
            ((char< (schar string1 pos)
                    (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is less than the character in
             ;; STRING2.
             pos)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             nil)))))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char-equal
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.
             (if (= (- end1 start1) (- end2 start2))
                 ;; We also reached the end of STRING2, so the strings
                 ;; are equal.  Therefore STRING1 is not less than
                 ;; STRING2.
                 nil
                 ;; There are more characters left in STRING2, so
                 ;; STRING1 is a prefix of STRING2.
                 pos))
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             nil)
            ((char-lessp (schar string1 pos)
                         (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is less than the character in
             ;; STRING2.
             pos)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             nil)))))

(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char=
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.  Then either we reached
             ;; the end of STRING2 as well, in which case the two
             ;; strings are equal, or we did NOT reach the end of
             ;; STRING2, and STRING1 is a prefix of STRING2.  In both
             ;; cases, STRING1 is NOT greater than STRING2.
             nil)
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             pos)
            ((char> (schar string1 pos)
                    (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             pos)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is NOT greater than the character in
             ;; STRING2.
             nil)))))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char-equal
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.  Then either we reached
             ;; the end of STRING2 as well, in which case the two
             ;; strings are equal, or we did NOT reach the end of
             ;; STRING2, and STRING1 is a prefix of STRING2.  In both
             ;; cases, STRING1 is NOT greater than STRING2.
             nil)
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             pos)
            ((char-greaterp (schar string1 pos)
                            (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             pos)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is NOT greater than the character in
             ;; STRING2.
             nil)))))

(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char=
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.  Then either we reached
             ;; the end of STRING2 as well, in which case the two
             ;; strings are equal, or we did NOT reach the end of
             ;; STRING2, and STRING1 is a prefix of STRING2.  In both
             ;; cases, STRING1 is less than or equal to STRING2.
             pos)
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             nil)
            ((char< (schar string1 pos)
                    (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is less than the character in
             ;; STRING2.
             pos)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             nil)))))

(defun string-not-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char-equal
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.  Then either we reached
             ;; the end of STRING2 as well, in which case the two
             ;; strings are equal, or we did NOT reach the end of
             ;; STRING2, and STRING1 is a prefix of STRING2.  In both
             ;; cases, STRING1 is less than or equal to STRING2.
             pos)
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             nil)
            ((char-lessp (schar string1 pos)
                         (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is less than the character in
             ;; STRING2.
             pos)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             nil)))))

(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char=
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.
             (if (= (- end1 start1) (- end2 start2))
                 ;; We also reached the end of STRING2, so the strings
                 ;; are equal.  Therefore STRING1 is greater than or
                 ;; equal to STRING2.
                 pos
                 ;; There are more characters left in STRING2, so
                 ;; STRING1 is a prefix of STRING2.
                 nil))
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             pos)
            ((char< (schar string1 pos)
                    (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is less than the character in
             ;; STRING2.
             nil)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             pos)))))

(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char-equal
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.
             (if (= (- end1 start1) (- end2 start2))
                 ;; We also reached the end of STRING2, so the strings
                 ;; are equal.  Therefore STRING1 is greater than or
                 ;; equal to STRING2.
                 pos
                 ;; There are more characters left in STRING2, so
                 ;; STRING1 is a prefix of STRING2.
                 nil))
            ((= (- pos start1) (- end2 start2))
             ;; We did not reach the end of STRING1, but we did reach
             ;; the end of STRING2.  Then STRING1 is strictly greater
             ;; than STRING2.
             pos)
            ((char-lessp (schar string1 pos)
                         (schar string2 (+ start2 (- pos start1))))
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is less than the character in
             ;; STRING2.
             nil)
            (t
             ;; We did not reach the end of either string, and the
             ;; character in STRING1 is greater than the character in
             ;; STRING2.
             pos)))))

(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char=
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.
             (if (= (- end1 start1) (- end2 start2))
                 ;; We also reached the end of STRING2, so the strings
                 ;; are equal.
                 nil
                 ;; There are more characters left in STRING2, so
                 ;; STRING1 is a prefix of STRING2.
                 pos))
            (t
             ;; We did not reach the end of STRING1, so the two strings
             ;; must be different.
             pos)))))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((string1 (string string1))
        (string2 (string string2)))
    (when (null end1) (setf end1 (length string1)))
    (when (null end2) (setf end2 (length string2)))
    (check-bounding-indices string1 start1 end1)
    (check-bounding-indices string2 start2 end2)
    (let ((pos (first-mismatch-simple-simple-char-equal
                string1 string2 start1 end1 start2 end2)))
      (cond ((= pos end1)
             ;; We reached the end of STRING1.
             (if (= (- end1 start1) (- end2 start2))
                 ;; We also reached the end of STRING2, so the strings
                 ;; are equal.
                 nil
                 ;; There are more characters left in STRING2, so
                 ;; STRING1 is a prefix of STRING2.
                 pos))
            (t
             ;; We did not reach the end of STRING1, so the two strings
             ;; must be different.
             pos)))))
