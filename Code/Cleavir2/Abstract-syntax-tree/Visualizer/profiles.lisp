(cl:in-package #:cleavir-ast-visualizer)

(defun make-point (x y) (cons x y))

(defun x (point) (car point))

(defun y (point) (cdr point))

(defun profile-width (profile)
  (x (first (last profile))))

(defun profile-height (profile)
  (y (first (last profile))))

;;; This function can be used when the resulting profile is the same
;;; as that of LOWER, except that it has been moved down by DY and
;;; width has changed, so that the X coordinate of the last point is
;;; WIDTH instead.
(defun combine-profiles-with-dy-and-width (lower dy width)
  (values (loop for ((x . y) . rest) on lower
                collect (make-point (if (null rest) width x)
                                    (+ y dy)))
          dy))

(defun combine-vertical-profiles-case-1 (upper lower)
  (combine-profiles-with-dy-and-width
   lower (profile-height upper) (profile-width lower)))

(defun combine-vertical-profiles-case-2 (upper lower)
  (combine-profiles-with-dy-and-width
   lower (profile-height upper) (profile-width upper)))
  
(defun combine-vertical-profiles-case-3a (upper lower suffix)
  (let ((suffix-y (y (first suffix))))
    (combine-profiles-with-dy-and-width
     lower suffix-y (profile-width upper))))

;;; Find the longest suffix of PROFILE such that the second element of
;;; the suffix is greater than Y.
(defun find-suffix-2 (profile y)
  (loop for suffix on profile
        when (> (y (second suffix)) y)
          return suffix))

(defun combine-vertical-profiles-case-3b (upper lower suffix)
  (let* ((suffix-y (y (first suffix)))
         (y-sum (+ (profile-height lower) (y (first suffix))))
         (suffix-suffix (find-suffix-2 suffix y-sum)))
    (multiple-value-bind (prefix dy)
        (combine-profiles-with-dy-and-width
         lower suffix-y (x (first suffix-suffix)))
      (values (append prefix (rest suffix-suffix)) dy))))

(defun combine-vertical-profiles-case-3 (upper lower suffix)
  (let ((suffix-height (- (profile-height upper) (y (first suffix)))))
    (if (<= suffix-height (profile-height lower))
        (combine-vertical-profiles-case-3a upper lower suffix)
        (combine-vertical-profiles-case-3b upper lower suffix))))

;;; Find a suffix of PROFILE such that the X value of the first point
;;; is greater than or equal to X.
(defun find-suffix-1 (profile x)
  (loop for suffix on profile
        when (>= (x (first suffix)) x)
          return suffix))

(defun combine-vertical-profiles (upper lower)
  (let* ((suffix (find-suffix-1 upper (profile-width lower))))
    (cond ((null suffix)
           (combine-vertical-profiles-case-1 upper lower))
          ((null (rest suffix))
           (combine-vertical-profiles-case-2 upper lower))
           
          (t
           (combine-vertical-profiles-case-3 upper lower suffix)))))

;;; LEFT is just a single point for the lower-right corner of the
;;; parent.  RIGHT is the combined profile of the children.
(defun combine-horizontal-profiles (left right)
  (let ((total-width (+ (x left) (profile-width right)))
        (new-right (loop for (x . y)  in right
                         collect (make-point (+ x (x left)) y))))
    (cond ((>= (y left) (profile-height right))
           (values (combine-profiles-with-dy-and-width (list left) 0 total-width)
                   (x left)))
          ((> (y (first right)) (y left))
           (values (cons left new-right) (x left)))
          (t
           (let ((suffix (find-suffix-2 new-right (y left))))
             (values (cons (make-point (x (first suffix)) (y left))
                           (rest suffix))
                     (x left)))))))
