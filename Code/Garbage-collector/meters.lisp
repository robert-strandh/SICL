(cl:in-package #:sicl-gc)

;;; The total number of garbage collections of the nursery.
(defparameter *number-of-nursery-gcs* 0)

;;; The total internal execution time spent marking objects
;;; in the nursery.
(defparameter *nursery-mark-time* 0)

;;; The total internal execution time spent building the cache
;;; for the nuresry liveness information.
(defparameter *nursery-cache-build-time* 0)

;;; The total internal execution time spent fixing up addresses
;;; of nursery objects prior to moving them. 
(defparameter *nursery-fixup-time* 0)

;;; The total internal execution time spent sliding nursery objects.
(defparameter *nursery-slide-time* 0)

;;; The total number of words recovered during all the nursery GCs
;;; done since the beginning of metering.
(defparameter *nursery-recovered-space* 0)

(defun reset-gc-meters ()
  (setf *number-of-nursery-gcs* 0)
  (setf *nursery-cache-build-time* 0)
  (setf *nursery-fixup-time* 0)
  (setf *nursery-slide-time* 0)
  (setf *nursery-recovered-space* 0))
