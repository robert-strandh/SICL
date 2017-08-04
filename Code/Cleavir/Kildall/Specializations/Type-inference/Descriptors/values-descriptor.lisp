(in-package #:cleavir-type-descriptors)

;;; Values descriptors are lists.
;;; The first element is whether the number of values is fixed.
;;; The remaining elements are the descriptors of the required
;;;  values, in order.
;;; (values a b c &rest t) => (T A B C)
;;; (values a b c &rest nil) => (NIL A B C)
;;; (values a b &optional c &rest nil) => (T A B)
;;; (values a b &optional c &rest t) => (T A B)
;;; (values a &rest b) => (T A)
;;; In other words, we don't care about &optional or weird &rest.

;;; All the descriptors are ltypes, for load order convenience

;;; imprecise, but good enough
(deftype values-descriptor ()
  '(cons (eql values)))

(declaim (inline values-data values-rest-p values-required
                 make-values values-top values-bottom values-nth
                 values-descriptor-top-p values-descriptor-bottom-p
                 values-required-count approximate-values))

(defun values-data (values-descriptor) (rest values-descriptor))

(defun values-rest-p (values-descriptor)
  (first (values-data values-descriptor)))

(defun values-required (values-descriptor)
  (rest (values-data values-descriptor)))

(defun make-values (rest-p required)
  (list* 'values rest-p required))

(defun values-descriptor-p (descriptor)
  (and (consp descriptor) (eq (car descriptor) 'values)))

;;; return a values top type
(defun values-top () (make-values t ()))

(defun values-bottom () (make-values nil ()))

(defun values-required-count (values-descriptor)
  (length (values-required values-descriptor)))

(defun values-nth (values-descriptor n env)
  (if (>= n (values-required-count values-descriptor))
      (if (values-rest-p values-descriptor)
          (ltype-top env)
          (ltype-bottom env))
      (nth n (values-required values-descriptor))))

;;; does the type not have any information?
(defun values-top-p (values-descriptor environment)
  (flet ((top (descriptor) (ltype-top-p descriptor environment)))
    (and (values-rest-p values-descriptor)
         (every #'top (values-required values-descriptor)))))

(defun values-bottom-p (values-descriptor environment)
  (flet ((bottom (descriptor)
           (ltype-bottom-p descriptor environment)))
    (some #'bottom (values-required values-descriptor))))

(defun values->descriptor
    (required optional rest environment)
  (make-values
   (if (or optional (not (ltype-bottom-p rest environment)))
       t
       nil)
   required))

(defun values-binary-meet (v1 v2 env)
  (let ((nv1 (values-required-count v1))
	(nv2 (values-required-count v2))
	max)
    (cond ((< nv1 nv2)
	   (if (values-rest-p v1)
	       (setf max nv2)
	       (return-from values-binary-meet (values-bottom))))
	  ((< nv2 nv1)
	   (if (values-rest-p v2)
	       (setf max nv1)
	       (return-from values-binary-meet (values-bottom))))
	  (t ; equal
	   (setf max nv1)))
    (make-values (and (values-rest-p v1) (values-rest-p v2))
                 (loop for n from 0 below max
                       collect (ltype-meet
                                (values-nth v1 n env)
                                (values-nth v2 n env))))))

(defun values-binary-join (v1 v2 env)
  (let* ((nv1 (values-required-count v1))
	 (nv2 (values-required-count v2))
	 (min (min nv1 nv2)))
    (make-values
     (or (/= nv1 nv2) (values-rest-p v1) (values-rest-p v2))
     (loop for n from 0 below min
           collect (ltype-join (values-nth v1 n env)
                               (values-nth v2 n env))))))

;;; Not sure if correct.
(defun sub-values-p (v1 v2 env)
  (let ((nv1 (values-required-count v1))
        (rp1 (values-rest-p v1))
        (rp2 (values-rest-p v2)))
    (and (or rp2 (not rp1))
         (loop for n below nv1
               always (subltypep (values-nth v1 n env)
                                 (values-nth v2 n env))))))

;;; FIXME: ltypes are not types
(defun values-descriptor->type (values-descriptor env)
  `(values ,@(mapcar (lambda (spec) (ltype->specifier spec env))
                     (values-required values-descriptor))
	   &rest ,(values-rest-p values-descriptor)))
