(cl:in-package #:sicl-elf)

(defclass vector-position ()
  ((%bytes :initarg :bytes :reader bytes)
   (%index :initform 0 :initarg :index :accessor index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for extracting signed and unsigned numbers of 1 to 8 bytes

;;; Convert unsigned numbers to signed number.
(defun convert-unsigned-to-signed (value size-in-bits)
  (if (>= value (ash 1 (1- size-in-bits)))
      (- value (ash 1 size-in-bits))
      value))

;;; Extract a single byte.
(defun extract-byte (vector-position)
  (aref (bytes vector-position)
        (prog1 (index vector-position)
          (incf (index vector-position)))))

;;; Functions for extracting little-endian numbers.
(defun extract-little-endian-unsigned (size-in-bits vector-position)
  (if (= size-in-bits 8)
      (extract-byte vector-position)
      (let ((half (ash size-in-bits -1)))
	(+ (extract-little-endian-unsigned half vector-position)
	   (ash (extract-little-endian-unsigned half vector-position) half)))))

(defun extract-little-endian-signed (size-in-bits vector-position)
  (convert-unsigned-to-signed
   (extract-little-endian-unsigned size-in-bits vector-position)
   size-in-bits))

;;; Functions for extracting big-endian numbers.
(defun extract-big-endian-unsigned (size-in-bits vector-position)
  (if (= size-in-bits 8)
      (extract-byte vector-position)
      (let ((half (ash size-in-bits -1)))
	(+ (ash (extract-big-endian-unsigned half vector-position) half)
	   (extract-big-endian-unsigned half vector-position)))))

(defun extract-big-endian-signed (size-in-bits vector-position)
  (convert-unsigned-to-signed
   (extract-big-endian-unsigned size-in-bits vector-position)
   size-in-bits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions for storing signed and unsigned numbers of 1 to 8 bytes

(defun convert-signed-to-unsigned (value size-in-bits)
  (if (minusp value)
      (+ value (ash 1 size-in-bits))
      value))

;;; Store a single byte.
(defun store-byte (byte vector-position)
  (setf (aref (bytes vector-position) (index vector-position))
        byte)
  (incf (index vector-position)))

;;; Functions for storing little-endian numbers.
(defun store-little-endian-unsigned (value size-in-bits vector-position)
  (if (= size-in-bits 8)
      (store-byte value vector-position)
      (let ((half (ash size-in-bits -1)))
	(store-little-endian-unsigned
	 (logand value (1- (ash 1 half))) half vector-position)
	(store-little-endian-unsigned
	 (ash value (- half)) half vector-position))))
	      
(defun store-little-endian-signed (value size-in-bits vector-position)
  (store-little-endian-unsigned
   (convert-signed-to-unsigned value size-in-bits) size-in-bits vector-position))
  

;;; Functions for storing big-endian numbers.
(defun store-big-endian-unsigned (value size-in-bits vector-position)
  (if (= size-in-bits 8)
      (store-byte value vector-position)
      (let ((half (ash size-in-bits -1)))
	(store-big-endian-unsigned
	 (ash value half) half vector-position)
	(store-big-endian-unsigned
	 (logand value (1- (ash 1 half))) half vector-position))))
	      
(defun store-big-endian-signed (value size-in-bits vector-position)
  (store-big-endian-unsigned
   (convert-signed-to-unsigned value size-in-bits) size-in-bits vector-position))
