(cl:in-package #:sicl-new-boot)

(defclass header (closer-mop:funcallable-standard-object)
  ((%class :initarg :class :accessor class)
   (%rack :initarg :rack :accessor rack)))

(defun allocate-general-instance (class size)
  (make-instance 'header
    :class class
    :rack (make-array size :initial-element 99999)))

(defgeneric standard-instance-access (object location))

(defgeneric (setf standard-instance-access) (value object location))

(defmethod standard-instance-access ((object header) location)
  (aref (rack object) (+ location 2)))

(defmethod (setf standard-instance-access) (value (object header) location)
  (setf (aref (rack object) (+ location 2)) value))

(defvar *fixnum-class*)
(defvar *character-class*)
(defvar *cons-class*)
(defvar *symbol-class*)

(defmethod class ((object integer))
  *fixnum-class*)

(defmethod class ((object character))
  *character-class*)

(defmethod class ((object cons))
  *cons-class*)

(defmethod class ((object symbol))
  *symbol-class*)

(defgeneric stamp (object))

(defmethod stamp ((object header))
  (standard-instance-access object -2))

(defvar *fixnum-stamp*)
(defvar *character-stamp*)
(defvar *cons-stamp*)
(defvar *symbol-stamp*)
  
(defmethod stamp ((object integer))
  *fixnum-stamp*)

(defmethod stamp ((object character))
  *character-stamp*)

(defmethod stamp ((object cons))
  *cons-stamp*)

(defmethod stamp ((object symbol))
  *symbol-stamp*)
