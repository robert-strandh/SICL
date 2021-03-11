(cl:in-package #:sicl-register-allocation)

(defclass location-assignment ()
  ((%lexical-location
    :initarg :lexical-location
    :reader lexical-location)
   (%register
    :initform nil
    :initarg :register
    :accessor register)
   (%stack-slot
    :initform nil
    :initarg stack-slot
    :accessor stack-slot)))

(defclass instruction-assignment ()
  ((%stack-map :initarg :stack-map :reader stack-map)
   (%location-assignments
    :initform '()
    :initarg :location-assignments
    :accessor location-assignments)))

(defvar *input-assignments*)

(defun input-assignment (instruction)
  (gethash instruction *input-assignments*))

(defun (setf input-assignment) (assignment instruction)
  (setf (gethash instruction *input-assignments*) assignment))

(defvar *output-assignments*)

(defun output-assignment (instruction)
  (gethash instruction *output-assignments*))

(defun (setf output-assignment) (assignment instruction)
  (setf (gethash instruction *output-assignments*) assignment))
