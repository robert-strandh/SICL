(cl:in-package #:sicl-register-allocation)

(defclass attribution ()
  ((%lexical-location
    :initarg :lexical-location
    :reader lexical-location)
   (%register-number
    :initform nil
    :initarg :register-number
    :accessor register-number)
   (%stack-slot
    :initform nil
    :initarg stack-slot
    :accessor stack-slot)))

(defclass arrangement ()
  ((%stack-map :initarg :stack-map :reader stack-map)
   (%register-map :initarg :register-map :reader register-map)
   (%attributions
    :initform '()
    :initarg :attributions
    :accessor attributions)))

(defmacro with-arrangement-parts
    ((stack-map-var register-map-var attributions-var arrangement-form) &body body)
  (let ((arrangement-var (gensym)))
    `(let* ((,arrangement-var ,arrangement-form)
            (,stack-map-var (stack-map ,arrangement-var))
            (,register-map-var (register-map ,arrangement-var))
            (,attributions-var (attributions ,arrangement-var)))
       ,@body)))

(defvar *input-arrangements*)

(defun input-arrangement (instruction)
  (gethash instruction *input-arrangements*))

(defun (setf input-arrangement) (arrangement instruction)
  (setf (gethash instruction *input-arrangements*) arrangement))

(defvar *output-arrangements*)

(defun output-arrangement (instruction)
  (gethash instruction *output-arrangements*))

(defun (setf output-arrangement) (arrangement instruction)
  (setf (gethash instruction *output-arrangements*) arrangement))
