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
    :initarg :stack-slot
    :accessor stack-slot)))

(defclass arrangement ()
  ((%stack-map :initarg :stack-map :reader stack-map)
   (%register-map :initarg :register-map :reader register-map)
   (%attributions
    :initform '()
    :initarg :attributions
    :accessor attributions)))

;;; FIXME: remove this method once everything is working.
(defmethod initialize-instance :after ((object arrangement) &key)
  (let ((register-count 0)
        (register-map (register-map object)))
    ;; Make sure every register in the attributions is accounted for in
    ;; the register map.  Also count the number of registers.
    (loop for attribution in (attributions object)
          for register-number = (register-number attribution)
          do (unless (null register-number)
               (incf register-count)
               (assert (= (bit register-map register-number) 1))))
    ;; Make sure there are as many 1s in the register map as there are
    ;; non-NIL registers in the attributions.
    (assert (= register-count (count 1 register-map)))))

(defmacro with-arrangement-parts
    ((stack-map-var register-map-var attributions-var arrangement-form) &body body)
  (let ((arrangement-var (gensym)))
    `(let* ((,arrangement-var ,arrangement-form)
            (,stack-map-var (stack-map ,arrangement-var))
            (,register-map-var (register-map ,arrangement-var))
            (,attributions-var (attributions ,arrangement-var)))
       ,@body)))

(defvar *input-arrangements*)

(defun input-arrangement-p (instruction)
  (nth-value 1 (gethash instruction *input-arrangements*)))

(defun input-arrangement (instruction)
  (multiple-value-bind (value value-p)
      (gethash instruction *input-arrangements*)
    (assert value-p)
    value))

(defun (setf input-arrangement) (arrangement instruction)
  (setf (gethash instruction *input-arrangements*) arrangement))

(defvar *output-arrangements*)

(defun output-arrangement (instruction)
  (multiple-value-bind (value value-p)
      (gethash instruction *output-arrangements*)
    (assert value-p)
    value))

(defun (setf output-arrangement) (arrangement instruction)
  (setf (gethash instruction *output-arrangements*) arrangement))

(defun update-arrangement-for-new-definition
    (arrangement lexical-location register-number)
  (with-arrangement-parts (stack-map register-map attributions arrangement)
    (let ((attribution (find lexical-location attributions
                             :test #'eq :key #'lexical-location)))
      (cond ((null attribution)
             (let ((new-register-map
                     (reserve-register register-map register-number))
                   (new-attribution (make-instance 'attribution
                                      :lexical-location lexical-location
                                      :stack-slot nil
                                      :register-number register-number)))
               (assert (null (find register-number attributions
                                   :test #'eql :key #'register-number)))
               (make-instance 'arrangement
                 :stack-map stack-map
                 :register-map new-register-map
                 :attributions (cons new-attribution attributions))))
            ((null (stack-slot attribution))
             (assert (eql register-number (register-number attribution)))
             ;; The existing arrangement is perfect.  Just return it.
             arrangement)
            (t
             (assert (eql register-number (register-number attribution)))
             ;; We must create a new arrangement that is similar to
             ;; the one we got, except that the attribution for
             ;; LEXICAL-LOCATION must have a stack slot that is NIL.
             (let ((residual-attributions
                     (remove attribution attributions :test #'eq))
                   (new-stack-map (copy-stack-map stack-map))
                   (new-attribution (make-instance 'attribution
                                      :lexical-location lexical-location
                                      :stack-slot nil
                                      :register-number register-number)))
               (free-stack-slot new-stack-map (stack-slot attribution))
               (make-instance 'arrangement
                 :stack-map new-stack-map
                 :register-map register-map
                 :attributions (cons new-attribution residual-attributions))))))))
