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
  ((%stack-map :initarg :stack-map :accessor stack-map)
   (%register-map :initarg :register-map :accessor register-map)
   (%attributions
    :initform '()
    :initarg :attributions
    :accessor attributions)))

(defun check-arrangement-integrity (arrangement)
  (let ((register-count 0)
        (register-map (register-map arrangement)))
    ;; Make sure every register in the attributions is accounted for in
    ;; the register map.  Also count the number of registers.
    (loop for attribution in (attributions arrangement)
          for register-number = (register-number attribution)
          do (unless (null register-number)
               (incf register-count)
               (assert (= (bit register-map register-number) 1))))
    ;; Make sure there are as many 1s in the register map as there are
    ;; non-NIL registers in the attributions.
    (assert (= register-count (count 1 register-map)))))

;;; FIXME: remove this method once everything is working.
(defmethod initialize-instance :after ((object arrangement) &key)
  (check-arrangement-integrity object))

(defmethod (setf attributions) :after
    (new-attributions (arrangement arrangement))
  (declare (ignore new-attributions))
  (check-arrangement-integrity arrangement))

(defmacro with-arrangement-parts
    ((stack-map-var register-map-var attributions-var arrangement-form) &body body)
  (let ((arrangement-var (gensym)))
    `(let* ((,arrangement-var ,arrangement-form)
            (,stack-map-var (stack-map ,arrangement-var))
            (,register-map-var (register-map ,arrangement-var))
            (,attributions-var (attributions ,arrangement-var)))
       ,@body)))

(defmacro with-arrangement (arrangement-form &body body)
  `(with-accessors ((stack-map stack-map)
                    (register-map register-map)
                    (attributions attributions))
       ,arrangement-form
     ,@body))

(defmacro with-attribution (attribution-form &body body)
  `(with-accessors ((lexical-location lexical-location)
                    (stack-slot stack-slot)
                    (register-number register-number))
       ,attribution-form
     ,@body))

(defun copy-bit-vector (bit-vector)
  (let ((result (make-array (length bit-vector) :element-type 'bit)))
    (replace result bit-vector)
    result))

(defun copy-attribution (attribution)
  (make-instance 'attribution
    :lexical-location (lexical-location attribution)
    :register-number (register-number attribution)
    :stack-slot (stack-slot attribution)))

(defun copy-attributions (attributions)
  (mapcar #'copy-attribution attributions))

(defun copy-arrangement (arrangement)
  (with-arrangement-parts (stack-map register-map attributions arrangement)
    (make-instance 'arrangement
      :stack-map (copy-bit-vector stack-map)
      :register-map (copy-bit-vector register-map)
      :attributions (copy-attributions attributions))))

;;; Return the attribution of LEXICAL-LOCATION in ARRANGEMENT as two
;;; values, the STACK-SLOT and the REGISTER-NUMBER.  If
;;; LEXICAL-LOCATION is not attributed in ARRANGEMENT, then return NIL
;;; and NIL.
(defun find-attribution (arrangement lexical-location)
  (let ((attribution (find lexical-location (attributions arrangement)
                           :test #'eq :key #'lexical-location)))
    (if (null attribution)
        (values nil nil)
        (values (stack-slot attribution) (register-number attribution)))))

(defun register-attributed-p (arrangement register-number)
  (not (zerop (bit (register-map arrangement) register-number))))

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

;;; Destructively update ARRANGEMENT to reflect a new definition of
;;; LEXICAL-LOCATION, by adding an attribution of LEXICAL-LOCATION to
;;; REGISTER-NUMBER.  If LEXICAL-LOCATION has an existing attribution
;;; in ARRANGEMENT, then signal an error.  If REGISTER-NUMBER is
;;; already attributed to some lexical location in ARRANGEMENT, then
;;; signal an error.
(defun update-arrangement-for-new-definition
    (arrangement lexical-location register-number)
  (assert (null (find-attribution arrangement lexical-location)))
  (assert (not (register-attributed-p arrangement register-number)))
  (push (make-instance 'attribution
          :lexical-location lexical-location
          :stack-slot nil
          :register-number register-number)
        (attributions arrangement)))

;;; Destructively update ARRANGEMENT so that LEXICAL-LOCATION is no
;;; longer attributed.  If LEXICAL-LOCATION is not attributed in
;;; ARRANGEMENT, then signal an error.
(defun delete-attribution (arrangement lexical-location)
  (with-arrangement arrangement
    (let ((attribution (find lexical-location attributions
                             :test #'eq :key #'lexical-location)))
      (assert (not (null attribution)))
      (with-attribution attribution
        (setf (bit stack-map stack-slot) 0)
        (unless (null register-number)
          (setf (bit register-map register-number) 0))
        (setf attributions
              (delete attribution attributions :test #'eq))))))

;;; Destructively update ARRANGEMENT so that the ATTRIBUTION
;;; containing REGISTER-NUMBER also has a stack slot.  If there is no
;;; attribution that contains REGISTER-NUMBER, then signal an error.
;;; If the attribution containing REGISTER-NUMBER already has a stack
;;; slot, then signal an error.
(defun ensure-stack-slot (arrangement register-number)
  (with-arrangement arrangement
    (let ((attribution (find register-number attributions
                             :test #'eql :key #'register-number)))
      (assert (not (null attribution)))
      (with-attribution attribution
        (assert (null stack-slot))
        (let ((free-stack-slot (find 0 stack-map :test #'eql)))
          (when (null free-stack-slot)
            (setf free-stack-slot (length stack-map))
            (let* ((length (1+ free-stack-slot))
                   (new-stack-map (make-array length :element-type 'bit)))
              (replace new-stack-map stack-map)
              (setf (bit new-stack-map free-stack-slot) 1)
              (setf stack-map new-stack-map)))
          (setf stack-slot free-stack-slot))))))
