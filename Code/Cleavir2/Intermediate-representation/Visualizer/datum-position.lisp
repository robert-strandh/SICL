(cl:in-package #:cleavir-ir-visualizer)

;;; Given a datum as the key, this table contains a CONS of the
;;; horizontal position and the vertical position for that datum.
;;; This variable must be bound at the beginning of the visualization.
(defvar *data-position-table*)

;;; Return the vertical position and the horizontal position of
;;; DATUM as two values.  If either of them has not been set,
;;; then signal an error.
(defun datum-position (datum)
  (let ((entry (gethash datum *data-position-table*)))
    (cond ((null entry)
           (error "No position has been assigned to ~s" datum))
          ((null (car entry))
           (error "No horizontal position has been assigned to ~s" datum))
          ((null (cdr entry))
           (error "No vertical position has been assigned to ~s" datum))
          (t
           (values (car entry) (cdr entry))))))

;;; Return the horizontal position of DATUM.  If the horizontal
;;; position has not been set, then signal an error.
(defun datum-horizontal-position (datum)
  (let ((entry (gethash datum *data-position-table*)))
    (cond ((or (null entry) (null (car entry)))
           (error "No horizontal position has been assigned to ~s" datum))
          (t
           (car entry)))))

;;; Return the vertical position of DATUM.  If the vertical
;;; position has not been set, then signal an error.
(defun datum-vertical-position (datum)
  (let ((entry (gethash datum *data-position-table*)))
    (cond ((or (null entry) (null (cdr entry)))
           (error "No vertical position has been assigned to ~s" datum))
          (t
           (cdr entry)))))

;;; Set the horizontal position and the vertical position of DATUM.
(defun (setf datum-position) (position datum)
  (setf (gethash datum *data-position-table*) position))

;;; Set the horizontal position of DATUM.
(defun (setf datum-horizontal-position) (hpos datum)
  (let ((entry (gethash datum *data-position-table*)))
    (if (null entry)
        (setf (gethash datum *data-position-table*)
              (cons hpos nil))
        (setf (car entry) hpos))
    hpos))

;;; Set the vertical position of DATUM.
(defun (setf datum-vertical-position) (vpos datum)
  (let ((entry (gethash datum *data-position-table*)))
    (if (null entry)
        (setf (gethash datum *data-position-table*)
              (cons nil vpos))
        (setf (cdr entry) vpos))
    vpos))
