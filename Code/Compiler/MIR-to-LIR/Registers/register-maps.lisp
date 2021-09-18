(cl:in-package #:sicl-x86-64-registers)

(deftype register-map ()
  `(array bit (,+register-count+)))

(defun register-number-in-map-p (register-number register-map)
  (not (zerop (bit register-map register-number))))

(defun make-empty-register-map ()
  (make-array +register-count+
              :element-type 'bit
              :initial-element 0))

(defun mark-register (register-map register-number)
  (setf (bit register-map register-number) 1))

(defun unmark-register (register-map register-number)
  (setf (bit register-map register-number) 0))

(defun copy-register-map (register-map)
  (let ((result (make-array +register-count+ :element-type 'bit)))
    (replace result register-map)
    result))

(defun reserve-register (register-map register-number)
  (let ((result (copy-register-map register-map)))
    (assert (zerop (bit result register-number)))
    (setf (bit result register-number) 1)
    result))

(defun free-register (register-map register-number)
  (let ((result (copy-register-map register-map)))
    (assert (= (bit result register-number) 1))
    (setf (bit result register-number) 0)
    result))

(defun register-map-difference (register-map-1 register-map-2)
  (bit-andc2 register-map-1 register-map-2))

(defun register-map-intersection (register-map-1 register-map-2)
  (bit-and register-map-1 register-map-2))

(defun register-map-union (register-map-1 register-map-2)
  (bit-ior register-map-1 register-map-2))

(defun find-any-register-in-map (register-map &key (start 0))
  (position 1 register-map :start start))

(defun register-map-empty-p (register-map)
  (null (find-any-register-in-map register-map)))

(defun register-number (register)
  (let ((position (position register *registers*)))
    (assert (not (null position)))
    position))

(defun make-register-map (&rest registers)
  (let ((map (make-empty-register-map)))
    (dolist (register registers)
      (mark-register map (register-number register)))
    map))
