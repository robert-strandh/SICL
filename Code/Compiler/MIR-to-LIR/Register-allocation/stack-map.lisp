(cl:in-package #:sicl-register-allocation)

;;;; A STACK SLOT is a non-negative integer that indicates an offset
;;;; relative to the frame pointer of a stack frame.  A STACK MAP is a
;;;; bit vector that mirrors the stack frame in that it has a bit for
;;;; each slot in the stack frame.  During register allocation, we do
;;;; not know how big the stack frame needs to be, so the stack map
;;;; grows as needed.  A 0 in the bit vector represents an available
;;;; stack slot, and a 1 represents a stack slot that holds some
;;;; lexical location at some program point.

;;; Finding a stack slot in a stack map is an operation that always
;;; succeeds.  If there is an available stack slot, then it is
;;; returned.  Otherwise, the length of the stack map is returned,
;;; indicating the first element beyond the current stack map.
(defun find-stack-slot (stack-map)
  (let ((position (position 0 stack-map)))
    (if (null position)
        (length stack-map)
        position)))

;;; Reserving a stack slot in a stack map means returning a new stack
;;; map where the bit corresponding to the stack slot has been set to
;;; 1.  STACK-SLOT can be either a valid array index into the stack
;;; map, or it can be equal to the length of the stack map.  In the
;;; latter case, we allocate a stack map with a length that is the
;;; length of STACK-MAP plus 1.
(defun reserve-stack-slot (stack-map stack-slot)
  (let* ((length (length stack-map))
         (new-length (max length (1+ stack-slot)))
         (result (make-array new-length :element-type 'bit)))
    (assert (<= new-length (1+ length)))
    (replace result stack-map)
    (assert (zerop (bit result stack-slot)))
    (setf (bit result stack-slot) 1)
    result))

(defun find-and-reserve-stack-slot (stack-map)
  (let ((stack-slot (find-stack-slot stack-map)))
    (values stack-slot
            (reserve-stack-slot stack-map stack-slot))))
