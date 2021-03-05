(cl:in-package #:sicl-register-allocation)

(defclass work-list ()
  ((%to-process :initform (make-hash-table :test #'eq) :reader to-process)
   (%stack :initform '() :accessor stack)))

;;; Normalizing a work list means popping element off the stack until
;;; either the stack is empty, or the top element is in the table.
(defun normalize (work-list)
  (loop until (or (null (stack work-list))
                  (gethash (first (stack work-list))
                           (to-process work-list)))
        do (pop (stack work-list))))

(defun emptyp (work-list)
  (null (stack work-list)))

(defun pop-item (work-list)
  (assert (not (emptyp work-list)))
  (let ((result (pop (stack work-list))))
    (setf (gethash result (to-process work-list)) nil)
    (normalize work-list)
    result))

(defun push-item (work-list item)
  (push item (stack work-list))
  (setf (gethash item (to-process work-list)) t))
