(cl:in-package #:sicl-reader)

(defvar *labels*)

(defun read (&optional
	       (input-stream *standard-input*)
	       (eof-error-p t)
	       (eof-value nil)
	       (recursive-p nil))
  (if recursive-p
      (let ((*preserve-whitespace* recursive-p))
        (read-common input-stream eof-error-p eof-value))
      (let* ((*preserve-whitespace* recursive-p)
             (*labels* (make-hash-table))
             (result (read-common input-stream eof-error-p eof-value)))
        (unless (zerop (hash-table-count *labels*))
          (let ((mapping (make-hash-table :test #'equal)))
            (maphash (lambda (key value)
                       (declare (ignore key))
                       (setf (gethash (car value) mapping) (cdr value)))
                     *labels*)
            (fixup result (make-hash-table :test #'eq) mapping)))
        result)))

(defun read-preserving-whitespace (&optional
                                     (input-stream *standard-input*)
                                     (eof-error-p t)
                                     (eof-value nil)
                                     (recursive-p nil))
  (if recursive-p
      (let ((*preserve-whitespace* t))
        (read-common input-stream eof-error-p eof-value))
      (let ((*preserve-whitespace* t)
            (*labels* (make-hash-table)))
        (read-common input-stream eof-error-p eof-value))))
