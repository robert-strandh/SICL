(cl:in-package #:sicl-boot-phase-6)

(defun define-posix-read (e5)
  (let* ((client (env:client e5))
         (setf-aref-function (env:fdefinition client e5 '(setf aref)))
         (posix-package (find-package '#:sicl-posix-high))
         (symbol-name (symbol-name '#:read))
         (read-symbol (find-symbol symbol-name posix-package)))
    (setf (env:fdefinition (env:client e5) e5 read-symbol)
          (lambda (file-descriptor vector &key (start 0) (end nil))
            (declare (ignore end))
            (let ((stream (ecase file-descriptor
                            (0 *standard-input*))))
              (funcall setf-aref-function
                       (char-code (read-char stream)) vector start)
              (values vector 1))))))
