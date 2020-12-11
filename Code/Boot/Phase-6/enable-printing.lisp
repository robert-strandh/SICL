(cl:in-package #:sicl-boot-phase-6)

(defun define-posix-write (e5)
  (let* ((client (env:client e5))
         (aref-function (env:fdefinition client e5 'aref))
         (array-total-size-function
           (env:fdefinition client e5 'array-total-size))
         (posix-package (find-package '#:sicl-posix-high))
         (symbol-name (symbol-name '#:write)))
    (when (null posix-package)
      (setf posix-package (make-package '#:sicl-posix-high :use '(#:common-lisp)))
      (export (intern symbol-name posix-package) posix-package))
    (let ((write-symbol (find-symbol symbol-name posix-package)))
      (setf (env:fdefinition (env:client e5) e5 write-symbol)
            (lambda (file-descriptor vector &key (start 0) (end nil))
              (let ((stream (ecase file-descriptor
                              (1 *standard-output*)
                              (2 *error-output*)))
                    (end (if (null end)
                             (funcall array-total-size-function vector)
                             end)))
                (loop for index from start below end
                      for element = (funcall aref-function vector index)
                      do (write-char (code-char element) stream))
                (finish-output stream)))))))

(defun enable-printing (e5)
  (load-source-file "Printer/variables.lisp" e5))
