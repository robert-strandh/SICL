(cl:in-package #:cleavir-io)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Reader programming.

(defparameter *io-readtable* (copy-readtable))

(defun read-model-object (stream char)
  (declare (ignore char))
  (apply #'make-instance (read-delimited-list #\] stream t)))

(set-macro-character #\[ #'read-model-object nil *io-readtable*)
(set-syntax-from-char #\] #\) *io-readtable*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Printer programming.

(defgeneric save-info (object)
  (:method-combination append :most-specific-last))

(defun print-model-object (obj stream)
  (let ((*package* (find-package '#:keyword)))
    (pprint-logical-block (stream nil :prefix "[" :suffix "]")
      (format stream "~s ~2i" (class-name (class-of obj)))
      (loop for info in (reverse (save-info obj))
            do (format stream
                       "~_~s ~_~W "
                       (car info)
                       (funcall (cadr info) obj))))))

(defmacro define-save-info (type &body save-info)
  `(progn

     (defmethod print-object ((obj ,type) stream)
       (if *print-readably*
           (print-model-object obj stream)
           (call-next-method)))

     (defmethod save-info append ((obj ,type))
       ',save-info)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; I/O.

(define-condition model-condition (error) ())

(define-condition file-does-not-exist (model-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "File does not exist"))))

(define-condition unknown-file-version (model-condition) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Unknown file version"))))

(defun read-model (filename allowed-version-names)
  (assert (probe-file filename) () 'file-does-not-exist)
  (with-open-file (stream filename :direction :input)
    (let* ((version (read-line stream)))
      (assert (member version allowed-version-names :test #'string=)
              () 'unknown-file-version)
      (let ((*read-eval* nil)
            (*readtable* *io-readtable*))
        (read stream)))))

(defun write-model (filename version-name object)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*print-circle* t)
          (*print-readably* t)
          (*package* (find-package :keyword)))
      (format stream "~a~%" version-name)
      (pprint object stream)
      (terpri stream)
      (finish-output stream))))
