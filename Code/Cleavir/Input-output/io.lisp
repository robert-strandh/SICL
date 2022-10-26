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
;;; Class CLONEABLE-MIXIN
;;;
;;; This class can be used as a superclass of classes with instances
;;; that need to be serialized.  We provide a method on PRINT-OBJECT
;;; specialized to this class that prints an instance in a way that it
;;; can be read back in to create a similar object.

(defclass cloneable-mixin () ())

(defmethod print-object ((object cloneable-mixin) stream)
  (if *print-readably*
      (let ((*package* (find-package '#:keyword)))
        (pprint-logical-block (stream nil :prefix "[" :suffix "]")
          (format stream "~s ~2i" (class-name (class-of object)))
          (loop for (initarg reader)
                  in (reverse (clonedijk:clone-information object))
                do (format stream "~_~s ~_~W "
                           initarg (funcall reader object)))))
      (call-next-method)))

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
