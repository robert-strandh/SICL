(cl:in-package #:sicl-register-allocation)

;;; Locations which merely represent argument registers that have to
;;; be preserved during argument parsing.

;;; We assume that an argument is only used once, which is true of the
;;; current argument parsing code.

(defclass constant-argument-instruction (cleavir-ir:instruction)
  ((register :initarg :register :reader register)))

(defvar *bogus-argument-locations*)

(defun make-bogus-argument-locations ()
  (loop for register in x86-64:*argument-registers*
        for n from 1
        collect (cleavir-ir:make-lexical-location
                 (format nil "~:r argument" n))))

