(cl:in-package #:sicl-readtable)

;;; This function makes a fresh copy of READTABLE and returns the
;;; fresh copy.  READTABLE must be a readtable.
(defgeneric copy-readtable (readtable))

;;; This function copies all the information from the readtable FROM-READTABLE
;;; to the readtable TO-READTABLE.  Finally, it returns TO-READTABLE.
;;; Both FROM-READTABLE and TO-READTABLE must be readtables.
(defgeneric copy-readtable-into (from-readtable to-readtable))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.  Notice that the readtable is the
;;; first parameter to this function. 
(defgeneric make-dispatch-macro-character
    (readtable char &optional non-terminating-p))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric readtable-case (readtable))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric (setf readtable-case) (mode readtable))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric readtablep (object))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric get-macro-character (readtable char))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric set-macro-character
    (function readtable char &optional non-terminating-p))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric get-dispatch-macro-character (readtable disp-char sub-char))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric set-dispatch-macro-character
    (function readtable disp-char sub-char))

;;; This function is the generic version of the standard Common Lisp
;;; function with the same name.
(defgeneric (setf syntax-from-char)
    (from-readtable to-readtable to-char from-char))

;;; This function returns the syntax type of the character CHAR in
;;; READTABLE.  The syntax type is one of :WHITESPACE,
;;; :TERMINATING-MACRO, :NON-TERMINATING-MACRO, :CONSTITUENT,
;;; :SINGLE-ESCAPE, :MULTIPLE-ESCAPE, or :INVALID.
(defgeneric syntax-type (readtable char))

;;; This function sets the syntax type of the character CHAR in
;;; READTABLE to SYNTAX-TYPE.  It return SYNTAX-TYPE.
(defgeneric (setf syntax-type) (syntax-type readtable char))
