(defpackage #:sicl-documentation
    (:use #:cl)
  (:shadow #:documentation))

(in-package #:sicl-documentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Classes for structured documentation

;;; The lowest level is a word.  One step above that is a paragraph.
;;; A paragraph is the base class for objects that can contain words
;;; and other paragraphs.  So for instance reference is a subclass of
;;; paragraph, because it potentially contains several words. 

(defclass word ()
  (;; a string of the characters in the word
   (%characters :initarg :characters :accessor characters)))

(defclass paragraph ()
  (;; a list of words or other structured object
   ;; (such as cross references) of the paragraph
   (%contents :initarg :contents :accessor contents)))

;;; Make it convenient to manually create paragraphs
;;; by allowing a string in the contents.  This string
;;; will be broken up in to words according to whitespace
;;; in the string.

(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun split-string (string)
  (loop with start = 0
	with end = (length string)
	;; Using collect would make the loop too obscure
	with words = '()
	until (= start end)
	do (let ((pos (position-if #'whitespacep string :start start)))
	     (cond ((null pos)
		    (push (make-instance 'word
			    :characters (subseq string start end))
			  words)
		    (setf start end))
		   ((= pos start)
		    (incf start))
		   (t
		    (push (make-instance 'word
			    :characters (subseq string start pos))
			  words)
		    (setf start (1+ pos)))))
	finally (return (nreverse words))))

(defmethod initialize-instance :after ((paragraph paragraph)
				       &rest args
				       &key contents
				       &allow-other-keys)
  (declare (ignore args))
  (setf (contents paragraph)
	(mapcan (lambda (item)
		  (if (stringp item)
		      (split-string item)
		      (list item)))
		contents)))

(defclass reference (paragraph) ())

(defclass documentation (reference)
  (;; arguments to the documentation function
   (%documentation-arguments
    :initarg :documentation-arguments
    :accessor documentation-arguments)))

(defclass glossary-entry (reference)
  ((%entry-key :initarg entry-key :accessor entry-key)))

;;; A section is an entity that has a title and that contains several
;;; paragraphs.  Rather than having separate classes for chapters,
;;; sections, subsection, etc, we just let sections nest arbitrarily,
;;; and let the renderer decide whether the top one is a chapter or a
;;; section.

(defclass section ()
  (;; a distinguished paragraph
   (%title :initarg :title :accessor title)
   ;; a list of paragraphs and other structured
   ;; objects (enumerations, itemized lists, etc) that
   ;; can occur in a section
   (%contents :initarg :contents :accessor contents)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style objects and renderers

(defclass style () ())

(defclass text-style (style) ())

(defclass enriched-stream ()
  ((%underlying-stream :initarg :underlying-stream :reader underlying-stream)
   (%column :initform 0 :accessor column)))

(defun output-string (string enriched-stream)
  (when (and (plusp (column enriched-stream))
	     (> (+ (column enriched-stream) (length string) 1) 72))
    (terpri (underlying-stream enriched-stream))
    (setf (column enriched-stream) 0))
  (format (underlying-stream enriched-stream) "~a " string)
  (incf (column enriched-stream) (1+ (length string))))

(defun output-newlines (n enriched-stream)
  (setf (column enriched-stream) 0)
  (format (underlying-stream enriched-stream) "~v%" n))

(defgeneric render (object style enriched-stream))

;;; This renderer is very primitive. 
(defmethod render ((section section) (style text-style) enriched-stream)
  (render (title section) style enriched-stream)
  (output-newlines 2 enriched-stream)
  (loop for object in (contents section)
	do (render object style enriched-stream)
	do (output-newlines 2 enriched-stream)))

(defmethod render ((paragraph paragraph) (style text-style) enriched-stream)
  (loop for object in (contents paragraph)
	do (render object style enriched-stream)))

(defmethod render ((word word) (style text-style) enriched-stream)
  (output-string (characters word) enriched-stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Readtable modification for structured objects

(defparameter *documentation-readtable* (copy-readtable))

(set-syntax-from-char #\] #\) *documentation-readtable*)

(set-macro-character
 #\[
 (lambda (stream char)
   (declare (ignore char))
   (apply #'make-instance (read-delimited-list #\] stream t)))
 nil
 *documentation-readtable*)
			   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documetation tables

(defparameter *structured-variable-documentation-table*
  (make-hash-table :test 'eq))

(defparameter *variable-documentation-table*
  (make-hash-table :test 'eq))

(defparameter *structured-function-documentation-table*
  (make-hash-table :test 'eq))

(defparameter *function-documentation-table*
  (make-hash-table :test 'eq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The documentation functions

(defgeneric structured-documentation (object doc-type))

(defgeneric (setf structured-documentation) (new-documentation object doc-type))

(defgeneric documentation (object doc-type))

(defgeneric (setf documentation) (new-documentation object doc-type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Documentation for variables

(defmethod structured-documentation ((object symbol) (doc-type (eql 'variable)))
  (gethash object *structured-variable-documentation-table*))

(defmethod documentation ((object symbol) (doc-type (eql 'variable)))
  (let ((documentation (gethash object *variable-documentation-table*)))
    (or documentation 
	(let ((structured-documentation (structured-documentation object doc-type)))
	  (if (null structured-documentation)
	      nil
	      (setf (documentation object doc-type)
		    (with-output-to-string (stream)
		      (render structured-documentation
			      (make-instance 'text-style)
			      (make-instance 'enriched-stream
				:underlying-stream stream)))))))))

(defmethod (setf structured-documentation)
    (new-documentation (object symbol) (doc-type (eql 'variable)))
  (setf (gethash object *structured-variable-documentation-table*) new-documentation))

(defmethod (setf documentation)
    (new-documentation (object symbol) (doc-type (eql 'variable)))
  (setf (gethash object *variable-documentation-table*) new-documentation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Load the docstrings file

(defun load-docstrings ()
  (let ((*readtable* *documentation-readtable*))
    (load "docstrings.lisp")))
