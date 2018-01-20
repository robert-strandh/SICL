(cl:in-package #:sicl-reader)

(defparameter *backquote-allowed-p* t)

(defparameter *backquote-in-subforms-allowed-p* t)

(defparameter *backquote-depth* 0)

(defvar *consing-dot* '#:|.|)

(defparameter *consing-dot-allowed-p* nil)

(define-condition end-of-list () ())

(defvar *end-of-list* (make-condition 'end-of-list))

(defparameter *preserve-whitespace* nil)

(defparameter *readtable* nil)

(defparameter *client* nil)
