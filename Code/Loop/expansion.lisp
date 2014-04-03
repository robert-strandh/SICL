;;;; Copyright (c) 2014
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

(in-package #:sicl-loop)

(defgeneric bindings (clause))

(defmethod bindings (clause)
  (declare (ignore clause))
  '())

(defgeneric prologue (clause)

(defmethod prologue (clause)
  (declare (ignore clause))
  '())

(defgeneric prologue (clause)

(defmethod prologue (clause)
  (declare (ignore clause))
  '())

(defgeneric termination (clause))

(defmethod termination (clause)
  (declare (ignore clause))
  '())

(defgeneric body (clause))

(defmethod body (clause)
  (declare (ignore clause))
  '())

(defgeneric step (clause))

(defmethod step (clause)
  (declare (ignore clause))
  '())

(defgeneric epilogue (clause))

(defmethod epilogue (clause)
  (declare (ignore clause))
  '())
