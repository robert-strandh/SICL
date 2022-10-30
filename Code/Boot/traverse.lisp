(cl:in-package #:sicl-boot)

;;;; The code in this file is meant to be loaded into environment E5.
;;;; The generic function TRAVERSE should return a list of work-list
;;;; items, each item having the form (ADDRESS . OBJECT) meaning that
;;;; the pointer to OBJECT should be stored in ADDRESS.

;;; OBJECT is an ersatz object that the methods will use for dispatch.
;;; RACK is the rack of the object, represented as a host vector.
;;; RACK-ADDRESS is the address in memory of the first word of the
;;; rack.
(defgeneric traverse (object rack rack-address))
