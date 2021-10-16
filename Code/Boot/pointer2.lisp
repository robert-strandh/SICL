(cl:in-package #:sicl-boot)

;;; Return the number words in the prefix of a rack that contains
;;; words that with Lisp objects in them, as opposed to raw data.
(defgeneric trace-prefix (object))

;;; This method is invoked when an object other than a standard object
;;; is given.  Such objects do not have racks, so we signal an error.
(defmethod trace-prefix (object)
  (error "Not rack in object ~s" object))

;;; This method is invoked for most standard objects.  In such an
;;; object every word in the rack contains a Lisp object, so we return
;;; the instance size of the class, plus 2 for the stamp and the list
;;; of effective slots.
(defmethod trace-prefix ((object standard-object))
  (+ (sicl-clos:instance-size (class-of object)) 2))
