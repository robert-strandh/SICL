(cl:in-package #:sicl-boot)

;;;; The main purpose of phase 2 is to create a mirror of the MOP
;;;; class hierarchy in run-time environment R2.  We call the classes
;;;; in this hierarchy BRIDGE CLASSES.  A bridge class is neither a
;;;; host class nor a target class.  Instead, a bridge class is an
;;;; instance of one of the host classes we created in phase 1.
;;;; However, a bridge class behaves pretty much like a target class
;;;; in that we have accessor functions that let us query all aspects
;;;; of it as if it were a target class.
;;;;
;;;; The main difficulty with phase 2 is that we need to implement the
;;;; class initialization protocol.  This protocol mainly works on the
;;;; classes metaobjects themselves.  So for instance, it is the
;;;; responsibility of the class initialization protocol to convert
;;;; canonicalized slot descriptors to direct-slot-definition
;;;; metaobjects and associate them with the class metaobjects.
;;;;
;;;; However, the class initialization protocol is also in charge of
;;;; adding accessor methods to generic functions that are mentioned
;;;; in the slot descriptors of the DEFCLASS forms.  These generic
;;;; functions and methods do not take bridge classes as arguments,
;;;; but INSTANCES of bridge classes that will be created in a later
;;;; phase.  We are thus dealing with two different families of
;;;; accessor generic functions with the same names during phase 2.
;;;; The first family consists of HOST GENERIC FUNCTIONS that were
;;;; created before bootstrapping commenced, and that had methods
;;;; added to them during phase 1.  Arguments to these generic
;;;; functions are bridge classes.  The second family is called BRIDGE
;;;; GENERIC FUNCTIONS and they are created as a result of bridge
;;;; classes being created through the instantiation of host classes
;;;; created in phase 1.  To avoid clashes between these two families
;;;; of generic functions, we make the names map to host generic
;;;; functions in run-time-environment R2, and we make the names map
;;;; to bridge generic functions in the run-time environment R3.

(defun phase2 (boot)
  (declare (ignore boot))
  (declare (ignore boot))
  nil)

;;  LocalWords:  accessor metaobject metaobjects canonicalized
;;  LocalWords:  accessors instantiation
