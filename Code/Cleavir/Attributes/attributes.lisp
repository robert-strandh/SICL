(in-package #:cleavir-attributes)

;;;; Attributes are generally organized so that their lack is the
;;;; general case, i.e. if an attribute is "positive" in that it
;;;; enables transformations, it must be explicitly asserted.
;;;; Another way of putting this is that a completely unknown
;;;; function essentially has no attributes.

;;;; Current attributes:

;;; :NO-CALL means that the function does not increase the number of
;;; ways its arguments can be called. That is, it does not call
;;; them itself, and does not enable calls to occur in new ways
;;; (e.g. by storing an argument in a global variable, where anybody
;;;  could call it later). This weird phrasing is because function
;;; arguments could do these things themselves
;;; (e.g. (labels ((foo (x) (push (cons #'foo x) *calls*))) ...))
;;; and this does not implicate the NO-CALL-ness of any function
;;; that is passed them as an argument.
;;; Implies DYN-CALL.

;;; :DYN-CALL means that the function can only increase the number
;;; of ways its arguments can be called with ways that call the
;;; argument in a dynamic environment substantially identical to
;;; that of the DYN-CALL function.
;;; For example, (lambda (f) (funcall f)) could be DYN-CALL,
;;; but (lambda (f x) (let ((*x* x)) (funcall f))) could not, as
;;; it calls its argument f in a different dynamic environment.
;;; This implies that arguments are dx-safe (TODO: attribute for
;;; that) because if f was e.g. stored in a global it could later
;;; be called in arbitrary dynamic environments.

;;; We represent boolean attributes as an integer bitfield.

(defun make-attributes (&rest attributes)
  (loop with result = 0
        for attr in attributes
        for bits = (ecase attr
                     ((:no-call) #b11)
                     ((:dyn-call) #b10))
        do (setf result (logior result bits))
        finally (return result)))

(defun default-attributes () 0)

(defun has-boolean-attribute-p (attributes attribute-name)
  (logbitp
   (ecase attribute-name
     ((:no-call) 0)
     ((:dyn-call) 1))
   attributes))
