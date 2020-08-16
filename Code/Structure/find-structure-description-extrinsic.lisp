(cl:in-package #:sicl-structure)

(defvar *structure-descriptions* (make-hash-table :test #'eq))

(defun find-structure-description (name &optional (errorp t) environment)
  (declare (ignore environment))
  (let ((description (gethash name *structure-descriptions*)))
    (if (and (null description) errorp)
	(error 'undefined-structure-description :name name)
	description)))

(defun slot-description-compatible-p (new old)
  (and (eql (slot-name new) (slot-name old))
       ;; ### should this be a proper type-equality check?
       (equal (slot-type new) (slot-type old))
       (eql (slot-read-only new) (slot-read-only old))))

(defun typed-structure-definitions-compatible-p (new old)
  ;; New and old must have the same type and layout.
  ;; Adding/removing constructors/predicates/copiers doesn't change the
  ;; layout, so isn't a compatibility constraint.
  (and
   ;; ### should this be a proper type-equality check?
   (equal (defstruct-type new) (defstruct-type old))
   (eql (defstruct-named new) (defstruct-named old))
   (eql (defstruct-initial-offset new) (defstruct-initial-offset old))
   (eql (defstruct-included-structure-name new)
        (defstruct-included-structure-name old))
   (eql (length (defstruct-included-slots new))
        (length (defstruct-included-slots old)))
   (every #'slot-description-compatible-p
          (defstruct-included-slots new)
          (defstruct-included-slots old))
   (eql (length (defstruct-direct-slots new))
        (length (defstruct-direct-slots old)))
   (every #'slot-description-compatible-p
          (defstruct-direct-slots new)
          (defstruct-direct-slots old))))

(defun (setf find-structure-description) (new-value name &optional (errorp t) environment)
  (declare (ignore errorp environment))
  (cond ((null new-value)
         (remhash name *structure-descriptions*))
        (t
         (let ((existing (gethash name *structure-descriptions*)))
           (when existing
             (unless (typed-structure-definitions-compatible-p new-value existing)
               (error 'incompatible-redefiniiton-of-typed-structure
                      :name name))))
         (setf (gethash name *structure-descriptions*) new-value))))
