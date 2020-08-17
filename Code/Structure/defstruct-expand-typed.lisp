;;;; Expand typed defstructs.

(cl:in-package #:sicl-structure)

(defun check-valid-defstruct-type (description environment)
  ;; The type must name a symbol naming a sequence type,
  ;; or a list that lookes like (VECTOR element-type).
  (let ((type (defstruct-type description)))
    (unless (or (and (symbolp type)
                     (subtypep type 'sequence environment))
                (and (consp type)
                     (eql (first type) 'vector)
                     (consp (rest type))
                     (endp (rest (rest type)))))
      (error 'invalid-defstruct-type :type type))))

(defun canonicalize-struct-type (type)
  (if (eql type 'vector)
      '(vector t)
      type))

(defun vector-defstruct-p (description)
  (let ((type (defstruct-type description)))
    (or (eql type 'vector)
        (and (consp type)
             (eql (first type) 'vector)))))

(defmethod compute-slot-layout ((description defstruct-typed-description) environment)
  (let* ((initial-offset (or (defstruct-initial-offset description) 0))
         (initial-offset-padding (make-list initial-offset :initial-element nil)))
    (cond ((defstruct-included-structure-name description)
           (let* ((parent-name (defstruct-included-structure-name description))
                  (parent (find-structure-description parent-name nil environment)))
             (unless parent
               (if (find-class parent-name nil environment)
                   (error 'included-structure-must-be-typed :name parent-name)
                   (error 'included-structure-does-not-exist :name parent-name)))
             ;; TODO: This should do a subtypep test to make sure the types are the same/compatible...
             (unless (equal (canonicalize-struct-type (defstruct-type description))
                            (canonicalize-struct-type (defstruct-type parent)))
               (error 'included-structure-type-is-incompatible
                      :type (defstruct-type description)
                      :included-type (defstruct-type parent)))
             (destructuring-bind (parent-slot-layout parent-name-layout)
                 (compute-slot-layout parent environment)
               (let ((parent-slots (remove nil parent-slot-layout)))
                 ;; Make sure there are no conflicting slots and that all the included
                 ;; slots exist.
                 (dolist (slot (defstruct-direct-slots description))
                   (let ((existing (find (slot-name slot) parent-slots :key #'slot-name :test #'string=)))
                     (when existing
                       (error 'direct-slot-conflicts-with-parent-slot
                              :slot-name (slot-name slot)
                              :parent-slot-name (slot-name existing)))))
                 (dolist (slot (defstruct-included-slots description))
                   (let ((parent-slot (find (slot-name slot) parent-slots :key #'slot-name)))
                     (unless parent-slot
                       (error 'included-slot-missing-from-parent
                              :slot-name (slot-name slot)))
                     (unless (subtypep (slot-type slot) (slot-type parent-slot) environment)
                       (error 'included-slot-type-must-be-subtype
                              :slot-name (slot-name slot)
                              :type (slot-type slot)
                              :included-type (slot-type parent-slot)))
                     (when (and (slot-read-only parent-slot)
                                (not (slot-read-only slot)))
                       (error 'included-slot-must-be-read-only
                              :slot-name (slot-name slot))))))
               ;; Turn the parent-slot-layout into a list of effective-ish slots
               (list (append (loop for parent-slot in parent-slot-layout
                                   collect (and parent-slot
                                                (or (find (slot-name parent-slot)
                                                          (defstruct-included-slots description)
                                                          :key #'slot-name)
                                                    (make-instance 'slot-description
                                                                   :name (slot-name parent-slot)
                                                                   :accessor-name (compute-accessor-name description (slot-name parent-slot))
                                                                   :initform (slot-initform parent-slot)
                                                                   :initform-p (slot-initform-p parent-slot)
                                                                   :type (slot-type parent-slot)
                                                                   :read-only (slot-read-only parent-slot)))))
                             initial-offset-padding
                             (if (defstruct-named description)
                                 (list nil)
                                 '())
                             (defstruct-direct-slots description))
                     (append (if (defstruct-named description)
                                 (list (cons (defstruct-name description) (+ (length parent-slot-layout)
                                                                             initial-offset)))
                                 '())
                             parent-name-layout)))))
          (t
           (if (defstruct-named description)
               (list (append initial-offset-padding
                             (list* nil
                                    (defstruct-direct-slots description)))
                     (list (cons (defstruct-name description) initial-offset)))
               (list (append initial-offset-padding
                             (defstruct-direct-slots description))
                     '()))))))

(defmethod layout-slots ((description defstruct-typed-description) layout)
  (remove nil (first layout)))

(defmethod generate-allocation-form ((description defstruct-typed-description) layout)
  (destructuring-bind (slot-layout name-layout) layout
    (let ((form (if (vector-defstruct-p description)
                    `(make-array ,(length slot-layout) :element-type ',(second (canonicalize-struct-type (defstruct-type description))))
                    `(make-sequence ',(defstruct-type description) ,(length slot-layout)))))
      (cond (name-layout
             (let ((temp (gensym)))
               `(let ((,temp ,form))
                  ,@(loop for (name . name-index) in name-layout
                          collect `(setf (elt ,temp ,name-index) ',name))
                  ,temp)))
            (t form)))))

(defmethod generate-slot-initialization-form ((description defstruct-typed-description) layout object slot value)
  (destructuring-bind (slot-layout name-layout) layout
    (declare (ignore name-layout))
    `(setf (elt ,object ,(position slot slot-layout)) ,value)))

(defmethod generate-predicate ((description defstruct-typed-description) layout predicate-name)
  (destructuring-bind (slot-layout name-layout) layout
    `(defun ,predicate-name (object)
       (and (typep object ',(defstruct-type description))
            (>= (length object) ,(length slot-layout))
            ,@(loop for (sym . index) in name-layout
                    collect `(eql (elt object ,index) ',sym))))))

(defmethod generate-copier ((description defstruct-typed-description) layout copier-name)
  (declare (ignore description))
  ;; TODO: Should this check the type?
  `(defun ,copier-name (object)
     (copy-seq object)))

(defun generate-typed-slot-accessor (slot index)
  `((defun ,(slot-accessor-name slot) (structure)
      (the ,(slot-type slot) (elt structure ,index)))
    ,@(unless (slot-read-only slot)
        (list `(defun (setf ,(slot-accessor-name slot)) (new-value structure)
                 (setf (elt structure ,index) (the ,(slot-type slot) new-value)))))))

(defun generate-typed-slot-accessors (description layout)
  (declare (ignore description))
  (destructuring-bind (slot-layout name-layout) layout
    (declare (ignore name-layout))
    (loop for slot in slot-layout
          for index from 0
          when slot
            append (generate-typed-slot-accessor slot index))))

(defmethod generate-defstruct-bits ((description defstruct-typed-description) layout)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (find-structure-description ',(defstruct-name description))
             ',description))
     ,@(generate-typed-slot-accessors description layout)))
