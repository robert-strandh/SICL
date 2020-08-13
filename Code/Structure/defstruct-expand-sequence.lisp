;;;; Expand sequence-based defstructs.

(cl:in-package #:sicl-structure)

(defun check-valid-defstruct-sequence (description environment)
  ;; The type must name a symbol naming a sequence type,
  ;; or a list that lookes like (VECTOR element-type).
  (let ((type (defstruct-type description)))
    (unless (or (and (symbolp type)
                     (subtypep type 'sequence environment))
                (and (consp type)
                     (eql (first type) 'vector)
                     (consp (rest type))
                     (endp (rest (rest type)))))
      (error "bad defstruct type ~S" type))))

(defun canonicalize-struct-type (type)
  (if (eql type 'vector)
      '(vector t)
      type))

(defun vector-defstruct-p (description)
  (let ((type (defstruct-type description)))
    (or (eql type 'vector)
        (and (consp type)
             (eql (first type) 'vector)))))

(defun compute-sequence-defstruct-slot-layout (description environment)
  (let* ((initial-offset (or (defstruct-initial-offset description) 0))
         (initial-offset-padding (make-list initial-offset :initial-element nil)))
    (cond ((defstruct-included-structure-name description)
           (let* ((parent-name (defstruct-included-structure-name description))
                  (parent (find-structure-description parent-name nil environment)))
             (unless parent
               (if (find-class parent-name nil environment)
                   (error "parent defstruct ~S is a structure-object defstruct, not a typed defstruct" parent-name)
                   (error "parent defstruct ~S does not exist" parent-name)))
             ;; TODO: This should do a subtypep test to make sure the types are the same/compatible...
             (unless (equal (canonicalize-struct-type (defstruct-type description))
                            (canonicalize-struct-type (defstruct-type parent)))
               (error "defstruct ~S and ~S have incompatible types ~S and ~S"
                      (defstruct-name description) (defstruct-name parent)
                      (defstruct-type description) (defstruct-type parent)))
             (multiple-value-bind (parent-slot-layout parent-name-layout)
                 (compute-sequence-defstruct-slot-layout parent environment)
               ;; Make sure there are no conflicting slots and that all the included
               ;; slots exist.
               (dolist (slot (defstruct-direct-slots description))
                 (when (find (slot-name slot) parent-slot-layout :key #'slot-name :test #'string=)
                   (error "duplicate slot name ~S" (slot-name slot))))
               (dolist (slot (defstruct-included-slots description))
                 (let ((parent-slot (find (slot-name slot) parent-slot-layout :key #'slot-name)))
                   (unless parent-slot
                     (error "included slot ~S missing from parent structure ~S"
                            (slot-name slot) (defstruct-included-structure-name description)))
                   (unless (subtypep (slot-type slot) (slot-type parent-slot) environment)
                     (error "included slot ~S has type ~S, which is not a subtype of ~S"
                            (slot-name slot) (slot-type slot) (slot-type parent-slot)))
                   ;; TODO: Check that the slot type is a subtype of the parent slot.
                   (when (and (slot-read-only parent-slot)
                              (not (slot-read-only slot)))
                     (error "included slot ~S is read-only in parent" (slot-name slot)))))
               ;; Turn the parent-slot-layout into a list of effective-ish slots
               (values (append (loop for parent-slot in parent-slot-layout
                                     collect (or (find (slot-name parent-slot)
                                                       (defstruct-included-slots description)
                                                       :key #'slot-name)
                                                 (make-instance 'slot-description
                                                                :name (slot-name parent-slot)
                                                                :accessor-name (if (defstruct-conc-name description)
                                                                                   (symbolicate (defstruct-conc-name description)
                                                                                                (slot-name parent-slot))
                                                                                   (slot-name parent-slot))
                                                                :initform (slot-initform parent-slot)
                                                                :initform-p (slot-initform-p parent-slot)
                                                                :type (slot-type parent-slot)
                                                                :read-only (slot-read-only parent-slot))))
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
               (values (append initial-offset-padding
                               (list* nil
                                      (defstruct-direct-slots description)))
                       (list (cons (defstruct-name description) initial-offset)))
               (values (append initial-offset-padding
                               (defstruct-direct-slots description))
                       '()))))))

(defun generate-sequence-ordinary-constructor (description constructor-name slot-layout name-layout)
  (let ((suppliedp-syms (loop for slot in slot-layout
                              when slot
                                collect (gensym (string (slot-name slot)))
                              else
                                collect nil))
        (object (gensym "OBJECT")))
    `(defun ,constructor-name (&key ,@(loop for slot in slot-layout
                                            for suppliedp in suppliedp-syms
                                            when slot
                                            collect (list (slot-name slot) (slot-initform slot) suppliedp)))
       (declare (ignorable ,@(remove nil suppliedp-syms)))
       (let ((,object ,(if (vector-defstruct-p description)
                           `(make-array ,(length slot-layout) :element-type ',(second (canonicalize-struct-type (defstruct-type description))))
                           `(make-sequence ',(defstruct-type description) ,(length slot-layout)))))
         ,@(loop for (name . name-index) in name-layout
                 collect `(setf (elt ,object ,name-index) ',name))
         ,@(loop for slot in slot-layout
                 for suppliedp in suppliedp-syms
                 for index from 0
                 when slot
                 collect (if (slot-initform-p slot)
                             `(setf (elt ,object ,index) ,(slot-name slot))
                             ;; If no initform was supplied, then leave the slot uninitialized.
                             `(when ,suppliedp
                                (setf (elt ,object ,index) ,(slot-name slot)))))
         ,object))))

;;;(generate-sequence-boa-constructor description environment (first constructor) (second constructor))

(defun generate-sequence-slot-accessor (slot index)
  `(progn
     (defun ,(slot-accessor-name slot) (structure)
       (the ,(slot-type slot) (elt structure ,index)))
     ,@(unless (slot-read-only slot)
         (list `(defun (setf ,(slot-accessor-name slot)) (new-value structure)
                  (setf (elt structure ,index) (the ,(slot-type slot) new-value)))))))

(defun generate-sequence-predicate (description slot-layout name-layout predicate-name)
  `(defun ,predicate-name (object)
     (and (typep object ',(defstruct-type description))
          (>= (length object) ,(length slot-layout))
          ,@(loop for (sym . index) in name-layout
                  collect `(eql (elt object ,index) ',sym)))))

(defun generate-sequence-copier (description copier-name)
  (declare (ignore description))
  ;; TODO: Should this check the type?
  `(defun ,copier-name (object)
     (copy-seq object)))

(defun expand-sequence-defstruct (description environment)
  (check-valid-defstruct-sequence description environment)
  (multiple-value-bind (slot-layout name-layout)
      (compute-sequence-defstruct-slot-layout description environment)
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (find-structure-description ',(defstruct-name description))
               ',description))
       ,@(loop for constructor in (defstruct-constructors description)
               collect (if (cdr constructor)
                           (generate-sequence-boa-constructor description environment (first constructor) (second constructor) slot-layout name-layout)
                           (generate-sequence-ordinary-constructor description (first constructor) slot-layout name-layout)))
       ,@(loop for slot in slot-layout
               for index from 0
               when slot
               collect (generate-sequence-slot-accessor slot index))
       ,@(loop for predicate-name in (defstruct-predicates description)
               collect (generate-sequence-predicate description slot-layout name-layout predicate-name))
       ,@(loop for copier-name in (defstruct-copiers description)
               collect (generate-sequence-copier description copier-name))
       ',(defstruct-name description))))
