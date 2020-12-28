(cl:in-package #:structure-grammar)

(defclass terminal () ())

(defclass non-terminal ()
  ((%children :initarg :chilcren :reader children)))

(defclass or (non-terminal)
  ())

(defclass suite (non-terminal)
  ())

(defclass * (suite)
  ())

(defclass + (suite)
  ())

(defclass ? (suite)
  ())

(defgeneric item-class (class-name &rest initargs))

(defclass item ()
  ((%class-name
    :initarg :class-name
    :reader class-name)
   (%indentation
    :initform 0
    :initarg :indentation
    :reader indentation)))

(defmethod item-class (class-name &rest initargs)
  (declare (ignore initargs))
  (values (find-class 'item) '()))

(defclass or-item (item)
  ())

(defmethod item-class ((class-name (eql 'or)) &rest initargs)
  (declare (ignore initargs))
  (values (find-class 'or-item) '()))

(defclass suite-item (item)
  ((%from :initarg :from :reader from)
   (%to :initarg :to :reader to)))

(defmethod item-class ((class-name (eql 'suite)) &rest initargs)
  (values (find-class 'suite-item) initargs))

(defmethod item-class ((class-name (eql '*)) &rest initargs)
  (declare (ignore initargs))
  (values (find-class 'suite-item) '(:from 0 :to nil)))

(defmethod item-class ((class-name (eql '+)) &rest initargs)
  (declare (ignore initargs))
  (values (find-class 'suite-item) '(:from 1 :to nil)))

(defmethod item-class ((class-name (eql '?)) &rest initargs)
  (declare (ignore initargs))
  (values (find-class 'suite-item) '(:from 0 :to 1)))

(defclass rule ()
  ((%left-hand-side
    :initarg :left-hand-side
    :reader left-hand-side)
   (%right-hand-side
    :initarg :right-hand-side
    :reader right-hand-side)))


