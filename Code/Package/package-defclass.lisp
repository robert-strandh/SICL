(cl:in-package #:sicl-package)

(defgeneric name (package))

(defgeneric (setf name) (new-name package))

(defgeneric nicknames (package))

(defgeneric (setf nicknames) (new-nicknames package))

(defgeneric local-nicknames (package))

(defgeneric (setf local-nicknames) (new-local-nicknames package))

(defgeneric use-list (package))

(defgeneric (setf use-list) (new-use-list package))

(defgeneric used-by-list (package))

(defgeneric (setf used-by-list) (new-used-by-list package))

(defgeneric external-symbols (package))

(defgeneric internal-symbols (package))

(defgeneric shadowing-symbols (package))

(defgeneric (setf shadowing-symbols) (new-shadowing-symbols package))

(defclass package ()
  ((%name
    :initarg :name
    :accessor name)
   (%nicknames
    :initarg :nicknames
    :initform '()
    :accessor nicknames)
   (%local-nicknames
    :initarg :nicknames
    :initform '()
    :accessor local-nicknames)
   (%use-list
    :initarg :use-list
    :initform '()
    :accessor use-list)
   (%used-by-list
    :initarg :used-by-list
    :initform '()
    :accessor used-by-list)
   (%external-symbols
    :initform (make-hash-table :test #'equal)
    :reader external-symbols)
   (%internal-symbols
    :initform (make-hash-table :test #'equal)
    :reader internal-symbols)
   (%shadowing-symbols
    :initarg :shadowing-symbols
    :initform '()
    :accessor shadowing-symbols)))
