(cl:in-package #:sicl-package)

(defclass package (t)
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
    :initarg :external-symbols
    :initform '()
    :accessor external-symbols)
   (%internal-symbols
    :initarg :internal-symbols
    :initform '()
    :accessor internal-symbols)
   (%shadowing-symbols
    :initarg :shadowing-symbols
    :initform '()
    :accessor shadowing-symbols)))
