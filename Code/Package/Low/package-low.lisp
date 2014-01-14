(in-package #:sicl-package-low)

(define-built-in-class package ()
  ((%name
    :initarg :name
    :accessor package-name)
   (%nicknames
    :initarg :nicknames
    :initform '()
    :accessor package-nicknames)
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

   
   
