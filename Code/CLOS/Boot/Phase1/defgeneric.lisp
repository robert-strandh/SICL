(cl:in-package #:sicl-clos)

;;; This macro is defined in preparation for reading the file
;;; containing DEFGENERIC forms of all the accessor generic functions.
;;; We know that these DEFGENERIC forms do not contain any options, so
;;; we just need to handle the parameters. 

(defmacro defgeneric (name parameters)
  `(cl:defgeneric ,name ,parameters))
