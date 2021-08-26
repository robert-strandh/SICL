(cl:in-package #:sicl-format)

;;; Return the name of a subclass to be used for a particular
;;; directive.  Each particular directive subclass must be accompanied
;;; by an eql-specialized method on this generic function.
(defgeneric directive-subclass-name (directive-character directive))

;;; Given a name of a type of a directive, return a list of parameter
;;; specifiers for that type of directive.  Each type of directive
;;; should supply an eql specialized method for this generic function.
(defgeneric parameter-specs (directive-name))

;;; Check the syntax of a directive.
(defgeneric check-directive-syntax (directive)
  (:method-combination progn :most-specific-last))

;;; DIRECTIVE is an instance of a subclass of the DIRECTIVE class
;;; describing the directive.
(defgeneric interpret-format-directive (directive))

;;; The directive compiler.
(defgeneric compile-format-directive (directive))
