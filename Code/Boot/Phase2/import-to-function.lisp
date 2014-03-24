(in-package #:sicl-boot-phase2)

(shadowing-import '(define-built-in-class)
		  '#:sicl-function)

(unintern 'cl:defclass '#:sicl-symbol)

(shadow '(#:defclass)
	'#:sicl-function)
