(in-package #:sicl-boot-phase2)

(shadowing-import '(define-built-in-class)
		  '#:sicl-symbol)

;;; Don't know why uninterning is necessary.
(unintern 'cl:symbol-package '#:sicl-symbol)
(unintern 'cl:symbol-name '#:sicl-symbol)

(shadow '(#:symbol-package #:symbol-name)
	'#:sicl-symbol)
