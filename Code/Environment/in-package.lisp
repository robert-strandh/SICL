;;;; The purpose of this file is to create the macro IN-PACKAGE.

;;; Since IN-PACKAGE does not exist when this file is compiled, we
;;; must do what IN-PACKAGE does manually.  In this case, we set the
;;; *PACKAGE* variable to the package SICL-ENV.
(eval-when (:compile-toplevel)
  (setq *package* (find-package '#:sicl-env)))

(defmacro in-package (string-designator)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (in-package-function ',string-designator)))
