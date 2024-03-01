(cl:in-package #:sicl-new-boot)

(defun current-package (client)
  (let* ((global-environment (environment client))
         (package-cell
           (clostrum-sys:variable-cell
            client global-environment '*package*)))
    (cbae:symbol-value
     '*package* package-cell cbae:*dynamic-environment*)))

;;; This method is applicable when Eclector sees a symbol without a
;;; package marker, so it should always be interned in the current
;;; package.  Then, if the current package is either COMMON-LISP,
;;; COMMON-LISP-USER, or KEYWORD, we let Eclector do it's thing.
;;; Otherwise we use Parcl to find the symbol with the name
;;; SYMBOL-NAME in the current Parcl package.  If there is no such
;;; symbol, we create a host uninterned symbol, and enter it into the
;;; current Parcl package.  We also update the hash table in
;;; (SYMBOL-PACKAGE *BOOT*).
(defmethod eclector.reader:interpret-symbol
    ((client client)
     input-stream
     (package-indicator (eql :current))
     symbol-name
     internp)
  (declare (ignore input-stream))
  (when (eq (eclector.reader:state-value client '*package*)
            (find-package "KEYWORD"))
    (return-from eclector.reader:interpret-symbol
      (intern symbol-name (find-package "KEYWORD"))))
  (let ((current-package (current-package client)))
    (case current-package
      ((#.(find-package '#:common-lisp)
        #.(find-package '#:common-lisp-user)
        #.(find-package '#:keyword))
       (call-next-method))
      (otherwise
       (multiple-value-bind (symbol status)
           (parcl:find-symbol client current-package symbol-name)
         (unless (null status)
           (return-from eclector.reader:interpret-symbol symbol)))
       (let ((symbol (make-symbol symbol-name)))
         (setf (gethash symbol (symbol-package *boot*)) current-package)
         (parcl:import client current-package symbol)
         symbol)))))

;;; This method is applicable when Eclector sees a symbol with one or
;;; two package markers, If it has a single package marker, then
;;; INTERNP is false, and we should just find the symbol and return
;;; it, and error if not.  If it has two package markers, then INTERNP
;;; is true, and we should intern it no matter what.  If
;;; PACKAGE-INDICATOR is either "COMMON-LISP" or "KEYWORD", then we
;;; let Eclector do it's thing.
(defmethod eclector.reader:interpret-symbol
    ((client client)
     input-stream
     (package-indicator string)
     symbol-name
     internp)
  (declare (ignore input-stream))
  (if (or (string= package-indicator "COMMON-LISP")
          (string= package-indicator "CL")
          (string= package-indicator "KEYWORD"))
      (call-next-method)
      (multiple-value-bind (package existsp)
          (gethash package-indicator (packages *boot*))
        (unless existsp
          (error "No package named ~s" package-indicator))
        (multiple-value-bind (symbol status)
            (parcl:find-symbol client package symbol-name)
          (case status
            ((:internal :inherited)
             (if internp
                 (return-from eclector.reader:interpret-symbol symbol)
                 (error "Symbol ~a is not external in package ~a"
                        symbol-name (parcl:name client package))))
            (:external
             (return-from eclector.reader:interpret-symbol symbol))
            ((nil)
             (if internp
                 (let ((symbol (make-symbol symbol-name)))
                   (setf (gethash symbol (symbol-package *boot*)) package)
                   (parcl:import client package symbol)
                   symbol)
                 (error "Symbol ~a does not exist in package ~a"
                        symbol-name (parcl:name client package)))))))))

(defmethod parcl:make-symbol ((client client) name package)
  (let ((result (make-symbol name)))
    (unless (null package)
      (setf (gethash result (symbol-package *boot*)) package))
    result))

(defmethod parcl:symbol-name ((client client) symbol)
  (symbol-name symbol))

(defmethod parcl:symbol-package ((client client) symbol)
  (gethash symbol (symbol-package *boot*)))

(defmethod parcl:symbol-names-equal ((client client) symbol1 symbol2)
  (string= symbol1 symbol2))

(defmethod (setf parcl:symbol-package) (new-package (client client) symbol)
  (setf (gethash symbol (symbol-package *boot*)) new-package))

(defmethod parcl:find-symbol ((client client) (package package) name)
  (find-symbol name package))

(defmethod parcl:intern ((client client) (package package) name)
  (intern name package))

(defmethod parcl:make-table ((client client))
  (make-hash-table :test #'equal))

(defmethod parcl:name-to-entry ((client client) name table)
  (gethash name table))

(defmethod (setf parcl:name-to-entry) (entry (client client) name table)
  (setf (gethash name table) entry))

(defun package-designator-to-package (client package-designator)
  (typecase package-designator
    ((or string character symbol)
     (let ((package-name (string package-designator)))
       (cond ((equal package-name "KEYWORD")
              (find-package "KEYWORD"))
             (t
              (gethash package-name (packages *boot*))))))
    (null
     (current-package client))
    (otherwise
     package-designator)))

(defun create-common-lisp-package (client)
  (let* ((name1 "COMMON-LISP")
         (name2 "CL")
         (package (parcl:make-package client name1)))
    (setf (gethash name1 (packages *boot*)) package)
    (setf (gethash name2 (packages *boot*)) package)
    (loop for symbol being each external-symbol of name1
          do (setf (parcl:symbol-package client symbol) package)
             (parcl:import client package symbol)
             (parcl:export client package symbol))))

(defun define-package-functions (client global-environment)
  (setf (clo:fdefinition client global-environment 'make-package)
        (lambda (package-name &key nicknames use)
          (let ((canonicalized-name (string package-name))
                (canonicalized-nicknames (mapcar #'string nicknames))
                (canonicalized-packages
                  (loop for package-to-use in use
                        collect (package-designator-to-package
                                 client package-to-use))))
            (let ((result (parcl:make-package client canonicalized-name)))
              (setf (parcl:nicknames client result) canonicalized-nicknames)
              (parcl:use-packages client result canonicalized-packages)
              (setf (gethash package-name (packages *boot*)) result)
              result))))
  (setf (clo:fdefinition client global-environment 'find-package)
        (lambda (package-designator)
          (package-designator-to-package client package-designator)))
  (setf (clo:fdefinition client global-environment '(setf find-package))
        (lambda (package package-designator)
          (setf (gethash (string package-designator) (packages *boot*))
                package)))
  (setf (clo:fdefinition client global-environment 'find-symbol)
        (lambda (string &optional package-designator)
          (let ((package
                  (package-designator-to-package client package-designator)))
            (parcl:find-symbol client package string))))
  (setf (clo:fdefinition client global-environment 'intern)
        (lambda (string &optional package-designator)
          (let ((package
                  (package-designator-to-package client package-designator)))
            (parcl:intern client package string))))
  (setf (clo:fdefinition client global-environment 'use-package)
        (lambda (packages-to-use &optional package)
          (let ((canonicalized-packages-to-use
                  (loop for package-to-use in packages-to-use
                        collect
                        (typecase package-to-use
                          ((or string character symbol)
                           (gethash (string package-to-use)
                                    (packages *boot*)))
                          (otherwise
                           package-to-use))))
                (canonicalized-package
                  (package-designator-to-package client package)))
            (parcl:use-packages
             client canonicalized-package canonicalized-packages-to-use))))
  (setf (clo:fdefinition client global-environment 'export)
        (lambda (symbols &optional package)
          (let ((canonicalized-symbols
                  (typecase symbols
                    (cons symbols)
                    (null '())
                    (otherwise (list symbols))))
                (canonicalized-package
                  (package-designator-to-package client package)))
            (loop for symbol in canonicalized-symbols
                  do (parcl:export client canonicalized-package symbol)))))
  (setf (clo:fdefinition client global-environment 'shadow)
        (lambda (symbol-names &optional package)
          (let ((canonicalized-symbol-names
                  (etypecase symbol-names
                    (null '())
                    (symbol (list (string symbol-names)))
                    (cons (mapcar #'string symbol-names))))
                (canonicalized-package
                  (package-designator-to-package client package)))
            (loop for symbol-name in canonicalized-symbol-names
                  do (parcl:shadow
                      client canonicalized-package symbol-name))))))

(defun intern-parcl-symbol (client package-name symbol-name)
  (let* ((package (gethash package-name (packages *boot*))))
    (parcl:intern client package symbol-name)))
