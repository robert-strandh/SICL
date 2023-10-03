(cl:in-package #:sicl-new-boot-phase-1)

;;; This variable contains a hash table of extrinsic Parcl packages.
;;; We use the host packages COMMON-LISP and KEYWORD, but all other
;;; packages created as a result of loading code into a Clostrum
;;; environment are contained in this table.
(defvar *packages*)

;;; This variable contains the current package, which can either be
;;; one of hte host packages COMMON-LISP or KEYWORD, or it can be an
;;; extrinsic Parcl package.
(defparameter *current-package* (find-package '#:common-lisp))

;;; When a target symbol is created in a package other than
;;; COMMON-LISP or KEYWORD, we instead create an uninterned host
;;; symbol, and we use this table to determine to which extrinsic
;;; Parcl package the target symbol belongs.
(defvar *symbol-package*)

(defun current-package (client)
  (let* ((global-environment (environment client))
         (package-cell
           (clostrum-sys:variable-cell
            client global-environment '*package*)))
    (cbae:symbol-value
     client '*package* package-cell cbae:*dynamic-environment*)))

;;; This method is applicable when Eclector sees a symbol without a
;;; package marker, so it should always be interned in the current
;;; package.  Then, if the current package is either COMMON-LISP or
;;; KEYWORD, then we let Eclector do it's thing.  Otherwise we use
;;; Parcl to find the symbol with the name SYMBOL-NAME in the current
;;; Parcl package, first among the external symbols and then among the
;;; internal ones.  If there is no such symbol, we create a host
;;; uninterned symbol, and enter it into the current Parcl package.
;;; We also update the hash table in *SYMBOL-PACKAGE*.
(defmethod eclector.reader:interpret-symbol
    ((client client)
     input-stream
     (package-indicator (eql :current))
     symbol-name
     internp)
  (declare (ignore input-stream))
  (let ((current-package (current-package client)))
    (case current-package
      ((#.(find-package '#:common-lisp) #.(find-package '#:keyword))
       (call-next-method))
      (otherwise
       (multiple-value-bind (symbol status)
           (parcl:find-symbol client current-package symbol-name)
         (unless (null status)
           (return-from eclector.reader:interpret-symbol symbol)))
       (let ((symbol (make-symbol symbol-name)))
         (setf (gethash symbol *symbol-package*) current-package)
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
          (gethash package-indicator *packages*)
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
                   (setf (gethash symbol *symbol-package*) package)
                   (parcl:import client package symbol)
                   symbol)
                 (error "Symbol ~a does not exist in package ~a"
                        symbol-name (parcl:name client package)))))))))

(defmethod parcl:make-symbol ((client client) name package)
  (let ((result (make-symbol name)))
    (unless (null package)
      (setf (gethash result *symbol-package*) package))))

(defmethod parcl:symbol-name ((client client) symbol)
  (symbol-name symbol))

(defmethod parcl:symbol-package ((client client) symbol)
  (gethash symbol *symbol-package*))

(defmethod (setf parcl:symbol-package) (new-package client symbol)
  (setf (gethash symbol *symbol-package*) new-package))

(defun canonicalize-package-designator (package-designator)
  (typecase package-designator
    ((or string character symbol)
     (gethash (string package-designator) *packages*))
    (null
     *current-package*)
    (otherwise
     package-designator)))

(defun define-package-functions (client global-environment)
  (setf (clostrum:fdefinition
         client global-environment 'make-package)
        (lambda (package-name &key nicknames use)
          (let ((canonicalized-name (string package-name))
                (canonicalized-nicknames (mapcar #'string nicknames))
                (canonicalized-packages
                  (mapcar #'canonicalize-package-designator use)))
            (let ((result (parcl:make-package client canonicalized-name)))
              (setf (parcl:nicknames client result) canonicalized-nicknames)
              (parcl:use-packages client result canonicalized-packages)
              result))))
  (setf (clostrum:fdefinition
         client global-environment 'use-package)
        (lambda (packages-to-use &optional package)
          (let ((canonicalized-packages-to-use
                  (loop for package-to-use in packages-to-use
                        collect
                        (typecase package-to-use
                          ((or string character symbol)
                           (gethash (string package-to-use) *packages*))
                          (otherwise
                           package-to-use))))
                (canonicalized-package
                  (canonicalize-package-designator package)))
            (parcl:use-packages
             client canonicalized-package canonicalized-packages-to-use))))
  (setf (clostrum:fdefinition
         client global-environment 'export)
        (lambda (symbols &optional package)
          (let ((canonicalized-symbols
                  (typecase symbols
                    (cons symbols)
                    (null '())
                    (otherwise (list symbols))))
                (canonicalized-package
                  (canonicalize-package-designator package)))
            (loop for symbol in canonicalized-symbols
                  do (parcl:export client canonicalized-package symbol)))))
  (setf (clostrum:fdefinition
         client global-environment 'shadow)
        (lambda (symbol-names &optional package)
          (let ((canonicalized-symbol-names
                  (etypecase symbol-names
                    (null '())
                    (symbol (list (string symbol-names)))
                    (cons (mapcar #'string symbol-names))))
                (canonicalized-package
                  (canonicalize-package-designator package)))
            (loop for symbol-name in canonicalized-symbol-names
                  do (parcl:shadow
                      client canonicalized-package symbol-name))))))
            
                        
  
