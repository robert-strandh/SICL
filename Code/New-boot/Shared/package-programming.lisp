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
           (parcl-low:find-symbol client current-package symbol-name)
         (unless (null status)
           (return-from eclector.reader:interpret-symbol symbol)))
       (let ((symbol (make-symbol symbol-name)))
         (setf (gethash symbol (symbol-package *boot*)) current-package)
         (parcl-low:import client current-package symbol)
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
      (let ((package (gethash package-indicator (packages *boot*))))
        (when (null package)
          (error "No package named ~s" package-indicator))
        (multiple-value-bind (symbol status)
            (parcl-low:find-symbol client package symbol-name)
          (case status
            ((:internal :inherited)
             (if internp
                 (return-from eclector.reader:interpret-symbol symbol)
                 (error "Symbol ~a is not external in package ~a"
                        symbol-name (parcl-low:name client package))))
            (:external
             (return-from eclector.reader:interpret-symbol symbol))
            ((nil)
             (if internp
                 (let ((symbol (make-symbol symbol-name)))
                   (setf (gethash symbol (symbol-package *boot*)) package)
                   (parcl-low:import client package symbol)
                   symbol)
                 (error "Symbol ~a does not exist in package ~a"
                        symbol-name (parcl-low:name client package)))))))))

(defmethod parcl-low:make-symbol ((client client) name package)
  (let ((result (make-symbol name)))
    (unless (null package)
      (setf (gethash result (symbol-package *boot*)) package))
    result))

(defmethod parcl-low:symbol-name ((client client) symbol)
  (symbol-name symbol))

(defmethod parcl-low:symbol-package ((client client) symbol)
  (gethash symbol (symbol-package *boot*)))

(defmethod parcl-low:symbol-names-equal ((client client) symbol1 symbol2)
  (string= symbol1 symbol2))

(defmethod (setf parcl-low:symbol-package) (new-package (client client) symbol)
  (setf (gethash symbol (symbol-package *boot*)) new-package))

(defmethod parcl-low:find-symbol ((client client) (package package) name)
  (find-symbol name package))

(defmethod parcl-low:intern ((client client) (package package) name)
  (intern name package))

(defmethod parcl-low:make-table ((client client))
  (make-hash-table :test #'equal))

(defmethod parcl-low:name-to-entry ((client client) name table)
  (gethash name table))

(defmethod (setf parcl-low:name-to-entry) (entry (client client) name table)
  (setf (gethash name table) entry))

(defun package-designator-to-package (client environment package-designator)
  (typecase package-designator
    ((or string character symbol)
     (let ((package-name (string package-designator))
           (current-package
             (env:symbol-value '*package* :environment environment)))
       (let ((local-nicknames
               (if (typep current-package 'package)
                   '()
                   (parcl-low:local-nicknames client current-package))))
         (let ((entry (assoc package-name local-nicknames
                             :test #'string=)))
           (if (null entry)
               (cond ((equal package-name "KEYWORD")
                      (find-package "KEYWORD"))
                     (t
                      (gethash package-name (packages *boot*))))
               (second entry))))))
    (null
     (current-package client))
    (otherwise
     package-designator)))

(defun create-common-lisp-package (client)
  (let* ((name1 "COMMON-LISP")
         (name2 "CL")
         (package (parcl-low:make-package client name1)))
    (setf (gethash name1 (packages *boot*)) package)
    (setf (gethash name2 (packages *boot*)) package)
    (loop for symbol being each external-symbol of name1
          do (setf (parcl-low:symbol-package client symbol) package)
             (parcl-low:import client package symbol)
             (parcl-low:export client package symbol))))

(defun define-package-functions (client global-environment)
  (setf (fdefinition (find-symbol (symbol-name 'find-package)
                                  '#:sicl-new-boot-parcl-extrinsic))
        (lambda (package-designator)
          (package-designator-to-package
           client global-environment package-designator)))
  (setf (fdefinition (find-symbol (symbol-name 'store-package)
                                  '#:sicl-new-boot-parcl-extrinsic))
        (lambda (package name nicknames)
          (loop for name in (cons name nicknames)
                do (setf (gethash name (packages *boot*)) package))))
  (setf (clo:fdefinition client global-environment 'make-package)
        (fdefinition (find-symbol (symbol-name 'make-package)
                                  '#:sicl-new-boot-parcl-extrinsic)))
  (setf (clo:fdefinition client global-environment 'find-package)
        (lambda (package-designator)
          (package-designator-to-package
           client global-environment package-designator)))
  (setf (clo:fdefinition client global-environment '(setf find-package))
        (lambda (package package-designator)
          (setf (gethash (string package-designator) (packages *boot*))
                package)))
  (setf (clo:fdefinition client global-environment 'find-symbol)
        (fdefinition (find-symbol (symbol-name 'find-symbol)
                                  '#:sicl-new-boot-parcl-extrinsic)))
  (setf (clo:fdefinition client global-environment 'intern)
        (fdefinition (find-symbol (symbol-name 'intern)
                                  '#:sicl-new-boot-parcl-extrinsic)))
  (setf (clo:fdefinition client global-environment 'use-package)
        (fdefinition (find-symbol (symbol-name 'use-package)
                                  '#:sicl-new-boot-parcl-extrinsic)))
  (setf (clo:fdefinition client global-environment 'export)
        (fdefinition (find-symbol (symbol-name 'export)
                                  '#:sicl-new-boot-parcl-extrinsic)))
  (setf (clo:fdefinition client global-environment 'shadow)
        (fdefinition (find-symbol (symbol-name 'shadow)
                                  '#:sicl-new-boot-parcl-extrinsic)))
  ;; For the purpose of bootstrapping, the environment will contain a
  ;; symbol SICL-NEW-BOOT:ADD-PACKAGE-LOCAL-NICKNAME and it will have
  ;; as its FDEFINITION that of ADD-PACKAGE-LOCAL-NICKNAME in the
  ;; extrinsic Parcl package.
  (let ((symbol 'add-package-local-nickname))
    (setf (clo:fdefinition client global-environment symbol)
          (fdefinition (find-symbol (symbol-name symbol)
                                    '#:sicl-new-boot-parcl-extrinsic)))))

(defun intern-parcl-symbol (client package-name symbol-name)
  (let* ((package (gethash package-name (packages *boot*))))
    (parcl-low:intern client package symbol-name)))
