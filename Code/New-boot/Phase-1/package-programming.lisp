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
  (case *current-package*
    ((#.(find-package '#:common-lisp) #.(find-package '#:keyword))
     (call-next-method))
    (otherwise
     (multiple-value-bind (symbol existsp)
         (parcl:find-external-symbol client *current-package* symbol-name)
       (when existsp
         (return-from eclector.reader:interpret-symbol symbol)))
     (multiple-value-bind (symbol existsp)
         (parcl:find-internal-symbol client *current-package* symbol-name)
       (when existsp
         (return-from eclector.reader:interpret-symbol symbol)))
     (let ((symbol (make-symbol symbol-name)))
       (setf (gethash symbol *symbol-package*) *current-package*)
       (parcl:add-internal-symbol client *current-package* symbol)
       symbol))))

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
        (multiple-value-bind (symbol existsp)
            (parcl:find-external-symbol client package symbol-name)
          (when existsp
            (return-from eclector.reader:interpret-symbol symbol)))
        (multiple-value-bind (symbol existsp)
            (parcl:find-internal-symbol client package symbol-name)
          (when existsp
            (if internp
                (return-from eclector.reader:interpret-symbol symbol)
                (error "Symbol ~a is not external in package ~a"
                       symbol-name (parcl:name client package)))))
        (if internp
            (let ((symbol (make-symbol symbol-name)))
              (setf (gethash symbol *symbol-package*) package)
              (parcl:add-internal-symbol client package symbol)
              symbol)
            (error "Symbol ~a does not exist in package ~a"
                   symbol-name (parcl:name client package))))))
