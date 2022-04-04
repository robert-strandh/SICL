(cl:in-package #:sicl-boot-phase-6)

(defun load-closer-mop (e5)
  ;; Closer mop contains loops over the symbols in two packages in
  ;; order to create the CLOSER-COMMON-LISP package.  And this loop
  ;; expands to an invocation of WITH-PACKAGE-ITERATOR.  But we are
  ;; dealing with host packges here, so the SICL version of
  ;; WITH-PACKAGE-ITERATOR won't work.  And we can't import it from
  ;; the host either, because the host expands it to a bunch of
  ;; internal host functions.  Luckily, we don't need the
  ;; CLOSER-COMMON-LISP package at all.  So the solution here is to
  ;; define WITH-PACKAGE-ITERATOR to return no symbols at all.
  (setf (env:macro-function (env:client e5) e5 'with-package-iterator)
        (lambda (form environment)
          (declare (ignore environment))
          `(flet ((,(caadr form) () '()))
             ,@(cddr form))))
  (let ((*features* '(:sicl)))
    (ensure-asdf-system '#:closer-mop e5)))
