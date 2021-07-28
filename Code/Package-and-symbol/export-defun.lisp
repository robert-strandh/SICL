(cl:in-package #:sicl-package)

(defun export-to-one-using-package (symbols using-package)
  (let ((conflicts (make-hash-table :test #'equal))
        (used-packages (use-list using-package)))
    (flet ((maybe-add-symbol (name symbol)
             (unless
                 (nth-value 1 (gethash name (shadowing-symbols using-package)))
               (pushnew symbol (gethash name conflicts '()) :test #'eq))))
      (loop for used-package in used-packages
            do (do-external-symbols (symbol used-package)
                 (maybe-add-symbol (symbol-name symbol) symbol)))
      (maphash (lambda (name symbol)
                 (maybe-add-symbol name symbol))
               (external-symbols using-package))
      (maphash (lambda (name symbol)
                 (maybe-add-symbol name symbol))
               (internal-symbols using-package)))
    (loop for shadowing-symbol in (shadowing-symbols package)
          do (remhash (symbol-name shadowing-symbol) conflicts))
    (loop for symbols being each hash-value of conflicts
          when (> (length symbols) 1)
            do (let* ((choice (resolve-conflicts symbols package))
                      (name (symbol-name choice))
                      (other (first (remove choice symbols))))
                 (cond ((symbol-is-present-p choice using-package)
                        ;; The choice was a symbol that is already
                        ;; present in USING-PACKAGE, and we had a
                        ;; conflict involving that symbol, so it can
                        ;; not have been a shadowing symbol.  Make it
                        ;; one.
                        (push choice (shadowing-symbols using-package)))
                       ((member choice symbols)
                        ;; The choice was a symbol that is going to
                        ;; become accessible in USING-PACKAGE.  There
                        ;; are two possibilities.  Either the conflict
                        ;; was with a symbol that is already present
                        ;; in USING-PACKAGE, or the conflict was with
                        ;; a symbol accessible by inheritance in one
                        ;; of the used packages.
                        (if (symbol-is-present-p other using-package)
                            ;; We need to unintern the existing symbol
                            ;; and then intern the new one.  However,
                            ;; we don't just call UNINTERN, because
                            ;; then, if the existing symbol was a
                            ;; shadowing one, new conflicts might
                            ;; uncovered.  We think that if the
                            ;; existing symbol was a shadowing symbol, we
                            ;; want the new one to be shadoing as well.
                            (progn
                              (remhash name (external-symbols using-package))
                              (remhash name (internal-symbols using-package))
                              (let ((cell (member
                                           other
                                           (shadowing-symbols using-package))))
                                (unless (null cell)
                                  (setf (car cell choice)))))
                            ;; The conflict was with a symbol
                            ;; accessible by inheritance.  We intern
                            ;; the new symbol and make it a shadowing
                            ;; one.
                            (progn
                              (setf (gethash name (internal-symbols using-package))
                                    choice)
                              (push choice (shadowing-symbols using-package)))))
                       (t
                        ;; The choice was a symbol accessible by inheritance
                        (setf (gethash name (internal-symbols using-package))
                              choice)
                        (push choice (shadowing-symbols using-package))))))))

(defun export (symbols-designator &optional package-designator *package*)
  (let ((package (package-designator-to-package package-designator))
        (symbols (designated-list-of-symbols symbols-designator)))
    (loop for using-package in (used-by-list package)
          do (export-to-one-using-package symbols using-package)))
  t)
