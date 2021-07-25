(cl:in-package #:sicl-package)

(defun import (symbols &optional (package-designator *package*))
  (when (and (atom symbols) (not (null symbols)))
    (setf symbol (list symbols)))
  (unless (cleavir-code-utilities:proper-list-p symbols)
    ;; FIXME: signal a more specific error.
    (error "argument must be a proper list"))
  (unless (every #'symbolp symbols)
    ;; FIXME: signal a more specific error.
    (error "argument must be a designator for a list of symbols"))
  (let ((conflicts (make-hash-table :test #'equal)))
    (labels ((add-symbol (name symbol)
               (pushnew symbol (gethash name conflicts '()) :test #'eq))
             (maybe-add-symbol (name symbol)
               (unless (nth-value 1 (gethash name (shadowing-symbols package)))
                 (add-symbol name symbol))))
      (loop for used-package in (use-list packages)
            do (do-external-symbols (symbol used-package)
                 (maybe-add-symbol (symbol-name symbol) symbol)))
      (maphash (lambda (name symbol)
                 (add-symbol name symbol))
               (external-symbols package))
      (maphash (lambda (name symbol)
                 (add-symbol name symbol))
               (internal-symbols package)))
    (loop for symbols being each hash-value of conflicts
          when (> (length symbols) 1)
            do (let ((choice (resolve-conflicts symbols package)))
                 (if (symbol-is-present-p choice package)
                     ;; The choice was a symbol that is already
                     ;; present in PACKAGE, and we had a conflict
                     ;; involving that symbol.  Then we just leave
                     ;; things the way they are.
                     nil
                     ;; The choice was a symbol to import.  We first
                     ;; determine whether the conflict was with a
                     ;; symbol present in PACKAGE.
                     (let ((name (symbol-name choice))
                           (other (first (remove choice symbols))))
                       (if (symbol-is-present-p other package)
                           ;; The conflict was with a symbol present
                           ;; in PACKAGE.  We must replace the
                           ;; existing present symbol with the choice.
                           ;; The standard says that the new symbol
                           ;; should not be exported.
                           (progn
                             (remhash name (external-symbols package))
                             (setf (gethash name (internal-symbols package))
                                   choice)
                             ;; If and only if the present symbol was
                             ;; a shadowing one, make the choice a
                             ;; shadowing symbol as well.
                             (let ((cell (member other (shadowing-symbols package))))
                               (unless (null cell)
                                 (setf (car cell) choice))))
                           ;; The conflict was with an inherited
                           ;; symbol.  We resolve the conflict by
                           ;; importing the new symbol and making it a
                           ;; shadowing symbol.
                           (progn
                             (setf (gethash name (internal-symbols package))
                                   choice)
                             (push choice (shadowing-symbols package))))))))))
