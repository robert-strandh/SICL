(cl:in-package #:sicl-package)

(defun use-package
    (designators-of-packages-to-use &optional package-designator)
  (when (atom designators-of-packages-to-use)
    (setf designators-of-packages-to-use
          (list designators-of-packages-to-use)))
  (unless (cleavir-code-utilities:proper-list-p designators-of-packages-to-use)
    ;; FIXME: signal a more specific error.
    (error "argument must be a proper list"))
  (let ((packages-to-use
          (mapcar #'package-designator-to-package
                  designators-of-packages-to-use))
        (package (package-designator-to-package package-designator))
        (conflicts (make-hash-table :test #'equal)))
    (flet ((maybe-add-symbol (name symbol)
             (unless (nth-value 1 (gethash name (shadowing-symbols package)))
               (pushnew symbol (gethash name conflicts '()) :test #'eq))))
      (loop for package-to-use in packages-to-use
            do (do-external-symbols (symbol package-to-use)
                 (maybe-add-symbol (symbol-name symbol) symbol)))
      (loop for used-package in (use-list package)
            do (do-external-symbols (symbol used-package)
                 (maybe-add-symbol (symbol-name symbol) symbol)))
      (maphash (lambda (name symbol)
                 (maybe-add-symbol name symbol))
               (external-symbols package))
      (maphash (lambda (name symbol)
                 (maybe-add-symbol name symbol))
               (internal-symbols package)))
    (loop for shadowing-symbol in (shadowing-symbols package)
          do (remhash (symbol-name shadowing-symbol) conflicts))
    (loop for symbols being each hash-value of conflicts
          when (> (length symbols) 1)
            do (let ((choice (resolve-conflicts symbols package)))
                 (if (symbol-is-present-p choice package)
                     ;; The choice was a symbol that is already
                     ;; present in PACKAGE, and we had a conflict
                     ;; involving that symbol, so it can not have been
                     ;; a shadowing symbol.  Make it one.
                     (push choice (shadowing-symbols package))
                     ;; The choice was a symbol in one of the packages
                     ;; to use.  The chosen symbol must be turned into
                     ;; a shadowing symbol in PACKAGE.
                     (let ((name (symbol-name choice)))
                       (remhash name (internal-symbol package))
                       (remhash name (external-symbols package))
                       (setf (shadowing-symbols package)
                             (remove name (shadowing-symbols package)
                                     :key #'symbol-name
                                     :test #'equal))
                       (setf (gethash name (internal-symbols package))
                             choice)
                       (push choice (shadowing-symbols package))))))
    (loop for package-to-use in packages-to-use
          do (pushnew package-to-use (use-list package) :test #'eq)
             (pushnew package (used-by-list package-to-use) :test #'eq))))
