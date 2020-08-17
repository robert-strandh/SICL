(cl:in-package #:sicl-structure)

(defun read-structure (stream ch p)
  (when (and p (not *read-suppress*))
    (warn "Ignored numeric argument in #~A~A." p ch))
  (cond (*read-suppress*
         (read stream t nil t)
         nil)
        (t
         (let ((form (read stream t nil t)))
           (unless (listp form)
             (error 'non-empty-list-must-follow-sharp-s :stream stream))
           (unless (oddp (list-length form))
             (error 'missing-sharp-s-argument :stream stream))
           (let* ((structure-name (first form))
                  (class (find-class structure-name nil)))
             (unless (and class (typep class 'structure-class))
               (error 'sharp-s-class-must-name-structure-class
                      :stream stream
                      :name structure-name))
             (unless (has-standard-constructor class)
               (error 'sharp-s-class-must-have-standard-constructor
                      :stream stream
                      :name structure-name))
             (apply #'make-instance class
                    ;; Convert slot names to keywords.
                    (loop
                      for (slot value) on (rest form) by #'cddr
                      collect (keywordify slot)
                      collect value)))))))
