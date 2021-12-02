(cl:in-package #:cleavir-literals)

(defgeneric convert-entry (client entry environment))

(defmethod convert-entry (client (entry entry) environment)
  (convert-initialization-form
   client (form entry) environment))

(defmethod convert-entry (client (entry creation-entry) environment)
  (convert-creation-form
   client (form entry) (lexical-location entry) environment))

(defun process-work-list (client environment)
  (let ((table *similarity-table*))
    (loop until (null (work-list table))
          do (let* ((entry (pop (work-list table)))
                    (*current-entry* entry))
               (setf (result entry)
                     (convert-entry client entry environment))))))

(defun report-circularity (entries)
  (labels ((depth-first-search (entry stack)
             (assert (member entry entries :test #'eq))
             (if (member entry stack :test #'eq)
                 (error 'circular-dependencies-in-creation-form
                        :creation-forms (mapcar #'form stack))
                 (loop for leader in (leaders entry)
                       do (depth-first-search leader (cons entry stack))))))
    (depth-first-search (first entries) '())))

(defmethod finalize-literals (client environment)
  (process-work-list client environment)
  (let ((all-entries
          (loop with table = (eql-table *similarity-table*)
                for literal-record being each hash-value of table
                collect (creation-entry literal-record)
                collect (initialization-entry literal-record)))
        (result '()))
    (loop with entries = all-entries
          until (null entries)
          do ;; Find and entry with no leaders.  The converted version
             ;; of the form of such and entry is ready to be part of
             ;; the result.
             (labels ((process-entries (entry)
                        ;; Remove the entry from the entries that
                        ;; remain, and add the converted version of
                        ;; the form to the result.
                        (setf entries (delete entry entries))
                        (push (result entry) result)
                        ;; Next, since the standard requires the
                        ;; initialization form to be executed "as soon
                        ;; as possible" after the creation form, if
                        ;; the current entry is a creation entry, we
                        ;; consider each the follower of the current
                        ;; entry, and if it has no leaders other than
                        ;; the current entry, it is recursively
                        ;; processed.
                        (when (typep entry 'creation-entry)
                          (loop for follower in (followers entry)
                                do (setf (leaders follower)
                                         (delete entry (leaders follower)))
                                   (when (null (leaders follower))
                                     (process-entries follower))))))
               (let ((entry (find-if (lambda (entry) (null (leaders entry)))
                                   entries)))
                 ;; If no such entry can be found, we have a circular
                 ;; dependency.
                 (when (null entry)
                   (report-circularity all-entries))
                 ;; Otherwise process the entry an recursively allso
                 ;; all its followers that depend only on this entry.
                 (process-entries entry))))
    (reverse result)))
