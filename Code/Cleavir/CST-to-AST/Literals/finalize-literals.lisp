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

(defmethod finalize-literals (client environment)
  (process-work-list client environment)
  (let ((entries
          (loop with table = (eql-table *similarity-table*)
                for literal-record being each hash-value of table
                collect (creation-entry literal-record)
                collect (initialization-entry literal-record)))
        (result '()))
    (loop until (null entries)
          do ;; Find and entry with no leaders.  The converted version
             ;; of the form of such and entry is ready to be part of
             ;; the result.
             (let ((entry (find-if (lambda (entry) (null (leaders entry)))
                                   entries)))
               ;; If no such entry can be found, we have a circular
               ;; dependency.
               (when (null entry)
                 (error "circularity detected"))
               ;; Otherwise remove the entry from the entries that
               ;; remain, and add the converted version of the form to
               ;; the result.
               (setf entries (delete entry entries))
               (push (result entry) result)
               ;; Next, since the standard requires the initialization
               ;; form to be executed "as soon as possible" after the
               ;; creation form, we consider each the follower of the
               ;; current entry, and if it has no leaders other than
               ;; the current entry, it too is reader to be part of
               ;; the result.
               (loop for follower in (followers entry)
                     do (setf (leaders follower)
                              (delete entry (leaders follower)))
                        (when (null (leaders follower))
                          (setf entries (delete follower entries))
                          (push (result follower) result)))))
    (reverse result)))
