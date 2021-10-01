(cl:in-package #:sicl-posix-high)

(defun read (file-descriptor vector-or-desired-count &key (start 0) (end nil))
  (let ((end (if (null end) (length vector-or-desired-count) end)))
    (multiple-value-bind (effective-count error)
        (low:read file-descriptor vector-or-desired-count start end)
      (ecase error
        (0 effective-count)
        ((#.sicl-posix-low:+eagain+
          #+(or)#.sicl-posix-low:+ewouldblock+)
         (error 'read-would-block
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+ebadf+
         (error 'invalid-file-descriptor
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+einval+
         (error 'invalid-value
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+eio+
         (error 'low-level-io-error
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+eisdir+
         (error 'file-descriptor-refers-to-directory
                :file-descriptor file-descriptor))))))
