(cl:in-package #:sicl-posix-high)

(defun write (file-descriptor vector &key (start 0) (end nil))
  (let ((end (if (null end) (length vector) end)))
    (let (effective-count error)
      (loop do (multiple-value-setq (effective-count error)
                 (low:write file-descriptor vector start end))
            while (= error sicl-posix-low:+eintr+))
      (ecase error
        (0 effective-count)
        ((#.sicl-posix-low:+eagain+
          #+(or)#.sicl-posix-low:+ewouldblock+)
         (error 'write-would-block
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+ebadf+
         (error 'invalid-file-descriptor
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+edestaddrreq+
         (error 'destination-address-not-set
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+edquot+
         (error 'write-exceeds-quota
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+efbig+
         (error 'write-exceeds-max-size
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+einval+
         (error 'invalid-value
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+eio+
         (error 'low-level-io-error
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+enospc+
         (error 'no-space-on-device
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+eperm+
         (error 'file-sealed
                :file-descriptor file-descriptor))
        (#.sicl-posix-low:+epipe+
         (error 'pipe-closed
                :file-descriptor file-descriptor))))))
