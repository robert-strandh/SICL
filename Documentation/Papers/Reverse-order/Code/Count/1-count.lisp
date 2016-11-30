(defun reverse-count-1 (x list)
  (declare (optimize
            (speed 3) (debug 0) (safety 0)
            (compilation-speed 0)))
  (loop for e in (reverse list)
        count (eq x e)))
