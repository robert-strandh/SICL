(defun reverse-count-1 (x list)
  (loop
    for e in (reverse list)
    count (eql x e)))
