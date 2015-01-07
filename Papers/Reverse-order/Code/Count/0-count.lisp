(defun reverse-count-0 (x list)
  (loop
    for e in (reverse list)
    count (eql x e)))
