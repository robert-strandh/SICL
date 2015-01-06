(defun reverse-count-0 (x list)
  (loop
    for e in (reverse list)
    when (eql x e)
      count e))
