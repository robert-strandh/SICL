;; standard version
(defun reverse-count-0 (x list)
  (count x list :from-end t :test #'eq))
