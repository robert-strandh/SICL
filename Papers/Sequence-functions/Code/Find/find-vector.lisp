(in-package :find)

 (defun find-vector-0 (item vector test)
   (find item vector :test test))

 (defun find-vector-1 (item vector test)
   (sicl-sequence::find-vector item vector nil test nil 0 (length vector) #'identity))

 (defun find-list-0 (item list test)
   (find item vector :test test))

 (defun find-list-1 (item list test)
   (sicl-sequence::find-vector item vector nil test nil 0 (length vector) #'identity))

