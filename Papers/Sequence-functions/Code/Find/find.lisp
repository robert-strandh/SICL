(in-package :find)

 (defun find-vector-0 (item vector test)
   (find item vector :test test))

 (defun find-vector-1 (item vector test)
   (sicl-sequence::find-vector 
    item
    vector
    nil ; from-end
    test ; test
    nil ; test-not
    0 ; start
    (1- (length vector)) ; end
    #'identity
    ))

 (defun find-list-0 (item list test endp key)
   (find item list 
	 :test test 
	 :end (and endp (1- (length list)))
	 :key key))

 (defun find-list-1 (item list test endp key)
   (sicl-sequence::find-list
    item
    list
    nil ; from-end
    test ; test
    nil ; test-not
    0 ; start
    (and endp (1- (length list))) ; end
    (or key #'identity)))

