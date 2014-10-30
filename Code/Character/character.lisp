(cl:in-package #:sicl-character)

(defun characterp (object)
  (sicl-word:==
   (sicl-word:& object sicl-configuration:+tag-mask+)
   sicl-configuration:+tag-character+))

(proclaim '(inline characterp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CHAR=

(defun binary-char= (char1 char2)
  (declare (type character char1 char2))
  (sicl-word:== char1 char2))

(proclaim '(inline binary-char=))

(defun char= (&rest characters)
  (when (null characters)
    (error 'program-error))
  (if (null (cdr characters))
      t
      (loop for rest = (cdr characters) then (cdr rest)
	    while (consp rest)
	    for char1 = (car characters) then char2
	    for char2 = (car rest)
	    unless (binary-char= char1 char2)
	      return nil
	    finally (return t))))
  
(proclaim '(notinline char=))

(define-compiler-macro char= (&whole form &rest arguments)
  (cond ((not (and (cleavir-code-utilities:proper-list-p arguments)
	       (>= (length arguments) 1)))
	 form)
	((= (length arguments) 1)
	 `(characterp ,(car arguments)))
	(t (let* ((vars (loop for argument in arguments collect (gensym))))
	     `(let ,(loop for var in vars
			  for arg in arguments
			  collect `(,var ,arg))
		(and ,@(loop for rest = (cdr vars) then (cdr rest)
			     while (consp rest)
			     for var1 = (car vars) then var2
			     for var2 = (car rest)
			     collect `(binary-char= ,var1 ,var2))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CHAR/=

(defun binary-char/= (char1 char2)
  (declare (type character char1 char2))
  (not (sicl-word:== char1 char2)))

(proclaim '(inline binary-char/=))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CHAR<

(defun binary-char< (char1 char2)
  (declare (type character char1 char2))
  (< (the fixnum (char-code char1))
     (the fixnum (char-code char2))))

(defun char< (&rest characters)
  (when (null characters)
    (error 'program-error))
  (if (null (cdr characters))
      t
      (loop for rest = (cdr characters) then (cdr rest)
	    while (consp rest)
	    for char1 = (car characters) then char2
	    for char2 = (car rest)
	    unless (binary-char< char1 char2)
	      return nil
	    finally (return t))))
  
(proclaim '(notinline char<))

(define-compiler-macro char< (&whole form &rest arguments)
  (cond ((not (and (cleavir-code-utilities:proper-list-p arguments)
	       (>= (length arguments) 1)))
	 form)
	((= (length arguments) 1)
	 `(characterp ,(car arguments)))
	(t (let* ((vars (loop for argument in arguments collect (gensym))))
	     `(let ,(loop for var in vars
			  for arg in arguments
			  collect `(,var ,arg))
		(and ,@(loop for rest = (cdr vars) then (cdr rest)
			     while (consp rest)
			     for var1 = (car vars) then var2
			     for var2 = (car rest)
			     collect `(binary-char< ,var1 ,var2))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CHAR<=

(defun binary-char<= (char1 char2)
  (declare (type character char1 char2))
  (<= (the fixnum (char-code char1))
      (the fixnum (char-code char2))))

(defun char<= (&rest characters)
  (when (null characters)
    (error 'program-error))
  (if (null (cdr characters))
      t
      (loop for rest = (cdr characters) then (cdr rest)
	    while (consp rest)
	    for char1 = (car characters) then char2
	    for char2 = (car rest)
	    unless (binary-char<= char1 char2)
	      return nil
	    finally (return t))))
  
(proclaim '(notinline char<=))

(define-compiler-macro char<= (&whole form &rest arguments)
  (cond ((not (and (cleavir-code-utilities:proper-list-p arguments)
	       (>= (length arguments) 1)))
	 form)
	((= (length arguments) 1)
	 `(characterp ,(car arguments)))
	(t (let* ((vars (loop for argument in arguments collect (gensym))))
	     `(let ,(loop for var in vars
			  for arg in arguments
			  collect `(,var ,arg))
		(and ,@(loop for rest = (cdr vars) then (cdr rest)
			     while (consp rest)
			     for var1 = (car vars) then var2
			     for var2 = (car rest)
			     collect `(binary-char<= ,var1 ,var2))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CHAR>

(defun binary-char> (char1 char2)
  (declare (type character char1 char2))
  (> (the fixnum (char-code char1))
     (the fixnum (char-code char2))))

(defun char> (&rest characters)
  (when (null characters)
    (error 'program-error))
  (if (null (cdr characters))
      t
      (loop for rest = (cdr characters) then (cdr rest)
	    while (consp rest)
	    for char1 = (car characters) then char2
	    for char2 = (car rest)
	    unless (binary-char> char1 char2)
	      return nil
	    finally (return t))))
  
(proclaim '(notinline char>))

(define-compiler-macro char> (&whole form &rest arguments)
  (cond ((not (and (cleavir-code-utilities:proper-list-p arguments)
	       (>= (length arguments) 1)))
	 form)
	((= (length arguments) 1)
	 `(characterp ,(car arguments)))
	(t (let* ((vars (loop for argument in arguments collect (gensym))))
	     `(let ,(loop for var in vars
			  for arg in arguments
			  collect `(,var ,arg))
		(and ,@(loop for rest = (cdr vars) then (cdr rest)
			     while (consp rest)
			     for var1 = (car vars) then var2
			     for var2 = (car rest)
			     collect `(binary-char> ,var1 ,var2))))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CHAR>=

(defun binary-char>= (char1 char2)
  (declare (type character char1 char2))
  (>= (the fixnum (char-code char1))
      (the fixnum (char-code char2))))

(defun char>= (&rest characters)
  (when (null characters)
    (error 'program-error))
  (if (null (cdr characters))
      t
      (loop for rest = (cdr characters) then (cdr rest)
	    while (consp rest)
	    for char1 = (car characters) then char2
	    for char2 = (car rest)
	    unless (binary-char>= char1 char2)
	      return nil
	    finally (return t))))
  
(proclaim '(notinline char>=))

(define-compiler-macro char>= (&whole form &rest arguments)
  (cond ((not (and (cleavir-code-utilities:proper-list-p arguments)
	       (>= (length arguments) 1)))
	 form)
	((= (length arguments) 1)
	 `(characterp ,(car arguments)))
	(t (let* ((vars (loop for argument in arguments collect (gensym))))
	     `(let ,(loop for var in vars
			  for arg in arguments
			  collect `(,var ,arg))
		(and ,@(loop for rest = (cdr vars) then (cdr rest)
			     while (consp rest)
			     for var1 = (car vars) then var2
			     for var2 = (car rest)
			     collect `(binary-char>= ,var1 ,var2))))))))
  
