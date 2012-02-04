(defclass ast ()
  ((%form :initarg :form :reader form)
   (%children :initarg :children :reader children)))
  

;;; This example is '((a b) (c d))
(defparameter *example*
  (let* ((a (make-instance 'ast :form 'a :children '()))
	 (b (make-instance 'ast :form 'b :children '()))
	 (c (make-instance 'ast :form 'c :children '()))
	 (d (make-instance 'ast :form 'd :children '()))
	 (ab (make-instance 'ast :form (list (form a) (form b)) :children (list a b)))
	 (cd (make-instance 'ast :form (list (form c) (form d)) :children (list c d))))
    (make-instance 'ast :form (list (form ab) (form cd)) :children (list ab cd))))

(defun transformer (list)
  `(c (,(caar list) 34) ,(car list) ,(cadr list)))

;;; Build a hash table that contains the paths of every subform of 
;;; a form.
(defun build-subform-table (form)
  (let ((ht (make-hash-table)))
    (labels ((aux (subform path)
	       (push path (gethash subform ht))
	       (when (consp subform)
		 (aux (car subform) (cons 'car path))
		 (aux (cdr subform) (cons 'cdr path)))))
      (aux form '()))
    ht))

;;; Given a form and a table containing all the subforms of some
;;; orinal form, return a list of entries.  An entry has the form
;;; (path . entry) where path is the reverse path of a subform of
;;; the form, and the entry is a list of paths in the original
;;; form to original subforms that eq to the subform at path in
;;; form.  We only include conses and atoms that are not symbols,
;;; numbers, or characters, because we don't know where they
;;; came from.
(defun find-subforms-in-table (form table)
  (let ((dico '()))
    (labels ((find-aux (form path)
	       (let ((entry (gethash form table)))
		 (cond ((or (typep form 'symbol)
			    (typep form 'number)
			    (typep form 'character))
			nil)
		       ((not (null entry))
			(push (cons path entry) dico))
		       ((consp form)
			(find-aux (car form) (cons 'car path))
			(find-aux (cdr form) (cons 'cdr path)))
		       (t nil)))))
      (find-aux form '()))
    dico))

(defun access-subform (form path)
  (loop for operation in (reverse path)
	do (setf form (funcall operation form)))
  form)

(defun replace-subform (form path new)
  (cond ((null path) new)
	((eq (car path) 'car)
	 (cons (replace-subform (car form) (cdr path) new)
	       (cdr form)))
	(t
	 (cons (car form)
	       (replace-subform (cdr form) (cdr path) new)))))

;;; Return true if path1 is a (not necessarily strict) subpath
;;; of path2.
(defun subpath-p (path1 path2)
  (let ((mismatch (mismatch path1 path2)))
    (or (null mismatch)
	(= mismatch (length path2)))))

;;; Return a form that is the result of calling
;;; the transformer on the tree, and also a list
;;; of paths in the form that almost certainly came
;;; from the tree, together with the path in that 
;;; tree where it came from.
(defun fff (original-form transformer)
  (let* ((form (funcall transformer original-form))
	 (table (build-subform-table original-form))
	 (subforms (find-subforms-in-table form original-form)))
    ;; Subforms is s list of elements of the form
    ;; (path . entry) where we are fairly sure that
    ;; the subform at the path in the transformed form
    ;; came from the original form.
    ;; Next, we try to figure out which subforms of
    ;; type symbol, number, and character that came
    ;; from the original form.  In order to do that,
    ;; we first want to limit our search to subforms
    ;; of the original form that were actually inspected
    ;; by the transformer.  We do that by trying to
    ;; replace each subform of the original form that
    ;; appeared in the transformed form by a freshly
    ;; generated symbol, and apply the transformer again.
    ;; Any attempt by the transformer to inspect parts
    ;; of the subform (using car, cdr) will result
    ;; in an error, in which case we know that 
    ;; this subform was taken apart.  In many cases,
    ;; however, the form will just be passed along without
    ;; further inspection, and we can exclude such subforms
    ;; as candidates for containing atoms that appeared
    ;; in the transformed form.
    (let ((opaque-paths
	   (remove-if (lambda (path)
			(handler-case
			    (progn (funcall transformer
					    (replace-subform original-form
							     (reverse path)
							     (gensym)))
				   nil)
			  (error () t)))
		      (reduce #'append (mapcar #'cdr subforms)))))
      ;; Next, we exclude any subpath of these paths from the
      ;; table we built from the original form.  It is safe to exclude
      ;; the path itself as well, because we know it does not contain
      ;; a symbol, a number, or a character.
      (maphash (lambda (key value)
		 (setf (gethash key table)
		       (set-difference value opaque-paths :test #'subpath-p)))
	       table)
      ;; Now we are ready to scan the transformed form to see whether any
      ;; symbols, numbers, or characters might have come from the
      ;; original form.

;;; rename it, not only compound forms
(defun find-compound-form-in-tree (form tree)
  (labels ((find-aux (tree)
	     (cond ((or (typep tree 'symbol)
			(typep tree 'number)
			(typep tree 'character))
		    nil)
		   ((eq form tree)
		    (return-from find-compound-form-in-tree form))
		   (t
		    (find-aux (car tree))
		    (find-aux (cdr tree))))))
    (find-aux tree)))

(defun find-all-compound-forms-in-tree (form tree)
  (let ((dico '()))
    (labels ((aux (form)
	       (cond ((find-compound-form-in-tree form tree)
		      (push form dico))
		     ((consp form)
		      (aux (car form))
		      (aux (cdr form)))
		     (t
		      nil))))
      (aux form)
      dico)))
		 
(defun replace-causes-error (form tree transformer)
  (handler-case (progn (funcall transformer (subst (gensym) form tree)) nil)
    (error () t)))

(defun filter-compound-forms (forms tree transformer)
  (remove-if (lambda (form) (replace-causes-error form tree transformer))
	     forms))