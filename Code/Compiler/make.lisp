(cl:in-package #:sicl-program)

;;; FIXME: perhaps I should figure out whether I can accomplish what
;;; is being done here by using ASDF instead of rolling my own.

;;;; A DEPENDENCE is a list of the form (DEPENDER FUN . DEPENDEES)
;;;; where DEPENDEES is a list of symbols that DEPENDER depends on,
;;;; and FUN is the NAME of a function that takes a single argument (a
;;;; program) and that must be called in order to "generate" the
;;;; symbol.

(defun verify-no-cyclic-dependencies (dependencies)
  (let ((table (make-hash-table :test #'eq)))
    (labels ((traverse (symbol)
	       (when (gethash symbol table)
		 (error "~s depends on itself" symbol))
	       (setf (gethash symbol table) t)
	       (let ((entry (find symbol dependencies :test #'eq :key #'car)))
		 ;; Don't require the entry to exist here.
		 (mapc #'traverse (cddr entry)))
	       (setf (gethash symbol table) nil)))
      (loop for entry in dependencies
	    do (traverse (car entry))))))

(defparameter *dependencies* '())

(defun add-dependencies (depender dependees)
  (when (null (find depender *dependencies* :test #'eq :key #'car))
    (error "no such entry: ~s" depender))
  ;; Before commiting to adding these dependencies, we copy the
  ;; existing set of dependencies to a temporary list, add the new
  ;; ones to the temporary list, and check that there are no cycles.
  (let* ((temp (mapcar #'copy-list *dependencies*))
	 (entry (find depender temp :test #'eq :key #'car)))
    (loop for dependee in dependees
	  do (pushnew dependee (cddr entry) :test #'eq))
    (verify-no-cyclic-dependencies temp)
    (setf *dependencies* temp)))

(defun set-processor (depender fun)
  (let ((entry (find depender *dependencies* :test #'eq :key #'car)))
    (cond ((null entry)
	   (push (list depender fun) *dependencies*))
	  ((eq (cadr entry) fun)
	   nil)
	  (t
	   (error "attempt to replace ~s by ~s" (cadr entry) fun)))))

;;; We use DEFVAR here so that we won't reset the current timestamp
;;; just because we recompile and reload this file. 
(defvar *current-timestamp* 0)

(defun older-than-p (depender dependee timestamps)
  (let ((depender-timestamp (gethash depender timestamps))
	(dependee-timestamp (gethash dependee timestamps)))
    (assert (not (null dependee-timestamp)))
    (or (null depender-timestamp)
	(< depender-timestamp dependee-timestamp))))

(defun set-timestamp (target timestamps)
  (setf (gethash target timestamps) (incf *current-timestamp*)))

(defun copy-timestamps (timestamps)
  (let ((new (make-hash-table :test #'eq)))
    (maphash (lambda (key value)
	       (setf (gethash key new) value))
	     timestamps)
    new))

(defun make (program target &key dry-run)
  (check-type target symbol)
  (let ((*program* program)
	(timestamps (timestamps program))
	(modify-p nil))
    (declare (special *program*))
    (when dry-run
      (setf timestamps (copy-timestamps timestamps)))
    (labels ((aux (target)
	       (let ((entry (find target *dependencies*
				  :test #'eq :key #'car)))
		 (when (null entry)
		   (error "don't know how to make ~s" target))
		 (loop for dependee in (cddr entry)
		       do (aux dependee))
		 (when (find-if (lambda (dependee)
				  (older-than-p target dependee timestamps))
				(cddr entry))
		   (setf modify-p t)
		   (if dry-run
		       (format t "calling ~s~%" (cadr entry))
		       (funcall (cadr entry) program))
		   (set-timestamp target timestamps)))))
      (aux target))
    modify-p))

(defun touch (program target)
  (set-timestamp target (timestamps program)))
