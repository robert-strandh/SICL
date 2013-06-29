(in-package #:sicl-graph-coloring)

;;; The degree of a lexical is the number of times it appears in
;;; the conflicts, either as the CAR or as the CDR of a conflict. 
(defun degree (lexical conflicts)
  (count-if (lambda (conflict)
	      (or (eq lexical (car conflict))
		  (eq lexical (cdr conflict))))
	    conflicts))

;;; The safe rule is the rule that finds a lexical with a degree that
;;; is strictly less than the number of registers.  If such a lexical
;;; exists, this function returns it.  Otherwise it returns NIL.
(defun safe-rule (lexicals conflicts register-count)
  (find-if (lambda (lexical)
	     (< (degree lexical conflicts)
		register-count))
	   lexicals))

;;; The optimistic rule always succeeds (unless there are no lexicals,
;;; of course).  It returns the lexical with the smallest degree.
(defun optimistic-rule (lexicals conflicts register-count)
  (loop with min-degree = (+ (length lexicals) register-count)
	with result = nil
	for lexical in lexicals
	do (let ((degree (degree lexical conflicts)))
	     (when (< degree min-degree)
	       (setf min-degree degree)
	       (setf result lexical)))
	finally (return result)))

;;; Given a lexical L, and a problem instance represented by a list of
;;; lexicals LL and a list of conflicts C, return three values:
;;;
;;;  * a list of lexicals that is like LL with L removed.
;;;
;;;  * a list of CONFLICTING-ITEMS, i.e. a list where each element is
;;;    either a lexical or a register such that the element conflicts
;;;    with L according to the conflicts in C. 
;;;
;;;  * a list of remaining conflicts, which is like C but with every
;;;    conflict mentioning L removed.
;;;
;;; The first and the third return value define a subproblem to be
;;; solved.  The second return value is used to take a solution to the
;;; subproblem and to derive from it a solution to the entire problem.
(defun split-problem (lexical lexicals conflicts)
  (let ((conflicting-items '())
	(remaining-conflicts '()))
    (loop for conflict in conflicts
	  do (cond ((eq (car conflict) lexical)
		    (push (cdr conflict) conflicting-items))
		   ((eq (cdr conflict) lexical)
		    (push (car conflict) conflicting-items))
		   (t
		    (push conflict remaining-conflicts))))
    (values (remove lexical lexicals :test #'eq)
	    conflicting-items
	    remaining-conflicts)))

;;; Take a solution S to a subproblem, a lexical L that does not
;;; participate in the problem that generated S, and a list I of items
;;; that conflict with L.  Such an item could be a register, or a
;;; lexical that occurs in the solution S.  Derive from these a
;;; solution to the problem that includes L by attempting to find a
;;; register that L does not conflict with, and that has not been
;;; assigned to any lexical in S that L conflicts with.  Such a
;;; solution might not be possible, in which case we give up.
(defun patch-solution (solution lexical conflicting-items registers)
  (let* ((conflicting-registers
	   (loop for thing in conflicting-items
		 if (member thing registers :test #'eq)
		   collect thing
		 else
		   collect (cdr (assoc thing solution
				       :test #'eq))))
	 (free-register
	   (find-if (lambda (register)
		      (not (member register conflicting-registers
				   :test #'eq)))
		    registers)))
    (if (null free-register)
	(throw 'no-solution nil)
	(cons (cons lexical free-register) solution))))

;;; Solve the suproblem that is like the original problem but with one
;;; particular lexical removed.
(defun solve-sub (lexical registers lexicals conflicts register-count)
  (multiple-value-bind (new-lexicals conflicting-items new-conflicts)
      (split-problem lexical lexicals conflicts)
    (let ((solution (solve-aux registers
			       new-lexicals
			       new-conflicts
			       register-count)))
      (patch-solution solution lexical conflicting-items registers))))
	  
(defun solve-aux (registers lexicals conflicts register-count)
  (if (null lexicals)
      '()
      (let ((lexical (or (safe-rule lexicals conflicts register-count)
			 (optimistic-rule lexicals conflicts register-count))))
	(solve-sub lexical registers lexicals conflicts register-count))))

(defun solve (registers lexicals conflicts)
  ;; Do a lot of error checking
  (loop for (item . rest) on registers
	do (when (member item rest :test #'eq)
	     (error "register ~s occurs multiple times" item)))
  (loop for (item . rest) on lexicals
	do (when (member item rest :test #'eq)
	     (error "lexical ~s occurs multiple times" item)))
  (loop for (item1 . item2) in conflicts
	do (unless (member item1 lexicals :test #'eq)
	     (error "~s is not a lexical, but is in a conflict" item1))
	   (unless (or (member item2 lexicals :test #'eq)
		       (member item2 registers :test #'eq))
	     (error "~s is neither a lexical nor a register" item2)))
  (loop for ((item1 . item2) . rest) on conflicts
	do (when (or (member (cons item1 item2) rest :test #'equal)
		     (member (cons item2 item1) rest :test #'equal))
	     (error "item (~s . ~s) occurs multiple times" item1 item2)))
  (catch 'no-solution 
    (solve-aux registers lexicals conflicts (length registers))))
