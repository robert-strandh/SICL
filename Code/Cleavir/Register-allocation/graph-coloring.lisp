(cl:in-package #:cleavir-register-allocation)

;;; The degree of a lexical is the number of times it appears in
;;; the conflicts, either as the CAR or as the CDR of a conflict.
(defun degree (lexical conflicts)
  (count-if (lambda (conflict)
              (or (eq lexical (car conflict))
                  (eq lexical (cdr conflict))))
            conflicts))

;;; The safe rule is the rule that finds a lexical that fulfills two
;;; criteria:
;;;
;;;   * The lexical does not have a required register as indictated by
;;;     the fact that when REQ-FUN is applied to the lexical, NIL is
;;;     returned.
;;;
;;;   * The degree of the lexical is strictly less than the number of
;;;     registers.
;;;
;;; If such a lexical exists, this function returns it.  Otherwise it
;;; returns NIL.
(defun safe-rule (lexicals conflicts register-count req-fun)
  (find-if (lambda (lexical)
             (and (null (funcall req-fun lexical))
                  (< (degree lexical conflicts)
                     register-count)))
           lexicals))

;;; The optimistic rule finds a lexical that fulfills two criteria:
;;;
;;;   * The lexical does not have a required register as indictated by
;;;     the fact that when REQ-FUN is applied to the lexical, NIL is
;;;     returned.
;;;
;;;   * The degree if the lexical is the one with the smallest degree
;;;     among all the lexicals.
;;;
;;; If such a lexical exists, this function returns it.  Otherwise it
;;; returns NIL.
(defun optimistic-rule (lexicals conflicts req-fun)
  (loop with min-degree = most-positive-fixnum
        with result = nil
        for lexical in lexicals
        do (let ((degree (degree lexical conflicts)))
             (when (and (null (funcall req-fun lexical))
                        (< degree min-degree))
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
(defun patch-solution (solution lexical conflicting-items registers pref-fun)
  (let* ((conflicting-registers
           (loop for thing in conflicting-items
                 if (member thing registers :test #'eq)
                   collect thing
                 else
                   collect (cdr (assoc thing solution
                                       :test #'eq))))
         (free-registers (set-difference registers conflicting-registers
                                         :test #'eq)))
    (cond ((null free-registers)
           ;; There are no free registers, so no solution was found.
           (throw 'no-solution nil))
          ((and (not (null (funcall pref-fun lexical)))
                (member (funcall pref-fun lexical) free-registers :test #'eq))
           ;; We are in luck.  The preferred register for the lexical
           ;; is free, so we assign it to the lexical and return the
           ;; resulting solution.
           (cons (cons lexical (funcall pref-fun lexical)) solution))
          (t
           ;; Either the preferred register of the lexical is not
           ;; free, or the lexical does not have a preffered register,
           ;; but there are other free registers, so we pick a
           ;; different one.
           (cons (cons lexical (car free-registers)) solution)))))

;;; Solve the subproblem that is like the original problem but with one
;;; particular lexical removed.
(defun solve-sub
    (lexical registers lexicals conflicts register-count req-fun pref-fun)
  (multiple-value-bind (new-lexicals conflicting-items new-conflicts)
      (split-problem lexical lexicals conflicts)
    (let ((solution (solve-aux registers
                               new-lexicals
                               new-conflicts
                               register-count
                               req-fun
                               pref-fun)))
      (patch-solution solution lexical conflicting-items registers pref-fun))))

(defun solve-aux (registers lexicals conflicts register-count req-fun pref-fun)
  (flet ((rule-1 (lexicals conflicts)
           (safe-rule lexicals conflicts register-count req-fun))
         (rule-2 (lexicals conflicts)
           (optimistic-rule lexicals conflicts req-fun)))
    (labels
        ((aux (lexicals conflicts)
           (if (null lexicals)
               '()
               (let ((lexical (or (rule-1 lexicals conflicts)
                                  (rule-2 lexicals conflicts))))

                 (if (null lexical)
                     ;; This situation happens when all of the
                     ;; lexicals have required registers.  Then there
                     ;; is a solution if and only if each such lexical
                     ;; has a different requried register.
                     (let ((required-registers (mapcar req-fun lexicals)))
                       (if (= (length required-registers)
                              (length (remove-duplicates required-registers
                                                         :test #'eq)))
                           ;; Each lexical has a different required
                           ;; register. build a solution and return
                           ;; it.
                           (mapcar (lambda (lexical)
                                     (cons lexical (funcall req-fun lexical)))
                                   lexicals)
                           ;; Otherwise there is a conflict in that
                           ;; some two lexicals require the same
                           ;; register.
                           (throw 'no-solution nil)))
                     ;; Some lexical with no required register was
                     ;; chosen by one of the rules.
                     (multiple-value-bind (new-lexicals
                                           conflicting-items
                                           new-conflicts)
                         (split-problem lexical lexicals conflicts)
                       (let ((solution (aux new-lexicals new-conflicts)))
                         (patch-solution solution lexical conflicting-items
                                         registers pref-fun))))))))
      (aux lexicals conflicts))))

;;; REQ-FUN is a function that takes a lexical location and returns a
;;; REQUIRED REGISTER for that location, or NIL if the lexical
;;; location does not have a required register.  PREF-FUN is a
;;; function that takes a location and returns a PREFERRED REGISTER
;;; for that location of NIL if the lexical location does not have a
;;; preferred register.
(defun solve (registers lexicals conflicts req-fun pref-fun)
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
    (solve-aux registers
               lexicals
               conflicts
               (length registers)
               req-fun
               pref-fun)))
