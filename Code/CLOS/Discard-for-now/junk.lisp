;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trial code.

;;; Given a list of interval endpoints, two points considered to be to
;;; the left of the leftmost and to the right of the righmost endpoint
;;; in that interval, generate a tree of tests that determine a unique
;;; interval. 
;; (defun compute-test-tree (l low high action)
;;   (let ((length (length l)))
;;     (cond ((= length 1)
;; 	   `(if (< x ,(car l)) 
;; 		,(funcall action low (car l))
;; 		,(funcall action (car l) high)))
;; 	  ((= length 2)
;; 	   `(if (< x ,(car l))
;; 		,(funcall action low (car l))
;; 		(if (< x ,(cadr l))
;; 		    ,(funcall action (car l) (cadr l))
;; 		    ,(funcall action (cadr l) high))))
;; 	  (t
;; 	   (let* ((half (floor length 2))
;; 		  (a (subseq l 0 half))
;; 		  (b (subseq l half)))
;; 	     `(if (< x ,(car b))
;; 		  ,(compute-test-tree a low (car b) action)
;; 		  ,(compute-test-tree (cdr b) (car b) high action)))))))

;;; Definition: An INTERVAL LIST is a non-empty list with an even
;;; number of unique integers, sorted in increasing order.  These
;;; integers represent intervals so that (s1 e1 s2 e2 ... sn en)
;;; represents the set of intervals [s1 .. e1-1], [s2 .. e2-1],
;;; ... [sn .. en-1].  Each interval is nonempty, i.e, we can be sure
;;; that si < ei.  Intervals are also do not overlap, i.e., we can be
;;; sure that ei <= s(i+1).  Two consecutive intervals i and i+1 are
;;; said to TOUCH if and only if ei = s(i+1).  An interval list in
;;; which no two intervals touch is said to be CANONICAL.

;;; Canonicalize an interval list that might not already be canonical.
;;; When the interval list is not canonical, it might have two
;;; consecutive numbers that are equal, where the first one represents
;;; the end of one interval ei, and the second one the beginning of
;;; the following interval s(i+1).  But since each interval is
;;; non-empty, there can only be two consecutive equal numbers.
(defun canonicalize-interval-list (interval-list)
  (labels ((eliminate-pairs (l)
	     (if (null (cdr l))
		 l
		 (if (= (car l) (cadr l))
		     (eliminate-pairs (cddr l))
		     (list* (car l) (cadr l) (eliminate-pairs (cddr l)))))))
    (cons (car interval-list) (eliminate-pairs (cdr interval-list)))))

;;; We are given a canonical interval list and a single integer n,
;;; representing the interval [n .. n].  We return a new list
;;; representing the canonical union of all these intervals.  There
;;; are essentially three cases.  Case 1 is when [n .. n] is a
;;; sub-interval of one of the intervals given.  Case 2 is when [n
;;; .. n] touches one of the intervals.  Finally, case 3 is when we
;;; have neither case 1 nor case 2.
(defun interval-union (interval-list n)
  (labels ((add-interval (l)
	     (cond ((or (null l) (< n (car l)))
		    (list* n (1+ n) l))
		   ((< n (cadr l))
		    l)
		   (t
		    (list* (car l) (cadr l) (add-interval (cddr l)))))))
    (canonicalize-interval-list (add-interval interval-list))))
		     
;;; An CACHE LIST is a list of canonical interval lists (see above),
;;; one for each required argument of a generic function, and that
;;; represents combinations of classes that have been used in some
;;; previous invocation of the generic function. 

;;; An EFFECTIVE METHOD ENTRY is a list of three elements representing
;;; combinations of classes of required arguments that result in the
;;; invocation of a particular effective method.  The first element of
;;; the list is a cache list (see above).  The second element is an
;;; effective method components object (see above) and the third
;;; element is the effective method itself.

;;; Each generic function maintains a cache consisting of a list of
;;; effective method entries, one entry for each effective method that
;;; has been invoked as a result of invoking the generic function
;;; itself.  

