(cl:in-package #:cleavir-hir-transformations)

;;;; Escape analysis.
;;;; Basically we figure out what happens to values by checking all
;;;; instructions used by variables those values can be in. A more
;;;; sophisticated analysis would account for control flow.

;;; First off we can do a simple analysis of closures (NOT cells).
;;; A closure can certainly be DX'd if it is not used as input to
;;; any instructions but assignments, some stuff like EQ, and as
;;; the zeroth argument to a call (i.e. as the function called).
;;; This is extremely simplistic, but I think it will be practically
;;; adequate... except that it excludes recursive functions, which
;;; are stored in cells (not an allowed operation above).
;;; If it was expanded it must be remembered that (f) could return
;;; #'f, from eg (labels ((f () #'f)) ...)

;;; Cells (NOT the values in them) are harder. There are only three
;;; valid operations on cells: read, write, and enclose. The latter
;;; two don't affect accessibility (since writing a cell into a cell
;;; is NOT allowed), but the last could. To see if an enclose
;;; affects accessibility, we have to know what the enter does with
;;; its cells. Then if the closure is called, that accessiblity info
;;; becomes relevant.
;;; Our first approximation is that a cell is DXable if it is only
;;; enclosed in functions that do not unDX that cell.
;;; A function unDXes a cell if it non-DX encloses it, or encloses
;;; it in a function that unDXes that cell (it is assumed all closures
;;; are called).
;;; That means we start at leaf functions (that don't have closures)
;;; and move up. function-tree.lisp provides this functionality.
;;; Confusing!

;;; Given a location, return all locations that have the value in it
;;; assigned to it, transitively.
;;; This is the part that control flow analysis could maybe improve
;;; Other restrictions on assignment could probably simplify/speed.
(defun locations-could-contain (location)
  (flet ((location-aliases (location)
           ;; performs one step, ie locations this is assigned to
           ;; plus the location itself.
           (cons
            location
            (loop for inst
                    in (cleavir-ir:using-instructions location)
                  when (typep inst
                              '(or cleavir-ir:assignment-instruction
                                cleavir-ir:fixed-to-multiple-instruction
                                cleavir-ir:multiple-to-fixed-instruction))
                    append (cleavir-ir:outputs inst)))))
    (let ((current (list location)))
      (loop for next = (reduce #'union current
                               :key #'location-aliases)
            until (subsetp next current) ; short for set-equal
            do (setf current next)
            finally (return next)))))

;; T if location (expected to be output of ENCLOSE or CREATE-CELL)
;; is dxable.
(defun track-value (location table)
  (labels ((location-dxable (location)
             (every (lambda (instruction)
                      (dxable instruction location table))
                    (cleavir-ir:using-instructions location))))
    (let ((locations (locations-could-contain location)))
      (every #'location-dxable locations))))

;; Does the instruction definitely preserve DXness of the given input?
;; TABLE contains info about enter instructions and maybe more later
(defgeneric dxable (instruction location table))

;;; default: assume no.
(defmethod dxable (instruction location table)
  (declare (ignore instruction location table))
  nil)

;;; writing an object into a cell blackholes it.
;;; if the object IS a cell, writing something into it doesn't
;;; change its accessibility.
(defmethod dxable
    ((instruction cleavir-ir:write-cell-instruction) location table)
  (declare (ignore table))
  (not (eq (second (cleavir-ir:inputs instruction)) location)))

;;; basically same idea as writing.
;;; this assumes that cells are already black holes, so it doesn't
;;; bother checking the output or anything.
(defmethod dxable
    ((instruction cleavir-ir:read-cell-instruction) location table)
  (declare (ignore location table))
  t)

(defmethod dxable
    ((instruction cleavir-ir:enclose-instruction) location table)
  (and (cleavir-ir:dynamic-extent-p instruction)
       (let ((info (find-function-dxness
                    (cleavir-ir:code instruction)
                    table)))
         (function-cell-dxable
          info (position location (cleavir-ir:inputs instruction))))))

(defun call-dxness (instruction location)
  ;; True iff not an argument.
  ;; Again, only valid if functions can't return themselves.
  (not (find location (rest (cleavir-ir:inputs instruction)))))

(defmethod dxable
    ((instruction cleavir-ir:funcall-instruction) location table)
  (declare (ignore table))
  (call-dxness instruction location))

(defmethod dxable
    ((instruction cleavir-ir:funcall-no-return-instruction)
     location table)
  (declare (ignore table))
  (call-dxness instruction location))

(defmethod dxable
    ((instruction cleavir-ir:multiple-value-call-instruction)
     location table)
  (declare (ignore table))
  (call-dxness instruction location))

(defun find-function-dxness (enter table)
  (let ((a (assoc enter table)))
    (if a
        (cdr a)
        (error "BUG: function-tree in escape got confused somehow!"))))

(defun function-cell-dxable (info position)
  (if (array-in-bounds-p info position)
      (aref info position)
      (error "BUG: bad DX info!")))

(defun analyze-enter-dx (enter table)
  ;; analyze encloses first, we need it for cell analysis
  ;; (a more sophisticated analysis will have to do both at once)
  (dolist (enclose (cleavir-ir:local-instructions-of-type
		    enter
		    'cleavir-ir:enclose-instruction))
    (when (track-value (first (cleavir-ir:outputs enclose)) table)
      (print enclose)
      (setf (cleavir-ir:dynamic-extent-p enclose) t)))
  ;; next we analyze created, not closed-over, cells.
  ;; this could be done before or after those, doesn't matter.
  (dolist (create (cleavir-ir:local-instructions-of-type
                   enter
                   'cleavir-ir:create-cell-instruction))
    (when (track-value (first (cleavir-ir:outputs create)) table)
      (print create)
      (setf (cleavir-ir:dynamic-extent-p create) t)))
  ;; for the closed over cells, we rely on the segregate-lexicals
  ;; generated code, i.e.: there is exactly one variable for each
  ;; cell, one FETCH for each variable, and each has an
  ;; immediate-input with the number.
  (let* ((fetches (cleavir-ir:local-instructions-of-type
                   enter
                   'cleavir-ir:fetch-instruction))
         (dxness (make-array (length fetches))))
    (dolist (fetch fetches)
      (let ((id (cleavir-ir:value (second (cleavir-ir:inputs fetch))))
            (out (first (cleavir-ir:outputs fetch))))
        (setf (aref dxness id) (track-value out table))))
    dxness))

;; returns an enter->closure-info alist, but the primary thing is
;; side-effecting dynamic-extent-p s to true.
;; Despite the argument name, this accepts tree-nodes. But it should
;; only be done recursively. Users just call with the top.
(defun mark-dxness (function-tree)
  ;; We have to do leaf functions first, and move in. So, recur.
  (check-type function-tree tree-node)
  (let* ((table (mapcan #'mark-dxness (children function-tree)))
         (enter (enter-instruction function-tree))
         (info (analyze-enter-dx enter table)))
    (acons enter info table)))
