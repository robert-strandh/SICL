(defpackage #:cleavir-kildall-escape
  (:use #:cl)
  (:export #:mark-dynamic-extent))

(in-package #:cleavir-kildall-escape)

;;;; Escape analysis.
;;;; Pools are alists variable -> escape-indicator.
;;;; DX, CALLED, RETURNED, CALLED-AND-RETURNED, UNKNOWN symbols are
;;;; escape indicators.
;;;; Meet is union of variables and union of indicators.
;;;; Call union "+", then:
;;;;  x + x = x
;;;;  DX + x = x
;;;;  CALLED + RETURNED = CALLED-AND-RETURNED
;;;;  CALLED + CALLED-AND-RETURNED = CALLED-AND-RETURNED
;;;;  RETURNED + CALLED-AND-RETURNED = CALLED-AND-RETURNED
;;;;  UNKNOWN + x = UNKNOWN
;;;; DX indicates that nothing really happens to the value.
;;;; Rest are obvious.
;;;; For marking purposes we currently have DX = CALLED and the
;;;; rest = UNKNOWN, but I want this for later.
;;;; UNKNOWN is meet zero.

;;; TODO: move this to cleavir-ir.
(defun variable-p (input)
  (typep input '(or cleavir-ir:lexical-location
                 cleavir-ir:values-location)))

(defclass escape (cleavir-kildall:reverse-spread-traverse
                  cleavir-kildall:interfunction-once-mixin)
  ())

(defun indicator-union (i1 i2)
  (ecase i1
    ((dx) i2)
    ((called) (ecase i2
                ((dx called) i1)
                ((returned) 'called-and-returned)
                ((called-and-returned unknown) i2)))
    ((returned) (ecase i2
                  ((dx returned) i1)
                  ((called) 'called-and-returned)
                  ((called-and-returned unknown) i2)))
    ((called-and-returned) (if (eq i2 'unknown)
                               i2
                               i1))
    ((unknown) i1)))

(defmethod cleavir-kildall:pool-meet ((s escape) p1 p2)
  (let ((result (copy-alist p2)))
    (loop for pair in p1
          for (location . indicator) = pair
          for a = (assoc location result)
          when a
            do (setf (cdr a) (indicator-union (cdr a) indicator))
          else do (push pair result))
    result))

(defun find-in-pool (input pool)
  (let ((a (assoc input pool)))
    (if a
        (cdr a)
        (error "BUG: missing input in escape analysis pool"))))

;;; <= is backwards again. x <= y if y should not overwrite x.
(defun indicator<= (i1 i2)
  (ecase i1
    ((unknown) nil)
    ((called-and-returned) (or (eq i2 'called-and-returned)
                               (eq i2 'unknown)))
    ((called returned) (or (eq i1 i2)
                           (eq i2 'called-and-returned)
                           (eq i2 'unknown)))
    ((dx) t)))

;;; iff p1 has all variables p2 does, and with <= indicators.
(defmethod cleavir-kildall:pool<= ((s escape) p1 p2)
  (every (lambda (pair2)
           (let ((pair1 (assoc (car pair2) p1)))
             (and pair1 (indicator<= (cdr pair1) (cdr pair2)))))
         p2))

(defmethod cleavir-kildall:entry-pool ((s escape) instruction)
  nil)

;;; default method: pass it along, marking inputs as unknown.
(defmethod cleavir-kildall:transfer ((s escape) instruction pool)
  (cleavir-kildall:pool-meet s
   pool
   (loop for input in (cleavir-ir:inputs instruction)
         when (variable-p input)
           collect (cons input 'unknown))))

;;; return returns.
(defmethod cleavir-kildall:transfer
    ((s escape) (instruction cleavir-ir:return-instruction) pool)
  (declare (ignore pool)) ; it's empty anyway.
  (list (cons (first (cleavir-ir:inputs instruction)) 'returned)))

;;; Assignment methods override, rather than merge.
(defmethod cleavir-kildall:transfer
    ((s escape) (instruction cleavir-ir:assignment-instruction)
     pool)
  (let ((input (first (cleavir-ir:inputs instruction))))
    (if (variable-p input)
        (acons (first (cleavir-ir:inputs instruction))
               (find-in-pool (first (cleavir-ir:outputs instruction))
                             pool)
               pool)
        pool)))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:fixed-to-multiple-instruction)
     pool)
  (loop with output-info
          = (find-in-pool (first (cleavir-ir:outputs instruction))
                          pool)
        for input in (cleavir-ir:inputs instruction)
        when (variable-p input)
          do (push (cons input output-info) pool))
  pool)

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:multiple-to-fixed-instruction)
     pool)
  (acons (first (cleavir-ir:inputs instruction))
         ;; merge all the output infos.
         (reduce #'indicator-union (cleavir-ir:outputs instruction)
                 :key (lambda (location)
                        (find-in-pool location pool)))
         pool))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:write-cell-instruction)
     pool)
  (cleavir-kildall:pool-meet s
   pool
   (list (cons (first (cleavir-ir:inputs instruction))
               ;; writing only side-effects the cell...
               ;; (but we do have to merge it, so that the pool is
               ;;  complete (FIXME: you sure?))
               'dx)
         (cons (second (cleavir-ir:inputs instruction))
               ;; ...but has mysterious consequences for the value
               'unknown))))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:read-cell-instruction)
     pool)
  (cleavir-kildall:pool-meet s
   pool
   (list (cons (first (cleavir-ir:inputs instruction)) 'dx))))

(macrolet ((defharmless (inst-class)
             `(defmethod cleavir-kildall:transfer
                  ((s escape) (instruction ,inst-class) pool)
                (harmless-pool s pool instruction))))
  (flet ((harmless-pool (s pool instruction)
           (cleavir-kildall:pool-meet s
            pool
            (loop for i in (cleavir-ir:inputs instruction)
                  when (variable-p i)
                    collect (cons i 'dx)))))
    (defharmless cleavir-ir:fdefinition-instruction)
    (defharmless cleavir-ir:the-values-instruction)
    (defharmless cleavir-ir:the-instruction)
    (defharmless cleavir-ir:eq-instruction)))

(defun transfer-call (specialization instruction pool)
  (cleavir-kildall:pool-meet specialization
   pool
   ;; calling is calling, but (for now) being an argument dooms.
   ;; this stupid thing is so that (f #'f) properly dooms.
   (let ((callee (first (cleavir-ir:inputs instruction)))
         (arguments (rest (cleavir-ir:inputs instruction))))
     (if (find callee arguments)
         (loop for input in arguments
               when (variable-p input)
                 collect (cons input 'unknown))
         (list* (cons callee 'called)
                (loop for input in arguments
                      when (variable-p input)
                        collect (cons input 'unknown)))))))

(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:funcall-instruction)
     pool)
  (transfer-call s instruction pool))
(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:funcall-no-return-instruction)
     pool)
  (transfer-call s instruction pool))
(defmethod cleavir-kildall:transfer
    ((s escape)
     (instruction cleavir-ir:multiple-value-call-instruction)
     pool)
  (transfer-call s instruction pool))

;;; infos are arrays. the nth element is what happens to the nth
;;; closed-over cell. in the future it could track arguments too,
;;; but using that information requires a forward analysis.
(defmethod cleavir-kildall:dictionary->info ((s escape) dictionary)
  ;; for the closed over cells, we rely on the segregate-lexicals
  ;; generated code, i.e.: there is exactly one variable for each
  ;; cell, one FETCH for each variable, and each has an
  ;; immediate-input with the position of that cell in the inputs.
  (let* ((fetches (loop for inst being the hash-keys of dictionary
                          using (hash-value pool)
                        when (typep inst 'cleavir-ir:fetch-instruction)
                          collect (cons inst pool)))
         (info (make-array (length fetches))))
    (dolist (fetch fetches)
      (destructuring-bind (inst . pool) fetch
        (let ((id (cleavir-ir:value (second (cleavir-ir:inputs inst))))
              (out (first (cleavir-ir:outputs inst))))
          (setf (aref info id) (find-in-pool out pool)))))
    info))

(defmethod cleavir-kildall:transfer-enclose
    ((s escape) instruction info pool)
  ;; a cell can be DXd if it it only input to DX closures that DX
  ;; their cells.
  (let* ((closure (first (cleavir-ir:outputs instruction)))
         (closure-dx (find-in-pool closure pool)))
    (cleavir-kildall:pool-meet s
      (loop for indicator across info
            for input in (cleavir-ir:inputs instruction)
            collecting (cons input
                             (indicator-union closure-dx indicator)))
      pool)))

(defun mark-one-function (enter dict)
  (cleavir-ir:map-instructions-locally
   (lambda (inst)
     (when (typep inst 'cleavir-ir:allocation-mixin)
       (let ((dxness
               (find-in-pool (first (cleavir-ir:outputs inst))
                             (gethash inst dict))))
         (when (find dxness '(dx called))
           (setf (cleavir-ir:dynamic-extent-p inst) t)))))
   enter))

(defun mark-dynamic-extent (initial-instruction)
  (check-type initial-instruction cleavir-ir:enter-instruction)
  (let* ((s (make-instance 'escape :enter initial-instruction))
         (d (cleavir-kildall:kildall s initial-instruction)))
    (setf (cleavir-kildall:dictionary s) d)
    (cleavir-kildall:map-tree #'mark-one-function s)))
