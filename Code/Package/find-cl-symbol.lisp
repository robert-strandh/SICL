;;; This code is preliminary, and not used at the moment.  The purpose
;;; is to implement FIND-SYMBOL for the CL package.  Since the set of
;;; symbols is fixed, we can preprocess those symbols into a form that
;;; fast to examine.  We use a technique similar to our technique for
;;; generic dispatch, but in two levels.
;;;
;;; At the first level, we discriminate using the length of the input
;;; string, and we use binary search with a fixed set of lengths,
;;; determined from the lengths of the symbol names in the CL package.
;;;
;;; At the second level, where all the symbols have the same length,
;;; we pre-select a character position that will give us maximum
;;; discrimination, and we do a binary search on the char-code of the
;;; character in that position in the input string.  We continue
;;; selecting positions until we have a unique possible answer.  If
;;; the symbol name of the answer is the same as the input string,
;;; then we have a hit.  Otherwise we don't.

;;; Names of functions and variables were not chosen very carefully,
;;; so will probably need to be modified.

;;; We should use the statistics of use of symbols in the CL package
;;; in order to select our lengths and character positions more
;;; carefully.  Though, already this code finds a symbol in the CL
;;; package almost twice as fast as SBCL FIND-SYMBOL does.

(defun unique-symbol-count (symbols position)
  (length
   (remove-duplicates
    (loop for symbol in symbols
          collect (aref (symbol-name symbol) position)))))

(defun best-position (length symbols)
  (loop with best = 0
        with max = 0
        for i from 0 below length
        for count = (unique-symbol-count symbols i)
        when (> count max)
          do (setf max count)
             (setf best i)
        finally (return best)))

(defun compute-code-alist (symbols position)
  (loop with result = '()
        for symbol in symbols
        for name = (symbol-name symbol)
        for char = (aref name position)
        for code = (char-code char)
        for entry = (assoc code result)
        do (if (null entry)
               (push (list code symbol) result)
               (push symbol (cdr entry)))
        finally (return (sort result #'< :key #'car))))

(defun make-dispatch (length symbols)
  (if (= (length symbols) 1)
      `',(first symbols)
      (let* ((best (best-position length symbols))
             (alist (compute-code-alist symbols best)))
        `(let ((code (char-code (schar name ,best))))
           (declare (type fixnum code))
           ,(make-position-dispatch length alist)))))

(defun make-position-dispatch (length alist)
  (if (= (length alist) 1)
      (make-dispatch length (rest (first alist)))
      (let* ((half (floor (length alist) 2))
             (left (subseq alist 0 half))
            (right (subseq alist half)))
        `(if (< code ,(first (first right)))
             ,(make-position-dispatch length left)
             ,(make-position-dispatch length right)))))

(defun make-length-alist ()
  (let ((result '()))
    (do-external-symbols (symbol "CL")
      (let* ((name (symbol-name symbol))
             (length (length name))
             (entry (assoc length result)))
        (if (null entry)
            (push (list length symbol) result)
            (push symbol (cdr entry)))))
    (sort result #'< :key  #'car)))

(defun make-length-dispatch (alist)
  (if (= (length alist) 1)
      (destructuring-bind ((length . symbols)) alist
        (make-dispatch length symbols))
      (let* ((half (max 1 (floor (length alist) 3)))
             (left (subseq alist 0 half))
             (right (subseq alist half)))
        `(if (< length ,(first (first right)))
             ,(make-length-dispatch left)
             ,(make-length-dispatch right)))))

(defun make-dispatch-function ()
  (compile
   nil
   `(lambda (name)
      (declare (optimize (speed 3)
                         (compilation-speed 0)
                         (debug 0)
                         (safety 0)
                         (space 0)))
      (declare (type simple-string name))
      (let ((length (length name)))
        (declare (type fixnum length))
        ,(make-length-dispatch (make-length-alist))))))
