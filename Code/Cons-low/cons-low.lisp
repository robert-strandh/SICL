(in-package :sicl.cons.low)

;;; make this check for out of memory
(defun cons (car cdr)
  (let ((pointer (allocate-words 2)))
    (store-word pointer car)
    (store-word (+ pointer (/ +machine-word-length+ 8)) cdr)
    (+ pointer +tag-cons+)))

(defun null (obj)
  (= (tag-bits obj) +tag-nil+))

(defun car (cons)
  (let ((tag (tag-bits cons)))
    (assert (or (= tag +tag-cons+) (= tag +tag-nil+))))
  (if (null cons)
      nil
      (load-word (raw-pointer cons))))

(defun cdr (x)
  (let ((tag (tag-bits cons)))
    (assert (or (= tag +tag-cons+) (= tag +tag-nil+))))
  (if (null cons)
      nil
      (load-word (+ (raw-pointer cons) (/ +machine-word-length+ 8)))))

(defun rplaca (cons object)
  (assert (= (tag-bits cons) +tag-cons+))
  (store-word (raw-pointer cons) object))
  cons)

(defun rplacd (cons object)
  (assert (= (tag-bits cons) +tag-cons+))
  (store-word (+ (raw-pointer cons) (/ +machine-word-length+ 8)) object)
  cons)

