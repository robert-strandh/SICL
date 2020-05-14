(cl:in-package #:sicl-sequence)

(defun sequence-type-specifier-p (x)
  (and (subtypep x 'sequence)
       (not
        (subtypep 'sequence x))))

(deftype sequence-type-specifier ()
  '(satisfies sequence-type-specifier-p))

(deftype array-index ()
  '(integer 0 (#.(1- array-total-size-limit))))

(deftype array-length ()
  '(integer 0 (#.array-total-size-limit)))
