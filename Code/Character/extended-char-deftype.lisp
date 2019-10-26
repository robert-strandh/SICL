(cl:in-package #:sicl-character)

(deftype extended-char ()
  '(and character (not base-char)))
