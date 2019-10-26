(cl:in-package #:sicl-character)

(deftype standard-char ()
  `(member . #.(append (loop for code from (char-code #\a) to (char-code #\z)
                             collect (code-char code))
                       (loop for code from (char-code #\A) to (char-code #\Z)
                             collect (code-char code))
                       (loop for code from (char-code #\0) to (char-code #\9)
                             collect (code-char code))
                       (loop for code from (char-code #\!) to (char-code #\/)
                             collect (code-char code))
                       '(#\: #\; #\< #\= #\> #\? #\@
                         #\[ #\\ #\] #\^ #\_ #\` #\{ #\| #\} #\~))))
