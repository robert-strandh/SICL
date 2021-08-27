(cl:in-package #:sicl-character)

(declaim (ftype (function (string (integer 0)) character)
                char))

(declaim (ftype (function (character) t)
                char-code))

(declaim (ftype (function (character) character)
                char-upcase char-downcase))

(declaim (ftype (function (&rest list) t)
                char=
                char/=
                char<
                char>
                char<=
                char>=
                char-equal
                char-not-equal
                char-lessp
                char-greaterp
                char-not-greaterp
                char-not-lessp))

(declaim (ftype (function (character) (integer 0))
                char-int))

(declaim (ftype (function (character) (or string null))
                char-name))

(declaim (ftype (function (character-designator)
                          character)
                character))

(declaim (ftype general-predicate
                characterp))
