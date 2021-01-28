(cl:in-package #:sicl-string)

(defmethod acclimation:report-condition
    ((c bag-is-dotted-list)
     stream
     acclimation:english)
  (format stream
          "If a character bag is a list, it must be a proper list.~@
           But the following dotted list was found instead:~@
           ~s."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c bag-is-circular-list)
     stream
     acclimation:english)
  (format stream
          "If a character bag is a list, it must be a proper list.~@
           But the following circular list was found instead:~@
           ~s."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c bag-contains-non-character)
     stream
     acclimation:english)
  (format stream
          "A character bag must be a sequence that contains only characters.~@
           But the following element was found which is not a character:~@
           ~s."
          (type-error-datum c)))

(defmethod acclimation:report-condition
    ((c invalid-bounding-indices)
     stream
     acclimation:english)
  (format stream
          "In order for START and END to be valid bounding indices,~@
           START must between 0 and the length of the string, and~@
           END must be between 0 and the length of the string or NIL,~@
           and START must be less than or equal to END.~@
           But the following values of START and END were found:~@
           ~s, ~s~@
           For the string:~@
           ~s~@
           And the length of that string is ~s."
          (start c) (end c) (target c) (length (target c))))
