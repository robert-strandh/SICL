(cl:in-package #:sicl-string)

(defmethod report-string-condition ((c bag-is-dotted-list) stream)
  (format stream
	  "If a character bag is a list, it must be a proper list.~@
           But the following dotted list was found instead:~@
           ~s."
	  (type-error-datum c)))

(defmethod report-string-condition ((c bag-is-circular-list) stream)
  (format stream
	  "If a character bag is a list, it must be a proper list.~@
           But the following circular list was found instead:~@
           ~s."
	  (type-error-datum c)))

(defmethod report-string-condition ((c bag-contains-non-character) stream)
  (format stream
	  "A character bag must be a sequence that contains only characters.~@
           But the following element was found which is not a character:~@
           ~s."
	  (type-error-datum c)))
