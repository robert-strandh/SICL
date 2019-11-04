(cl:in-package #:sicl-boot-inspector)

(defun present-very-pure-object-slots (object stream)
  (loop with e5 = (sicl-boot::e5 *boot*)
        for slot in (aref (rack-of-object object) 1)
        for slot-name = (funcall (sicl-genv:fdefinition 'sicl-clos:slot-definition-name e5) slot)
        for slot-location = (funcall (sicl-genv:fdefinition 'sicl-clos:slot-definition-location e5) slot)
        for slot-value = (aref (rack-of-object object) slot-location)
        do (clouseau:format-place-row
            stream
            object
            'clouseau:pseudo-place
            slot-value
            :label (symbol-name slot-name))))
