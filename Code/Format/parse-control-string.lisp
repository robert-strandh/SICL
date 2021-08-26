(cl:in-package #:sicl-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsing a control string

;;; Parse a parameter.  This function is called only if a parameter
;;; should be there, either because it is the first possible parameter
;;; position, and we have verified that the character at the start
;;; position is a member of "',vV#+-0123456789" or because the
;;; previous character was a comma, so there ought to be a parameter
;;; next.  If no parameter is found, signal an error.  Return two
;;; values, the parameter that was parsed and the position immediately
;;; beyond the parameter that was parsed.
(defun parse-parameter (string start end tilde-position)
  (cond ((= start end)
         (error 'end-of-control-string-error
                :control-string string
                :tilde-position tilde-position
                :why "expected a parameter"))
        ((eql (char string start) #\,)
         ;; Indicates absence of parameter.
         (values nil start))
        ((or (eql (char string start) #\v) (eql (char string start) #\V))
         ;; Indicates that the value is to be taken from the arguments.
         (values 'v (1+ start)))
        ((eql (char string start) #\#)
         ;; Indicates that the value is the remaining number of arguments
         (values '|#| (1+ start)))
        ((eql (char string start) #\')
         (incf start)
         (when (= start end)
           (error 'end-of-control-string-error
                  :control-string string
                  :tilde-position tilde-position
                  :why "character expected"))
         (values (char string start) (1+ start)))
        ((find (char string start) "+-0123456789")
         (multiple-value-bind (value position)
             (parse-integer string :start start :junk-allowed t)
           (when (null value)
             (error 'expected-integer-error
                    :control-string string
                    :tilde-position tilde-position
                    :index start))
           (values value position)))
        (t
         (error 'expected-parameter-start
                :control-string string
                :tilde-position tilde-position
                :index start))))

;;; Parse the parameters of a format directive.  STRING is the entire
;;; control string START is the position of the tilde character that
;;; starts the directive.  END is the length of the control string.
;;; Return the list of parameters and the position immediately beyond
;;; the last parameter.
(defun parse-parameters (string start end)
  (let ((position (1+ start))
        (parameters '()))
    (when (find (char string position) "',vV#+-0123456789")
      (multiple-value-bind (parameter pos)
          (parse-parameter string position end start)
        (push parameter parameters)
        (setf position pos))
      (loop while (and (not (= position end))
                       (eql (char string position) #\,))
            do (progn (incf position)
                      (multiple-value-bind (parameter pos)
                          (parse-parameter string position end start)
                        (push parameter parameters)
                        (setf position pos)))))
    (values (nreverse parameters) position)))

;;; Parse the modifiers of a format directive.  The colon and at-sign
;;; modifiers are optional and can appear in any order.  However, we
;;; do not allow more than one of each kind.  Return three values, a
;;; boolean indicating whether the colon modifier was found, a boolean
;;; indicating whether the at-sign modifier was found, and the first
;;; position beyond the modifiers in the string.
(defun parse-modifiers (string start end tilde-position)
  (let ((position (position-if-not (lambda (char)
                                     (or (eql char #\@)
                                         (eql char #\:)))
                                   string
                                   :start start)))
    (when (null position)
      (setf position end))
    (cond ((= position start)
           (values nil nil start))
          ((= position (1+ start))
           (if (eql (char string start) #\:)
               (values t nil (1+ start))
               (values nil t (1+ start))))
          ((= position (+ start 2))
           (if (eql (char string start)
                    (char string (1+ start)))
               (error 'two-identical-modifiers
                      :control-string string
                      :tilde-position tilde-position
                      :index start)
               (values t t (+ start 2))))
          (t
           (error 'more-than-two-modifiers
                  :control-string string
                  :tilde-position tilde-position
                  :index start)))))

;;; Parse a format directive.  The string is a format control string.
;;; The start position is the position of the tilde character that
;;; starts the directive.  Return the the character indicating the
;;; directive, a list of format parameters, two booleans indicating
;;; whether the colon and the at-sign modifiers were given, and the
;;; position in the string immediately beyond the character indicating
;;; the directive.
(defun parse-format-directive (string start)
  (let ((end (length string)))
    (multiple-value-bind (parameters position1)
        (parse-parameters string start end)
      (multiple-value-bind (colonp at-signp position2)
          (parse-modifiers string position1 end start)
        (when (= position2 end)
          (error 'end-of-control-string-error
                 :control-string string
                 :tilde-position start
                 :why "expected a letter corresponding to a format directive"))
        ;; We need to handle the special cases of the ~Newline and ~/
        ;; directives, because those directive comprise characters
        ;; that follow the directive character itself.
        (let ((directive-character (char string position2)))
          (incf position2)
          (cond ((eql directive-character #\Newline)
                 ;; I think we must assume standard syntax here, because
                 ;; there is no portable way of checking the syntax type of
                 ;; a character.
                 (loop while (and (< position2 end)
                                  (find (char string position2)
                                        #(#\Space #\Tab #\Page #\Return)))
                  do (incf position2)))
                ((eql directive-character #\/)
                 (let ((position-of-trailing-slash
                        (position #\/ string :start position2)))
                   (when (null position-of-trailing-slash)
                     (error 'end-of-control-string-error
                            :control-string string
                            :tilde-position start
                            :why "expected a trailing slash"))
                   (setf position2 (1+ position-of-trailing-slash)))))
          (values directive-character parameters colonp at-signp position2))))))
