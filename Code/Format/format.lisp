;;; A portable implementation of the Common Lisp FORMAT function.
;;;
;;; Status:
;;;
;;;   Not all directives are implemented.
;;;
;;; TODO:
;;;
;;;   * Implement the last couple of directives.
;;;
;;;   * Implement the directive compiler.
;;;
;;;   * Improve some of the condition reports.
;;;
;;;   * We might want to use reader conditionals to determine how
;;;     to handle things like counting colums (for the TAB directive),
;;;     because it could be costly (and/or imprecise) to count each
;;;     character output.
;;;
;;;
;;; To think about:
;;;
;;;   * Should we use ASSERT as opposed to ERROR to get correctable
;;;     errors?
;;;
;;;   * Should we put in restarts?
;;;
;;;   * What do we do about the possibility that the syntax categories
;;;     of some characters might be altered (for ignored newline
;;;     directive)?

(cl:in-package #:sicl-format)

;;; During runtime, this variable is bound to a stream to which
;;; all the output goes.
(defvar *destination*)

(defun interpret-items (items)
  (loop for item across items
        do (if (stringp item)
               (write-string item *destination*)
               (interpret-format-directive item))))

;;; Runtime environment

;;; A vector of all the arguments that were passed to this
;;; invocation of FORMAT.
(defvar *arguments*)

;;; An index into the vector of arguments indicating the next
;;; argument to treat.
(defvar *next-argument-pointer*)

;;; A tag for catch/throw to use by the ~^ directive
(defvar *catch-tag*)

(defun compute-parameter-value (directive parameter-spec)
  (let* ((parameter-name (car parameter-spec))
         (compile-time-value (funcall parameter-name directive)))
    (cond ((null compile-time-value)
           ;; The parameter was not given at all in the format control
           ;; string, neither as a constant value, nor as a value to
           ;; be acquired at runtime (# or V).  We must use a default
           ;; value if it has any.
           (getf (cdr parameter-spec) :default-value))
          ((eq compile-time-value 'V)
           ;; The parameter was given the explicit value V in the
           ;; format control string, meaning we use the next argument
           ;; to acquire the value of the parameter.  We must test
           ;; that there are more arguments, consume the next one, and
           ;; check that the type of the argument acquired is correct.
           (when (>= *next-argument-pointer*
                     (length *arguments*))
             (error 'no-more-arguments))
           (let ((argument (aref *arguments*
                                 *next-argument-pointer*)))
             (incf *next-argument-pointer*)
             (unless (typep argument (getf (cdr parameter-spec) :type))
               (error 'argument-type-error
                      :expected-type
                      (getf (cdr parameter-spec) :type)
                      :datum
                      argument))
             argument))
          ((eq compile-time-value '|#|)
           ;; The parameter was given the explicit value # in the
           ;; format control string, meaning we use the number of
           ;; remaining arguments as the value of the parameter.
           (let ((number-of-remaining-arguments
                  (- (length *arguments*) *next-argument-pointer*)))
             (unless (typep number-of-remaining-arguments
                            (getf (cdr parameter-spec) :type))
               (error 'argument-type-error
                      :expected-type
                      (getf (cdr parameter-spec) :type)
                      :datum
                      number-of-remaining-arguments))
             number-of-remaining-arguments))
          (t
           ;; The parameter was given an explicit value (number or
           ;; character) in the format control string, and this is the
           ;; value we want.
           compile-time-value))))

;;; The directive interpreter.

(defmethod interpret-format-directive (directive)
  (error 'unknown-format-directive
         :control-string (control-string directive)
         :tilde-position (start directive)
         :index (1- (end directive))))

(defmacro define-format-directive-interpreter (class-name &body body)
  `(defmethod interpret-format-directive ((directive ,class-name))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp))
       directive
       (let ,(loop for parameter-spec in (parameter-specs class-name)
                   collect `(,(car parameter-spec)
                              (compute-parameter-value directive ',parameter-spec)))
         ,@body))))

(defun consume-next-argument (type)
  (when (>= *next-argument-pointer* (length *arguments*))
    (error 'no-more-arguments))
  (let ((arg (aref *arguments* *next-argument-pointer*)))
    (incf *next-argument-pointer*)
    (unless (typep arg type)
      (error 'argument-type-error
             :expected-type type
             :datum arg))
    arg))

(defmacro define-format-directive-compiler (class-name &body body)
  `(defmethod compile-format-directive ((directive ,class-name))
     (with-accessors ((control-string control-string)
                      (start start)
                      (end end)
                      (colonp colonp)
                      (at-signp at-signp)
                      (given-parameters given-parameters)
                      ,@(loop for parameter-spec in (parameter-specs class-name)
                              collect `(,(car parameter-spec) ,(car parameter-spec))))
       directive
       ,@body)))

(defun compile-time-value (directive slot-name)
  (let ((value (slot-value directive slot-name)))
    (cond ((or (eq value '|#|) (eq value 'V))
           nil)
          ((null value)
           (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
             (getf (cdr (find slot-name parameter-specs :key #'car)) :default-value)))
          (t
           value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Code for individual directives

;;; Signal an error of a modifier has been given for such a directive.
(defmethod check-directive-syntax progn ((directive no-modifiers-mixin))
  (with-accessors ((colonp colonp)
                   (at-signp at-signp)
                   (control-string control-string)
                   (end end))
    directive
    (when (or colonp at-signp)
      (error 'directive-takes-no-modifiers
             :directive directive))))

;;; Signal an error of an at-sign has been given for such a directive.
(defmethod check-directive-syntax progn ((directive only-colon-mixin))
  (with-accessors ((at-signp at-signp)
                   (control-string control-string)
                   (end end))
    directive
    (when at-signp
      (error 'directive-takes-only-colon
             :directive directive))))

;;; Signal an error of a colon has been given for such a directive.
(defmethod check-directive-syntax progn ((directive only-at-sign-mixin))
  (with-accessors ((colonp colonp)
                   (control-string control-string)
                   (end end))
    directive
    (when colonp
      (error 'directive-takes-only-at-sign
             :directive directive))))

;;; Signal an error if both modifiers have been given for such a directive.
(defmethod check-directive-syntax progn ((directive at-most-one-modifier-mixin))
  (with-accessors ((colonp colonp)
                   (at-signp at-signp)
                   (control-string control-string)
                   (end end))
    directive
    (when (and colonp at-signp)
      (error 'directive-takes-at-most-one-modifier
             :directive directive))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1 Basic output

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.1 ~c Character

(define-directive #\c c-directive (named-parameters-directive) ())

(define-format-directive-interpreter c-directive
  (let ((char (consume-next-argument 'character)))
    (cond ((and (not colonp) (not at-signp))
           ;; Neither colon nor at-sign.
           ;; The HyperSpec says to do what write-char does.
           (write-char char *destination*))
          ((not at-signp)
           ;; We have only a colon modifier.
           ;; The HyperSpec says to do what write-char does for
           ;; printing characters, and what char-name does otherwise.
           ;; The definition of "printing char" is a graphic character
           ;; other than space.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (princ (char-name char) *destination*)))
          ((not colonp)
           ;; We have only an at-sign modifier.
           ;; The HyperSpec says to print it the way the Lisp
           ;; reader can understand, which I take to mean "use PRIN1".
           ;; It also says to bind *print-escape* to t.
           (let ((*print-escape* t))
             (prin1 char *destination*)))
          (t
           ;; We have both a colon and and at-sign.
           ;; The HyperSpec says to do what ~:C does, but
           ;; also to mention unusual shift keys on the
           ;; keyboard required to type the character.
           ;; I don't see how to do that, so we do the same
           ;; as for ~:C.
           (if (and (graphic-char-p char) (not (eql char #\Space)))
               (write-char char *destination*)
               (princ (char-name char) *destination*))))))

(define-format-directive-compiler c-directive
  `(let ((char (consume-next-argument 'character)))
     ,(cond ((and (not colonp) (not at-signp))
             `(write-char char *destination*))
            ((not at-signp)
             `(if (and (graphic-char-p char) (not (eql char #\Space)))
                 (write-char char *destination*)
                 (princ (char-name char) *destination*)))
            ((not colonp)
             `(let ((*print-escape* t))
                (prin1 char *destination*)))
            (t
             `(if (and (graphic-char-p char) (not (eql char #\Space)))
                  (write-char char *destination*)
                  (princ (char-name char) *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.2 ~% Newline.

(define-directive #\% percent-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter percent-directive
  (loop repeat how-many
        do (terpri *destination*)))

(define-format-directive-compiler percent-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(loop repeat how-many
                  do (terpri *destination*)))
          ((< how-many 3)
           `(progn ,@(loop repeat how-many
                           collect `(terpri *destination*))))
          (t `(loop repeat ,how-many
                    do (terpri *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.3 ~& Fresh line and newlines.

(define-directive #\& ampersand-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter ampersand-directive
  (unless (zerop how-many)
    (fresh-line *destination*)
    (loop repeat (1- how-many)
          do (terpri *destination*))))

(define-format-directive-compiler ampersand-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(unless (zerop how-many)
              (fresh-line *destination*)
              (loop repeat (1- how-many)
                    do (terpri *destination*))))
          ((zerop how-many)
           nil)
          ((< how-many 3)
           `(progn (fresh-line *destination*)
                   ,@(loop repeat (1- how-many)
                           collect `(terpri *destination*))))
          (t `(progn (fresh-line *destination*)
                     (loop repeat ,(1- how-many)
                           do (terpri *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.4 ~| Page separators.

(define-directive #\| vertical-bar-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter vertical-bar-directive
  (loop repeat how-many
        do (write-char #\Page *destination*)))

(define-format-directive-compiler vertical-bar-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(loop repeat how-many
                  do (write-char #\Page *destination*)))
          ((< how-many 3)
           `(progn ,@(loop repeat how-many
                           collect `(write-char #\Page *destination*))))
          (t `(loop repeat ,how-many
                    do (write-char #\Page *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.1.5 ~~ Tildes.

(define-directive #\~ tilde-directive (named-parameters-directive no-modifiers-mixin)
    ((how-many :type (integer 0) :default-value 1)))

(define-format-directive-interpreter tilde-directive
  (loop repeat how-many
        do (write-char #\~ *destination*)))

(define-format-directive-compiler tilde-directive
  (let ((how-many (compile-time-value directive 'how-many)))
    (cond ((null how-many)
           `(loop repeat how-many
                  do (write-char #\~ *destination*)))
          ((< how-many 3)
           `(progn ,@(loop repeat how-many
                           collect `(write-char #\~ *destination*))))
          (t `(loop repeat ,how-many
                    do (write-char #\~ *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2 Radix control

(defun print-radix-arg (radix colonp at-signp mincol padchar commachar comma-interval)
  (let ((argument (consume-next-argument t)))
    (if (not (integerp argument))
        (let ((*print-base* 10))
          (format *destination* "~a" argument))
        (let* ((string (let ((*print-base* radix))
                         (princ-to-string (abs argument))))
               (comma-length (if colonp
                                 (max 0 (floor (1- (length string)) comma-interval))
                                 0))
               (sign-length (if (or at-signp (minusp argument)) 1 0))
               (total-length (+ (length string) comma-length sign-length))
               (pad-length (max 0 (- mincol total-length))))
          ;; Print the padding.
          (loop repeat pad-length
                do (write-char padchar *destination*))
          ;; Possibliy print a sign.
          (cond ((minusp argument)
                 (write-char #\- *destination*))
                (at-signp
                 (write-char #\+ *destination*))
                (t nil))
          ;; Print the string in reverse order
          (loop for index downfrom (1- (length string)) to 0
                for c across string
                do (write-char c *destination*)
                do (when (and colonp
                              (plusp index)
                              (zerop (mod index comma-interval)))
                     (write-char commachar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.1 ~r Radix.

(define-directive #\r r-directive (named-parameters-directive)
    ((radix :type (integer 2 36) :default-value nil)
     (mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

;;; Print an integer as roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-roman (integer stream)
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (princ (case hundreds
               (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CD")
               (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "CM"))
             stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (princ (case tenths
                 (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XL")
                 (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "XC"))
               stream)
        (princ (case rest
                 (0 "") (1 "I") (2 "II") (3 "III") (4 "IV")
                 (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "IX"))
               stream)))))

;;; Print an integer as old roman numerals to the stream.
;;; The integer must be strictly greater than zero,
;;; and strictly less than 4000.
(defun print-as-old-roman (integer stream)
  (multiple-value-bind (thousands rest) (floor integer 1000)
    (loop repeat thousands
          do (write-char #\M stream))
    (multiple-value-bind (hundreds rest) (floor rest 100)
      (princ (case hundreds
               (0 "") (1 "C") (2 "CC") (3 "CCC") (4 "CCCC")
               (5 "D" ) (6 "DC") (7 "DCC") (8 "DCCC") (9 "DCCCC"))
             stream)
      (multiple-value-bind (tenths rest) (floor rest 10)
        (princ (case tenths
                 (0 "") (1 "X") (2 "XX") (3 "XXX") (4 "XXXX")
                 (5 "L" ) (6 "LX") (7 "LXX") (8 "LXXX") (9 "LXXXX"))
               stream)
        (princ (case rest
                 (0 "") (1 "I") (2 "II") (3 "III") (4 "IIII")
                 (5 "V" ) (6 "VI") (7 "VII") (8 "VIII") (9 "VIIII"))
               stream)))))

(defparameter *cardinal-ones*
  #(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))

(defparameter *cardinal-teens*
  #("ten" "eleven" "twelve" "thirteen" "fourteen"
    "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"))

(defparameter *cardinal-tens*
  #(nil nil "twenty" "thirty" "fourty"
    "fifty" "sixty" "seventy" "eighty" "ninety"))

(defparameter *groups-of-three*
  #(nil "thousand" "million" "billion" "trillion" "quadrillion"
    "quintillion" "sextillion" "septillion" "octillion" "nonillion"
    "decillion" "undecillion" "duodecillion" "tredecillion"
    "quattuordecillion" "quindecillion" "sexdecillion"
    "septendecillion" "octodecillion" "novemdecillion" "vigintillion"))

;;; print a cardinal number between 1 and 99
(defun print-cardinal-tenths (n stream)
  (cond ((< n 10)
         (princ (aref *cardinal-ones* n) stream))
        ((< n 20)
         (princ (aref *cardinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (princ (aref *cardinal-tens* tens) stream)
           (unless (zerop ones)
             (princ "-" stream)
             (princ (aref *cardinal-ones* ones) stream))))))

;;; print a cardinal number between 1 and 999
(defun print-cardinal-hundreds (n stream)
  (cond ((< n 100)
         (print-cardinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (princ (aref *cardinal-ones* hundreds) stream)
           (princ " hundred" stream)
           (unless (zerop rest)
             (princ " " stream)
             (print-cardinal-tenths rest stream))))))

;;; print a cardinal number n such that 0 < n < 10^65
(defun print-cardinal-non-zero (n stream magnitude)
  (multiple-value-bind (thousands rest) (floor n 1000)
    (unless (zerop thousands)
      (print-cardinal-non-zero thousands stream (1+ magnitude)))
    (unless (or (zerop thousands) (zerop rest))
      (princ " " stream))
    (unless (zerop rest)
      (print-cardinal-hundreds rest stream)
      (unless (zerop magnitude)
        (princ " " stream)
        (princ (aref *groups-of-three* magnitude) stream)))))

;;; print a cardinal number n such that - 10^65 < n < 10^65
(defun print-cardinal-number (n stream)
  (cond ((minusp n)
         (princ "negative " stream)
         (print-cardinal-non-zero (- n) stream 0))
        ((zerop n)
         (princ "zero" stream))
        (t
         (print-cardinal-non-zero n stream 0))))

(defparameter *ordinal-ones*
  #(nil "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth"))

(defparameter *ordinal-teens*
  #("tenth" "eleventh" "twelvth" "thirteenth" "fourteenth"
    "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))

(defparameter *ordinal-tens*
  #(nil nil "twentieth" "thirtieth" "fourtieth"
    "fiftieth" "sixtieth" "seventieth" "eightieth" "ninetieth"))

;;; print an ordinal number between 1 and 99
(defun print-ordinal-tenths (n stream)
  (cond ((< n 10)
         (princ (aref *ordinal-ones* n) stream))
        ((< n 20)
         (princ (aref *ordinal-teens* (- n 10)) stream))
        (t
         (multiple-value-bind (tens ones) (floor n 10)
           (if (zerop ones)
               (princ (aref *ordinal-tens* tens) stream)
               (progn (princ (aref *cardinal-tens* tens) stream)
                      (princ "-" stream)
                      (princ (aref *ordinal-ones* ones) stream)))))))

;;; print an ordinal number n such that 0 < n < 1000
(defun print-ordinal-hundreds (n stream)
  (cond ((< n 100)
         (print-ordinal-tenths n stream))
        (t
         (multiple-value-bind (hundreds rest) (floor n 100)
           (princ (aref *cardinal-ones* hundreds) stream)
           (princ " hundred" stream)
           (if (zerop rest)
               (princ "th" stream)
               (progn (princ " " stream)
                      (print-ordinal-tenths rest stream)))))))

;;; print an ordinal number n such that 0 < n < 10^65
(defun print-ordinal-non-zero (n stream)
  (multiple-value-bind (hundreds rest) (floor n 100)
    (cond ((zerop rest)
           ;; hudreds is nonzero
           (print-cardinal-non-zero n stream 0)
           (princ "th" stream))
          ((zerop hundreds)
           (print-ordinal-hundreds rest stream))
          (t
           ;; they are both nonzero
           (print-cardinal-non-zero (* 100 hundreds) stream 0)
           (princ " " stream)
           (print-ordinal-tenths rest stream)))))

;;; print an ordninal number n such that - 10^65 < n < 10^65
(defun print-ordinal-number (n stream)
  (cond ((minusp n)
         (princ "negative " stream)
         (print-ordinal-non-zero (- n) stream))
        ((zerop n)
         (princ "zeroth" stream))
        (t
         (print-ordinal-non-zero n stream))))

(define-format-directive-interpreter r-directive
  (cond ((not (null radix))
         (print-radix-arg radix colonp at-signp mincol padchar commachar comma-interval))
        ((and colonp at-signp)
         (print-as-old-roman (consume-next-argument '(integer 1))
                             *destination*))
        (at-signp
         (print-as-roman (consume-next-argument '(integer 1))
                         *destination*))
        (colonp
         (print-ordinal-number (consume-next-argument
                                `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                               *destination*))
        (t
         (print-cardinal-number (consume-next-argument
                                 `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                *destination*))))

(define-format-directive-compiler r-directive
    (cond ((not (null radix))
           `(print-radix-arg radix ,colonp ,at-signp mincol padchar commachar comma-interval))
          ((and colonp at-signp)
           `(print-as-old-roman (consume-next-argument '(integer 1))
                                *destination*))
          (at-signp
           `(print-as-roman (consume-next-argument '(integer 1))
                            *destination*))
          (colonp
           `(print-ordinal-number (consume-next-argument
                                   `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                  *destination*))
          (t
           `(print-cardinal-number (consume-next-argument
                                    `(integer ,(1+ (- (expt 10 65))) ,(1- (expt 10 65))))
                                   *destination*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.2 ~d Decimal.

(define-directive #\d d-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter d-directive
  (print-radix-arg 10 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler d-directive
    `(print-radix-arg 10 ,colonp ,at-signp mincol padchar commachar comma-interval))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.3 ~b Binary.

(define-directive #\b b-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter b-directive
  (print-radix-arg 2 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler b-directive
    `(print-radix-arg 2 ,colonp ,at-signp mincol padchar commachar comma-interval))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.4 ~o Octal.

(define-directive #\o o-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter o-directive
  (print-radix-arg 8 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler o-directive
    `(print-radix-arg 8 ,colonp ,at-signp mincol padchar commachar comma-interval))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.2.5 ~x Hexadecimal.

(define-directive #\x x-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)
     (commachar :type character :default-value #\,)
     (comma-interval :type '(integer 1) :default-value 3)))

(define-format-directive-interpreter x-directive
  (print-radix-arg 16 colonp at-signp mincol padchar commachar comma-interval))

(define-format-directive-compiler x-directive
    `(print-radix-arg 16 ,colonp ,at-signp mincol padchar commachar comma-interval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3 Floating-point printers

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.1 ~f Fixed-format floating point.
(define-directive #\f f-directive (named-parameters-directive)
  ((w  :type integer)
   (d :type integer)
   (k :type (integer 0) :default-value 0)
   (overflowchar :type character)
   (padchar :type character :default-value #\Space)))

(defun print-float-arg (colonp at-signp w d k overflowchar padchar)
  (let ((argument (consume-next-argument t)))
    (if (numberp argument)
        (format *destination* "decimal ~D" argument)
        (if (floatp argument)
            (format *destination* "float!")
            (format *destination* "aesthetic ~A" argument)))))

(define-format-directive-interpreter f-directive
  (print-float-arg colonp at-signp w d k overflowchar padchar))

(define-format-directive-compiler f-directive
  `(print-float-arg ,colonp ,at-signp w d k overflowchar padchar))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.2 ~e Exponential floating point.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.3 ~g General floating point.

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.3.4 ~$ Monetary floating point.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4 Printer operations

(defun print-a-or-s (raw-output at-signp mincol colinc minpad padchar)
  (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
    (if at-signp
        (progn (loop repeat pad-length do (write-char padchar *destination*))
               (write-string raw-output *destination*))
        (progn (write-string raw-output *destination*)
               (loop repeat pad-length do (write-char padchar *destination*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.1 ~a Aesthetic.

(define-directive #\a a-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)))

(define-format-directive-interpreter a-directive
  (let* ((*print-escape* nil)
         (*print-readably* nil)
         (raw-output
          (let ((arg (consume-next-argument t)))
            (if (and colonp (null arg))
                "()"
                (princ-to-string arg)))))
    (print-a-or-s raw-output at-signp mincol colinc minpad padchar)))

(define-format-directive-compiler a-directive
  `(let* ((*print-escape* nil)
          (*print-readably* nil)
          (raw-output
           (let ((arg (consume-next-argument t)))
             ,(if colonp
                  `(if (null arg)
                       "()"
                       (princ-to-string arg))
                  `(princ-to-string arg)))))
     (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
       ,(if at-signp
            `(progn (loop repeat pad-length do (write-char padchar *destination*))
                    (write-string raw-output *destination*))
            `(progn (write-string raw-output *destination*)
                    (loop repeat pad-length do (write-char padchar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.2 ~s Standard.

(define-directive #\s s-directive (named-parameters-directive)
    ((mincol :type (integer 0) :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)))

(define-format-directive-interpreter s-directive
  (let* ((*print-escape* t)
         (raw-output
          (let ((arg (consume-next-argument t)))
            (if (and colonp (null arg))
                "()"
                (prin1-to-string arg)))))
    (print-a-or-s raw-output at-signp mincol colinc minpad padchar)))

(define-format-directive-compiler s-directive
  `(let* ((*print-escape* t)
          (raw-output
           (let ((arg (consume-next-argument t)))
             ,(if colonp
                  `(if (null arg)
                       "()"
                       (princ-to-string arg))
                  `(prin1-to-string arg)))))
     (let ((pad-length (max minpad (* colinc (ceiling (- mincol (length raw-output)) colinc)))))
       ,(if at-signp
            `(progn (loop repeat pad-length do (write-char padchar *destination*))
                    (write-string raw-output *destination*))
            `(progn (write-string raw-output *destination*)
                    (loop repeat pad-length do (write-char padchar *destination*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.4.3 ~w Write.

(define-directive #\w w-directive (named-parameters-directive) ())

(define-format-directive-interpreter w-directive
  (cond ((and colonp at-signp )
         (let ((*print-pretty* t)
               (*print-level* nil)
               (*print-length* nil))
           (write (consume-next-argument t) :stream *destination*)))
        (colonp
         (let ((*print-pretty* t))
           (write (consume-next-argument t) :stream *destination*)))
        (at-signp
         (let ((*print-level* nil)
               (*print-length* nil))
           (write (consume-next-argument t) :stream *destination*)))
        (t
         (write (consume-next-argument t) :stream *destination*))))

(define-format-directive-compiler w-directive
  (cond ((and colonp at-signp )
         `(let ((*print-pretty* t)
                (*print-level* nil)
                (*print-length* nil))
            (write (consume-next-argument t) :stream *destination*)))
        (colonp
         `(let ((*print-pretty* t))
            (write (consume-next-argument t) :stream *destination*)))
        (at-signp
         `(let ((*print-level* nil)
                (*print-length* nil))
            (write (consume-next-argument t) :stream *destination*)))
        (t
         `(write (consume-next-argument t) :stream *destination*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5 Pretty printer operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.1 ~_ Conditional newline

(define-directive #\_ underscore-directive (named-parameters-directive) ())

(define-format-directive-interpreter underscore-directive
  (pprint-newline (cond ((and colonp at-signp) :mandatory)
                        (colonp :fill)
                        (at-signp :miser)
                        (t :linear))
                  *destination*))

(define-format-directive-compiler underscore-directive
  `(pprint-newline ,(cond ((and colonp at-signp) :mandatory)
                          (colonp :fill)
                          (at-signp :miser)
                          (t :linear))
                   *destination*))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.2 ~< Logical block

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\< logical-block-directive (named-parameters-directive structured-directive-mixin) ())



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.3 ~i Indent

(define-directive #\i i-directive (named-parameters-directive)
    ((how-many :type (integer 0) :default-value 0)))

(define-format-directive-interpreter i-directive
  (pprint-indent (if colonp :current :block) how-many *destination*))

(define-format-directive-compiler i-directive
  `(pprint-indent ,(if colonp :current :block) how-many *destination*))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.5.4 ~/ Call function

;;; This directive is particular in two different ways.  First, as
;;; with the "ignored newline" directive, there are characters
;;; belonging to the directive beyond the directive character itself,
;;; which means the standard mechanism of parsing it cannot be used.
;;; Second, this directive takes an arbitrary number of parameters.
;;;
;;; So, define-format-directive-interpreter cannot be used, since its
;;; main purpose is to give lexical access to each parameter by name.

(define-directive #\/ call-function-directive (directive)
    ()
  (%function-name :accessor function-name))

(defmethod check-directive-syntax progn ((directive call-function-directive))
  ;; Check that there is at most one package marker in the function name.
  ;; Also, compute a symbol from the name.
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp))
    directive
    ;; To figure out where the name of the function starts and ends,
    ;; we cannot search from the beginning of the directive, because
    ;; the given parameters can contain arbitrary characters following
    ;; a single quote (indicating a character parameter).  However,
    ;; we know that the last character of the directive is the trailing
    ;; #\/ of the function name, and the one preceding that is the
    ;; #\/ preceding the function name.
    (let ((pos1 (1+ (position #\/ control-string :end (1- end) :from-end t)))
          (pos2 (1- end)))
      (let ((position-of-first-package-marker
             (position #\: control-string :start pos1 :end pos2))
            (position-of-last-package-marker
             (position #\: control-string :start pos1 :end pos2 :from-end t)))
        (when (and (not (null position-of-first-package-marker))
                   (> position-of-last-package-marker
                      (1+ position-of-first-package-marker)))
          (error 'too-many-package-markers
                 :directive directive))
        ;; The HyperSpec says that all the characters of the function
        ;; name are treated as if they were upper-case.  It would
        ;; probably be smarter to follow the readtable-case of the
        ;; current readtable, but that's not what the spec says.
        (let ((package-name
               (if (null position-of-first-package-marker)
                   "COMMON-LISP-USER"
                   (string-upcase
                    (subseq control-string
                            pos1
                            position-of-first-package-marker))))
              (symbol-name
               (string-upcase
                (subseq control-string
                        (if (null position-of-first-package-marker)
                            pos1
                            (1+ position-of-last-package-marker))
                        pos2))))
          (let ((package (find-package package-name)))
            (when (null package)
              (error 'no-such-package
                     :directive directive))
            (multiple-value-bind (symbol status)
                (find-symbol symbol-name package)
              (when (or (null status)
                        (eq status :inherited))
                (error 'no-such-symbol
                       :directive directive))
              (when (and (= position-of-first-package-marker
                            position-of-last-package-marker)
                         (eq status :internal))
                (error 'symbol-not-external
                       :directive directive))
              (setf (function-name directive)
                    symbol))))))))

(defmethod interpret-format-directive ((directive call-function-directive))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
    directive
    (let ((param-args
           (loop for parameter in given-parameters
                 collect (cond ((eq parameter '|#|)
                                (- (length *arguments*)
                                   *next-argument-pointer*))
                               ((eq parameter 'V)
                                (consume-next-argument t))
                               (t parameter)))))
      (apply function-name
             *destination*
             (consume-next-argument t)
             colonp
             at-signp
             param-args))))

;;; This is not quite right.  We should probably look up the
;;; function name at runtime as opposed to compile time.
(defmethod compile-format-directive ((directive call-function-directive))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end)
                   (colonp colonp)
                   (at-signp at-signp)
                   (given-parameters given-parameters)
                   (function-name function-name))
    directive
    (let ((param-args
           (loop for parameter in given-parameters
                 collect (cond ((eq parameter '|#|)
                                `(- (length *arguments*)
                                    *next-argument-pointer*))
                               ((eq parameter 'V)
                                `(consume-next-argument t))
                               (t parameter)))))
      `(,function-name
        *destination*
        (consume-next-argument t)
        ,colonp
        ,at-signp
        ,@param-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6 Layout control

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.1 ~TAB Tabulate

(define-directive #\t tabulate-directive (named-parameters-directive)
    ((colnum :type (integer 1) :default-value 1)
     (colinc :type (integer 1) :default-value 1)))

(define-format-directive-interpreter tabulate-directive
  (if (colonp
       (pprint-tab (if at-signp :section-relative :section)
                   colnum colinc *destination*))
      ;; Thanks to Brian Mastenbrook for suggesting this solution.
      (let ((*print-level* nil))
        (pprint-logical-block (*destination* nil)
                              (pprint-tab (if at-signp :line-relative :line)
                                          colnum colinc *destination*)))))

(define-format-directive-compiler tabulate-directive
  (if (colonp
       `(pprint-tab ,(if at-signp :section-relative :section)
                    colnum colinc *destination*))
      ;; Thanks to Brian Mastenbrook for suggesting this solution.
      `(let ((*print-level* nil))
         (pprint-logical-block (*destination* nil)
                               (pprint-tab ,(if at-signp :line-relative :line)
                                           colnum colinc *destination*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.2 ~< Justification

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\< justification-directive (named-parameters-directive structured-directive-mixin)
    ((mincol :type (integer 0) :default-value 0)
     (colinc :type (integer 0) :default-value 1)
     (minpad :type (integer 0) :default-value 0)
     (padchar :type character :default-value #\Space)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.6.3 ~> End of justification or of logical block

(define-directive #\> greater-than-directive (named-parameters-directive) ())

(define-format-directive-interpreter greater-than-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7 Control-flow operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.1 ~* Go to

(define-directive #\* go-to-directive (named-parameters-directive at-most-one-modifier-mixin)
    ((param :type (integer 0))))

(define-format-directive-interpreter go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         (let ((new-arg-pointer (- *next-argument-pointer* (or param 1))))
           (unless (>= new-arg-pointer 0)
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-pointer
                    :max-arguments (length *arguments*)))
           (setf *next-argument-pointer* new-arg-pointer)))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         (let ((new-arg-pointer (or param 0)))
           (unless (<= 0 new-arg-pointer (length *arguments*))
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-pointer
                    :max-arguments (length *arguments*)))
           (setf *next-argument-pointer* new-arg-pointer)))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         (let ((new-arg-pointer (+ *next-argument-pointer* (or param 1))))
           (unless (<= new-arg-pointer (length *arguments*))
             (error 'go-to-out-of-bounds
                    :what-argument new-arg-pointer
                    :max-arguments (length *arguments*)))
           (setf *next-argument-pointer* new-arg-pointer)))))

(define-format-directive-compiler go-to-directive
  (cond (colonp
         ;; Back up in the list of arguments.
         ;; The default value for the parameter is 1.
         `(let ((new-arg-pointer (- *next-argument-pointer* (or param 1))))
            (unless (>= new-arg-pointer 0)
              (error 'go-to-out-of-bounds
                     :what-argument new-arg-pointer
                     :max-arguments (length *arguments*)))
            (setf *next-argument-pointer* new-arg-pointer)))
        (at-signp
         ;; Go to an absolute argument number.
         ;; The default value for the parameter is 0.
         `(let ((new-arg-pointer (or param 0)))
            (unless (<= 0 new-arg-pointer (length *arguments*))
              (error 'go-to-out-of-bounds
                     :what-argument new-arg-pointer
                     :max-arguments (length *arguments*)))
            (setf *next-argument-pointer* new-arg-pointer)))
        (t
         ;; Skip the next arguments.
         ;; The default value for the parameter is 1.
         `(let ((new-arg-pointer (+ *next-argument-pointer* (or param 1))))
            (unless (<= new-arg-pointer (length *arguments*))
              (error 'go-to-out-of-bounds
                     :what-argument new-arg-pointer
                     :max-arguments (length *arguments*)))
            (setf *next-argument-pointer* new-arg-pointer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.1 ~; Clause separator

;;; This one is out of order to allow a clean compilation

(define-directive #\; semicolon-directive (named-parameters-directive only-colon-mixin) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.2 ~[ Conditional expression

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\[ conditional-directive
    (named-parameters-directive structured-directive-mixin at-most-one-modifier-mixin)
    ((param :type integer))
  (%clauses :accessor clauses)
  (%last-clause-is-default-p :initform nil :accessor last-clause-is-default-p))

(defmethod check-directive-syntax progn ((directive conditional-directive))
  ;; Check that, if a parameter is given, then there are
  ;; no modifiers.
  (when (and (not (null (given-parameters directive)))
             (or (colonp directive) (at-signp directive)))
    (error 'modifier-and-parameter
           :directive directive))
  ;; eliminate the end-of-conditional directive from the items
  (let ((items (subseq (items directive) 0 (1- (length (items directive))))))
    ;; Check that there is at least one item in items
    (when (zerop (length items))
      (error 'at-least-one-item-required
             :directive directive))
    ;; Check that, if a colon modifier was given, then
    ;; there should be a single clause separator (two clauses).
    (when (and (colonp directive)
               (/= (count-if (lambda (item) (typep item 'semicolon-directive))
                             items)
                   1))
      (error 'colon-modifier-requires-two-clauses))
    ;; Check that, if an at-sign modifier was given, then
    ;; there should be a no clause separators (a single clause).
    (when (and (at-signp directive)
               (/= (count-if (lambda (item) (typep item 'semicolon-directive))
                             items)
                   0))
      (error 'at-sign-modifier-requires-one-clause))
    (flet ((clause-separator-with-colon-modifier (item)
             (and (typep item 'semicolon-directive)
                  (colonp item))))
      (let ((position-of-clause-with-colon-modifier
             (position-if #'clause-separator-with-colon-modifier
                          items
                          :from-end t)))
        ;; Check that, if a modifier is given, then there should
        ;; be no clause separator with colon modifier.
        (when (and (or (colonp directive) (at-signp directive))
                   (not (null position-of-clause-with-colon-modifier)))
          (error 'clause-separator-with-colon-modifier-not-allowed
                 :directive directive))
        (when (or
               ;; Check whether there is more than one clause separator
               ;; with a `:' modifier.
               (> (count-if #'clause-separator-with-colon-modifier items)
                  1)
               ;; Check whether the clause separator with a `:' modifier
               ;; (if any) is not the last one.
               (and (find-if #'clause-separator-with-colon-modifier items)
                    (/= position-of-clause-with-colon-modifier
                        (position-if (lambda (item) (typep item 'semicolon-directive))
                                     items
                                     :from-end t))))
          (error 'illegal-clause-separators
                 :directive directive))
        (unless (null position-of-clause-with-colon-modifier)
          (setf (last-clause-is-default-p directive) t))
        ;; Divide the items into clauses.
        ;; Each clause is just a vector of items.
        (loop with start = 0
              with end = (length items)
              with clauses = '()
              until (= start end)
              do (let ((position-of-clause-separator
                        (position-if (lambda (item) (typep item 'semicolon-directive))
                                     items
                                     :start start)))
                   (if (null position-of-clause-separator)
                       (progn (push (subseq items start) clauses)
                              (setf start end))
                       (progn (push (subseq items
                                            start
                                            position-of-clause-separator)
                                    clauses)
                              (setf start (1+ position-of-clause-separator)))))
              finally (setf (clauses directive)
                            (coerce (nreverse clauses) 'vector)))))))

(define-format-directive-interpreter conditional-directive
  (cond (at-signp
         (when (>= *next-argument-pointer* (length *arguments*))
           (error 'no-more-arguments))
         (if (aref *arguments* *next-argument-pointer*)
             ;; Then do not consume the argument and
             ;; process the clause.
             (interpret-items (aref (clauses directive) 0))
             ;; Else, consume the argument and
             ;; do not process the clause
             (incf *next-argument-pointer*)))
        (colonp
         (interpret-items (aref (clauses directive)
                                (if (consume-next-argument t)
                                    ;; Then interpret the first clause
                                    ;; (yes that's what the CLHS says)
                                    0
                                    ;; Else interpret the second clause
                                    1))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         (let ((val (or param (consume-next-argument 'integer))))
           (if (or (minusp val)
                   (>= val (length (clauses directive))))
               ;; Then the argument is out of range
               (when (last-clause-is-default-p directive)
                 ;; Then execute the default-clause
                 (interpret-items (aref (clauses directive)
                                        (1- (length (clauses directive))))))
               ;; Else, execute the corresponding clause
               (interpret-items (aref (clauses directive) val)))))))

(define-format-directive-compiler conditional-directive
  (cond (at-signp
         `(progn (when (>= *next-argument-pointer* (length *arguments*))
                   (error 'no-more-arguments))
                 (if (aref *arguments* *next-argument-pointer*)
                     ;; Then do not consume the argument and
                     ;; process the clause.
                     (progn ,@(compile-items (aref (clauses directive) 0))
                     ;; Else, consume the argument and
                     ;; do not process the clause
                     (incf *next-argument-pointer*)))))
        (colonp
         `(if (consume-next-argument t)
              ;; Compile the first clause
              ;; (yes that's what the CLHS says)
              (progn ,@(compile-items (aref (clauses directive) 0)))
              ;; Compile the second clause
              (progn ,@(compile-items (aref (clauses directive) 1)))))
        (t
         ;; If a parameter was given, use it,
         ;; else use the next argument.
         `(let ((val (or param (consume-next-argument 'integer))))
            (if (or (minusp val)
                    (>= val ,(length (clauses directive))))
                ;; Then the argument is out of range
                ,(when (last-clause-is-default-p directive)
                       ;; Then execute the default-clause
                       `(progn ,@(compile-items
                                  (aref (clauses directive)
                                        (1- (length (clauses directive)))))))
                ;; Else, execute the corresponding clause
                (case val
                  ,@(loop for i from 0 below (length (clauses directive))
                          for clause across (clauses directive)
                          collect `(,i ,@(compile-items
                                          (aref (clauses directive) i))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.3 ~] End of conditional expression

(define-directive #\] right-bracket-directive (named-parameters-directive no-modifiers-mixin) ())

(define-format-directive-interpreter right-bracket-directive
    ;; do nothing
    nil)

(define-format-directive-compiler right-bracket-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.4 ~{ Iteration

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\{ iteration-directive (named-parameters-directive structured-directive-mixin)
    ((iteration-limit :type (integer 0))))

(define-format-directive-interpreter iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let ((items (subseq (items directive) 0 (1- (length (items directive))))))
    (cond ((and colonp at-signp)
           ;; The remaining arguments should be lists.  Each argument
           ;; is used in a different iteration.
           (flet ((one-iteration ()
                    (let ((arg (aref *arguments* *next-argument-pointer*)))
                      (unless (listp arg)
                        (error 'argument-type-error
                               :expected-type 'list
                               :datum arg))
                      (let ((*arguments* (coerce arg 'vector))
                            (*next-argument-pointer* 0))
                        (interpret-items items))
                      (incf *next-argument-pointer*))))
             (if (null iteration-limit)
                 (loop until (= *next-argument-pointer* (length *arguments*))
                       do (one-iteration))
                 (loop until (= *next-argument-pointer* (length *arguments*))
                       repeat iteration-limit
                       do (one-iteration)))))
          (colonp
           ;; We use one argument, and that should be a list of sublists.
           ;; Each sublist is used as arguments for one iteration.
           (let ((arg (consume-next-argument 'list)))
             (flet ((one-iteration (args)
                      (unless (listp args)
                        (error 'argument-type-error
                               :expected-type 'list
                               :datum args))
                      (let ((*arguments* (coerce args 'vector))
                            (*next-argument-pointer* 0))
                        (interpret-items items))))
               (if (null iteration-limit)
                   (loop for args in arg ; a bit unusual naming perhaps
                         do (one-iteration args))
                   (loop for args in arg ; a bit unusual naming perhaps
                         repeat iteration-limit
                         do (one-iteration args))))))
          (at-signp
           (if (null iteration-limit)
               (loop until (= *next-argument-pointer* (length *arguments*))
                     do (interpret-items items))
               (loop until (= *next-argument-pointer* (length *arguments*))
                     repeat iteration-limit
                     do (interpret-items items))))
          (t
           ;; no modifiers
           ;; We use one argument, and that should be a list.
           ;; The elements of that list are used by the iteration.
           (let ((arg (consume-next-argument 'list)))
             (let ((*arguments* (coerce arg 'vector))
                   (*next-argument-pointer* 0))
               (if (null iteration-limit)
                   (loop until (= *next-argument-pointer* (length *arguments*))
                         do (interpret-items items))
                   (loop until (= *next-argument-pointer* (length *arguments*))
                         repeat iteration-limit
                         do (interpret-items items)))))))))

(define-format-directive-compiler iteration-directive
  ;; eliminate the end-of-iteration directive from the
  ;; list of items
  (let ((items (subseq (items directive) 0 (1- (length (items directive))))))
    (cond ((and colonp at-signp)
           ;; The remaining arguments should be lists.  Each argument
           ;; is used in a different iteration.
           `(flet ((one-iteration ()
                     (let ((arg (aref *arguments* *next-argument-pointer*)))
                       (unless (listp arg)
                         (error 'argument-type-error
                                :expected-type 'list
                                :datum arg))
                       (let ((*arguments* (coerce arg 'vector))
                             (*next-argument-pointer* 0))
                         ,@(compile-items items))
                       (incf *next-argument-pointer*))))
              ,(if (null iteration-limit)
                   `(loop until (= *next-argument-pointer* (length *arguments*))
                          do (one-iteration))
                   `(loop until (= *next-argument-pointer* (length *arguments*))
                          repeat ,iteration-limit
                          do (one-iteration)))))
          (colonp
           ;; We use one argument, and that should be a list of sublists.
           ;; Each sublist is used as arguments for one iteration.
           `(let ((arg (consume-next-argument 'list)))
              (flet ((one-iteration (args)
                       (unless (listp args)
                         (error 'argument-type-error
                                :expected-type 'list
                                :datum args))
                       (let ((*arguments* (coerce args 'vector))
                             (*next-argument-pointer* 0))
                         ,@(compile-items items))))
                ,(if (null iteration-limit)
                     `(loop for args in arg ; a bit unusual naming perhaps
                            do (one-iteration args))
                     `(loop for args in arg ; a bit unusual naming perhaps
                            repeat ,iteration-limit
                            do (one-iteration args))))))
          (at-signp
           (if (null iteration-limit)
               `(loop until (= *next-argument-pointer* (length *arguments*))
                      do (progn ,@(compile-items items)))
               `(loop until (= *next-argument-pointer* (length *arguments*))
                      repeat ,iteration-limit
                      do (progn ,@(compile-items items)))))
          (t
           ;; no modifiers
           ;; We use one argument, and that should be a list.
           ;; The elements of that list are used by the iteration.
           `(let ((arg (consume-next-argument 'list)))
              (let ((*arguments* (coerce arg 'vector))
                    (*next-argument-pointer* 0))
                ,(if (null iteration-limit)
                     `(loop until (= *next-argument-pointer* (length *arguments*))
                            do (progn ,@(compile-items items)))
                     `(loop until (= *next-argument-pointer* (length *arguments*))
                            repeat iteration-limit
                            do (progn ,@(compile-items items))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.5 ~} End of iteration

(define-directive #\} right-brace-directive (named-parameters-directive only-colon-mixin) ())

(define-format-directive-interpreter right-brace-directive
    ;; do nothing
    nil)

(define-format-directive-compiler right-brace-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.7.6 ~? Recursive processing

(define-directive #\? recursive-processing-directive (named-parameters-directive only-at-sign-mixin) ())

(define-format-directive-interpreter recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      (format-with-runtime-arguments *destination*
                                     (consume-next-argument 'string))
      ;;
      (apply #'format
             *destination*
             (consume-next-argument 'string)
             (consume-next-argument 'list))))

(define-format-directive-compiler recursive-processing-directive
  (if at-signp
      ;; reuse the arguments from the parent control-string
      `(format-with-runtime-arguments *destination*
                                      (consume-next-argument 'string))
      ;;
      `(apply #'format
              *destination*
              (consume-next-argument 'string)
              (consume-next-argument 'list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8 Miscellaneous operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.1 ~( Case conversion

;;; The character is not used to determine the class for
;;; this directive.
(define-directive #\( case-conversion-directive (named-parameters-directive structured-directive-mixin) ())

(define-format-directive-interpreter case-conversion-directive
  (let ((output (with-output-to-string (stream)
                  (let ((*destination* stream))
                    (interpret-items (items directive))))))
    (cond ((and colonp at-signp)
           (nstring-upcase output))
          (colonp
           (nstring-capitalize output))
          (at-signp
           (let ((pos (position-if #'alphanumericp output)))
             (when (not (null pos))
               (setf (char output pos)
                     (char-upcase (char output pos))))))
          (t
           (nstring-downcase output)))
    (princ output *destination*)))

(define-format-directive-compiler case-conversion-directive
  `(let ((output (with-output-to-string (stream)
                   (let ((*destination* stream))
                     ,@(compile-items (items directive))))))
     ,(cond ((and colonp at-signp)
             `(nstring-upcase output))
            (colonp
             `(nstring-capitalize output))
            (at-signp
             `(let ((pos (position-if #'alphanumericp output)))
                (when (not (null pos))
                  (setf (char output pos)
                        (char-upcase (char output pos))))))
            (t
             `(nstring-downcase output)))
     (princ output *destination*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.2 ~) End of case conversion

(define-directive #\) right-paren-directive (named-parameters-directive no-modifiers-mixin) ())

(define-format-directive-interpreter right-paren-directive
    ;; do nothing
    nil)

(define-format-directive-compiler right-paren-directive
    ;; do nothing
    nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.8.3 ~p Plural

(define-directive #\p plural-directive (named-parameters-directive) ())

(define-format-directive-interpreter plural-directive
  (when colonp
    (when (zerop *next-argument-pointer*)
      (error 'go-to-out-of-bounds
             :what-argument -1
             :max-arguments (length *arguments*)))
    (decf *next-argument-pointer*))
  (if at-signp
      (princ (if (eql (consume-next-argument t) 1)
                 "y"
                 "ies")
             *destination*)
      (when (eql (consume-next-argument t) 1)
        (write-char #\s *destination*))))

(define-format-directive-compiler plural-directive
  (when colonp
    `(progn (when (zerop *next-argument-pointer*)
              (error 'go-to-out-of-bounds
                     :what-argument -1
                     :max-arguments (length *arguments*)))
            (decf *next-argument-pointer*)))
  (if at-signp
      `(princ (if (eql (consume-next-argument t) 1)
                  "y"
                  "ies")
              *destination*)
      `(when (eql (consume-next-argument t) 1)
         (write-char #\s *destination*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9 Miscellaneous pseudo-operations

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.1 ~; Clause separator

;;; see above

(define-directive #\; semicolon-directive (named-parameters-directive only-colon-mixin) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.2 ~^ Escape upward

(define-directive #\^ circumflex-directive (named-parameters-directive)
    ((p1 :type integer)
     (p2 :type integer)
     (p3 :type integer)))

(defmethod check-directive-syntax progn
  ((directive circumflex-directive))
  (let ((parameters (given-parameters directive)))
    (when (and (second parameters) (not (first parameters)))
      (error 'parameter-omitted
             :parameter1 1
             :parameter2 2))
    (when (and (third parameters) (not (second parameters)))
      (error 'parameter-omitted
             :parameter2 2
             :parameter3 3))))

(define-format-directive-interpreter circumflex-directive
  (let ((parameters (given-parameters directive)))
    (cond ((not (first parameters))
           (throw *catch-tag* nil))
          ((not (second parameters))
           (when (zerop p1)
             (throw *catch-tag* nil)))
          ((not (third parameters))
           (when (= p1 p2)
             (throw *catch-tag* nil)))
          (t
           (when (<= p1 p2 p3)
             (throw *catch-tag* nil))))))

(define-format-directive-compiler circumflex-directive
  (let ((parameters (given-parameters directive)))
    (cond ((not (first parameters))
           `(throw *catch-tag* nil))
          ((not (second parameters))
           `(when (zerop p1)
              (throw *catch-tag* nil)))
          ((not (third parameters))
           `(when (= p1 p2)
              (throw *catch-tag* nil)))
          (t
           `(when (<= p1 p2 p3)
              (throw *catch-tag* nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22.3.9.3 ~Newline Igored newline

(define-directive #\Newline newline-directive (named-parameters-directive at-most-one-modifier-mixin) ())

(define-format-directive-interpreter newline-directive
  (cond (colonp
         ;; remove the newline but print the following whitespace
         (let ((start (1+ (position #\Newline control-string :start start))))
           (loop for index from start below end
                 do (write-char (char control-string index) *destination*))))
        (at-signp
         ;; print the newline, but remove the following whitespace
         (write-char #\Newline *destination*))
        (t
         ;; ignore both the newline and the following whitespace
         nil)))

(define-format-directive-compiler newline-directive
  (cond (colonp
         ;; remove the newline but print the following whitespace
         `(let ((start (1+ (position #\Newline control-string :start start))))
            (write-string ,(subseq control-string start end) *destination*)))
        (at-signp
         ;; print the newline, but remove the following whitespace
         `(write-char #\Newline *destination*))
        (t
         ;; ignore both the newline and the following whitespace
         nil)))

;;; The reason we define this function is that the ~? directive
;;; (recursive processing), when a @ modifier is used, reuses
;;; the arguments of the parent control string, so we need
;;; to call a version of format that doesn't initialize the
;;; *arguments* runtime environment variable.
(defun format-with-runtime-arguments (destination control-string)
  (flet ((format-aux (stream items)
           ;; interpret the control string in a new environment
           (let ((*destination* stream)
                 ;; We are at the beginning of the argument vector.
                 (*next-argument-pointer* 0)
                 ;; Any unique object will do.
                 (*catch-tag* (list nil)))
             (catch *catch-tag*
               (interpret-items items)))))
    (let ((items (structure-items (split-control-string control-string) nil)))
      (cond ((or (streamp destination)
                 (and (stringp destination)
                      (array-has-fill-pointer-p destination)))
             (format-aux destination items))
            ((null destination)
             (with-output-to-string (stream)
                                    (format-aux stream items)))
            ((eq destination t)
             (format-aux *standard-output* items))
            (t
             (error 'invalid-destination
                    :destination destination))))))

(defun format (destination control-string &rest args)
  (let (;; initialize part of the runtime environment here
        (*arguments* (coerce args 'vector)))
    (format-with-runtime-arguments destination control-string)))
