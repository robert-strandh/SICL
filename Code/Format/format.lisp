;;;; Copyright (c) 2008.
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or
;;;; without modification, are permitted provided that the following
;;;; conditions are met:
;;;;
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above
;;;;    copyright notice, this list of conditions and the following
;;;;    disclaimer in the documentation and/or other materials
;;;;    provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS
;;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
;;;; ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;;;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;;;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.


;;; The name of this project is SICL, which doesn't stand for anything
;;; in particular.  Pronounce it like "sickle".
;;;
;;; The purpose of this code is to provide a totally portable
;;; implementation of some high-level functionality of the Common Lisp
;;; language, so that implementors of Common Lisp systems can
;;; integrate it as it is into their systems, without having to
;;; implement and maintain a specific version of it.
;;;
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

;;; How we represent a directive.  It may seem wasteful to allocate
;;; a class instance for each directive, but most format directives
;;; are handled at compile time anyway.
(defclass directive ()
  (;; the entire control string in which this directive was found
   (%control-string :initarg :control-string :reader control-string)
   ;; the position in the control string of the ~ character.
   (%start :initarg :start :reader start)
   ;; the first position beyond the directive character
   (%end :initarg :end :reader end)
   ;; The directive character used.
   (%directive-character :initarg :directive-character :reader directive-character)
   ;; a list of parameters, each one is either an integer or a character
   (%given-parameters :initarg :given-parameters :reader given-parameters)
   ;; true if and only if the `:' modifier was given
   (%colonp :initarg :colonp :reader colonp)
   ;; true if and only if the `@' modifier was given
   (%at-signp :initarg :at-signp :reader at-signp)))

;;; The base class of all directives that take a maximum number of
;;; named parameters.  Those are all the directives except the
;;; call-function directive.
(defclass named-parameters-directive (directive) ())

;;; Split a control string into its components.  Each component is
;;; either a string to be printed as it is, or a directive.  The list
;;; of components will never contain two consecutive strings.
(defun split-control-string (control-string)
  (loop with start = 0
        with end = (length control-string)
        while (< start end)
        collect (let ((tilde-position (position #\~ control-string :start start)))
                  (cond ((null tilde-position)
                         ;; No tilde was found.  The rest of the control string
                         ;; is just a string to be printed.
                         (prog1 (subseq control-string start end)
                           (setf start end)))
                        ((> tilde-position start)
                         ;; A tilde was found, but it is not in the
                         ;; start position.  A prefix of the control
                         ;; string is therefore a string to be
                         ;; printed.
                         (prog1 (subseq control-string start tilde-position)
                           ;; Make sure we find the tilde at the start position
                           ;; in the next iteration.
                           (setf start tilde-position)))
                        (t
                         ;; We found a tilde in the start position, so we have
                         ;; a directive.
                         (multiple-value-bind (directive-character
                                               parameters
                                               colonp
                                               at-signp
                                               end-of-directive-position)
                             (parse-format-directive control-string tilde-position)
                           (prog1 (make-instance 'directive
                                    :control-string control-string
                                    :start tilde-position
                                    :end end-of-directive-position
                                    :directive-character (char-upcase directive-character)
                                    :given-parameters parameters
                                    :colonp colonp
                                    :at-signp at-signp)
                             (setf start end-of-directive-position))))))))


;;; Return the name of a subclass to be used for a particular
;;; directive.  Each particular directive subclass must be accompanied
;;; by an eql-specialized method on this generic function.
(defgeneric directive-subclass-name (directive-character directive))

;;; For the default case, signal an error
(defmethod directive-subclass-name (directive-character directive)
  (error 'unknown-directive-character
         :directive directive))

;;; Given a name of a type of a directive, return a list of parameter
;;; specifiers for that type of directive.  Each type of directive
;;; should supply an eql specialized method for this generic function.
(eval-when (:compile-toplevel :load-toplevel)
  (defgeneric parameter-specs (directive-name)))

;;; A macro that helps us define directives. It takes a directive
;;; character, a directive name (to be used for the class) and a body
;;; in the form of a list of parameter specifications.  Each parameter
;;; specification is a list where the first element is the name of the
;;; parameter, and the remaining elemnts are keyword/value pairs.
;;; Currently, the only keywords allowed are :type and
;;; :default-value.
(defmacro define-directive (character name superclasses parameters &body slots)
  `(progn
     (defmethod directive-subclass-name
         ((char (eql ,(char-upcase character))) directive)
       (declare (ignore directive))
       ',name)

     (eval-when (:compile-toplevel :load-toplevel)
       (defmethod parameter-specs ((directive-name (eql ',name)))
         ',(loop for parameter in parameters
                 collect (if (getf (cdr parameter) :default-value)
                             parameter
                             (cons (car parameter)
                                   (list* :default-value nil (cdr parameter)))))))

     (defclass ,name ,superclasses
       (,@(loop for parameter in parameters
                collect `(,(car parameter)
                           :initform nil
                           :reader
                           ,(car parameter)))
          ,@slots))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Checking syntax, interpreting, and compiling directives.

;;; For certain common types used by FORMAT, return a string
;;; explaining in English what the type means.  For other
;;; types, return a string "an object of type <type>"
(defun type-name (type)
  (cond ((symbolp type)
         (case type
           (integer "an integer")
           (character "a character")
           (list "a list")
           (t (format nil "an object of type ~s" type))))
        ((and (consp type) (eq (car type) 'integer))
         (case (length type)
           (1 "an integer")
           (2 (case (second type)
                (0 "a nonnegative integer")
                (1 "a strictly positive integer")
                (t (format nil "an integer greater than or equal to ~d" (second type)))))
           (3 (format nil "an integer between ~d and ~d" (second type) (third type)))
           (t (format nil "an object of type ~s" type))))
        (t (format nil "an object of type ~s" type))))

;;; Specialize a directive according to a particular directive
;;; character.
(defun specialize-directive (directive)
  (change-class directive (directive-subclass-name (directive-character directive) directive)))

;;; Check the syntax of a directive.
(defgeneric check-directive-syntax (directive)
  (:method-combination progn :most-specific-last))

(defmethod check-directive-syntax progn (directive)
  (with-accessors ((given-parameters given-parameters))
    directive
    (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
      ;; When a parameter was explicitly given, check that
      ;; what was given does not have an incompatible type
      ;; with respect to the default value of the corresponding
      ;; slot, and assign the explicitly given value to
      ;; the slot.
      (let ((parameter-number 1))
        (mapc (lambda (parameter-spec parameter-value)
                (unless (or (eq parameter-value '|#|)
                            (eq parameter-value 'V))
                  (unless
                      (or
                       ;; Either a parameter was not supplied, but it has a
                       ;; default value
                       (and (null parameter-value)
                            (not (null (getf (cdr parameter-spec) :default-value))))
                       ;; Or else it was supplied, and it is of the right type.
                       (typep parameter-value (getf (cdr parameter-spec) :type)))
                    (error 'parameter-type-error
                           :expected-type
                           (getf (cdr parameter-spec) :type)
                           :datum parameter-value)))
                (setf (slot-value directive (car parameter-spec))
                      parameter-value)
                (incf parameter-number))
        parameter-specs
        given-parameters)))))

(defmethod check-directive-syntax progn ((directive named-parameters-directive))
  (with-accessors ((given-parameters given-parameters))
    directive
    (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
      ;; Check that the number of parameters given is no more than
      ;; what this type of directive allows.
      (when (> (length given-parameters) (length parameter-specs))
        (error 'too-many-parameters
               :directive directive
               :at-most-how-many (length parameter-specs)
               :how-many-found (length given-parameters))))))

;;; Runtime environment

;;; During runtime, this variable is bound to a stream to which
;;; all the output goes.
(defvar *destination*)

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

;;; DIRECTIVE is an instance of a subclass of the DIRECTIVE class
;;; describing the directive.
(defgeneric interpret-format-directive (directive))

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

;;; The directive compiler.
(defgeneric compile-format-directive (directive))

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


;;; Mixin class for directives that take no modifiers
(defclass no-modifiers-mixin () ())

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


;;; Mixin class for directives that take only colon modifiers
(defclass only-colon-mixin () ())

;;; Signal an error of an at-sign has been given for such a directive.
(defmethod check-directive-syntax progn ((directive only-colon-mixin))
  (with-accessors ((at-signp at-signp)
                   (control-string control-string)
                   (end end))
    directive
    (when at-signp
      (error 'directive-takes-only-colon
             :directive directive))))


;;; Mixin class for directives that take only at-sign modifiers
(defclass only-at-sign-mixin () ())

;;; Signal an error of a colon has been given for such a directive.
(defmethod check-directive-syntax progn ((directive only-at-sign-mixin))
  (with-accessors ((colonp colonp)
                   (control-string control-string)
                   (end end))
    directive
    (when colonp
      (error 'directive-takes-only-at-sign
             :directive directive))))

;;; Mixin class for directives that take at most one modifier
(defclass at-most-one-modifier-mixin () ())

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


;;; Mixin class for structured directives
(defclass structured-directive-mixin ()
  ((%items :initarg :items :reader items)))

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

(define-format-directive-interpreter f-directive
  (print-float-arg colonp at-signp w d k overflowchar padchar))

(define-format-directive-compiler f-directive
  `(print-float-arg ,colonp ,at-signp w d k overflowchar padchar))

; (defun format-fixed (stream number w d k ovf pad atsign)

(defun print-float-arg (colonp at-signp w d k overflowchar padchar)
  (let ((argument (consume-next-argument t)))
    (if (numberp argument)
        (format *destination* "decimal ~D" argument)
        (if (floatp argument)
            (format *destination* "float!")
            (format *destination* "aesthetic ~A" argument)))))

      #|
      (if (floatp argument)
          (format-fixed-aux stream number w d k ovf pad atsign)
          (if (rationalp number)
              (format-fixed-aux stream
                                (coerce number 'single-float)
                                w d k ovf pad atsign)
              (format-write-field stream
                                  (decimal-string number)
                                  w 1 0 #\space t)))
      |#

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The main entry point

(defun structure-items (items end)
  (loop with result = '()
        with first = (car items)
        do (cond ((null items)
                  (if (null end)
                      (return (values (coerce (nreverse result) 'vector)
                                      '()))
                      (error 'unmatched-directive
                             :directive first
                             :control-string (control-string first)
                             :tilde-position (start first))))
                 ((stringp (car items))
                  (push (pop items) result))
                 ((find (directive-character (car items))
                        ">)}]")
                  (if (eql (directive-character (car items)) end)
                      (progn (push (pop items) result)
                             (return (values (coerce (nreverse result) 'vector)
                                             items)))
                      (error 'nesting-violation
                             :directive (car items))))
                 ((find (directive-character (car items))
                        "<({[")
                  (let ((item (pop items)))
                    (multiple-value-bind (nested-items rest)
                        (structure-items items
                                         (ecase (directive-character item)
                                           (#\< #\>) (#\( #\)) (#\{ #\}) (#\[ #\])))
                      (setf items rest)
                      (ecase (directive-character item)
                        (#\< (if (colonp (aref nested-items (1- (length nested-items))))
                                 (change-class item 'logical-block-directive
                                               :items nested-items)
                                 (change-class item 'justification-directive
                                               :items nested-items)))
                        (#\( (change-class item 'case-conversion-directive
                                           :items nested-items))
                        (#\{ (change-class item 'iteration-directive
                                           :items nested-items))
                        (#\[ (change-class item 'conditional-directive
                                           :items nested-items)))
                      (check-directive-syntax item)
                      (push item result))))
                 (t
                  (let ((item (pop items)))
                    (specialize-directive item)
                    (check-directive-syntax item)
                    (push item result))))))

(defun interpret-items (items)
  (loop for item across items
        do (if (stringp item)
               (write-string item *destination*)
               (interpret-format-directive item))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Control-string compiler

(defun compile-parameter-value (directive parameter-spec)
  (let* ((parameter-name (car parameter-spec))
         (compile-time-value (funcall parameter-name directive)))
    (cond ((null compile-time-value)
           ;; The parameter was not given at all, in the format control
           ;; string, neither as a constant value, nor as a value to
           ;; be acquired at runtime (# or V).  We must use a default
           ;; value of it has any.
           (getf (cdr parameter-spec) :default-value))
          ((eq compile-time-value 'V)
           ;; The parameter was given the explicit value V in the
           ;; format control string, meaning we use the next argument
           ;; to acquire the value of the parameter.  We must generate
           ;; code to test that there are more arguments, to consume
           ;; the next one, and to check that the type of the argument
           ;; acquired is correct.
           `(progn (when (>= *next-argument-pointer*
                             (length *arguments*))
                     (error 'no-more-arguments))
                   (let ((argument (aref *arguments*
                                         *next-argument-pointer*)))
                     (incf *next-argument-pointer*)
                     (unless (typep argument ',(getf (cdr parameter-spec) :type))
                       (error 'argument-type-error
                              :expected-type
                              ',(getf (cdr parameter-spec) :type)
                              :datum
                              argument))
                     argument)))
          ((eq compile-time-value '|#|)
           ;; The parameter was given the explicit value # in the
           ;; format control string, meaning we use the number of
           ;; remaining arguments as the value of the parameter.
           `(let ((number-of-remaining-arguments
                   (- (length *arguments*) *next-argument-pointer*)))
              (unless (typep number-of-remaining-arguments
                             ',(getf (cdr parameter-spec) :type))
                (error 'argument-type-error
                       :expected-type
                       ',(getf (cdr parameter-spec) :type)
                       :datum
                       number-of-remaining-arguments))
              number-of-remaining-arguments))
          (t
           ;; The parameter was given an explicit value (number or
           ;; character) in the format control string, and this is the
           ;; value we want.
           compile-time-value))))

(defun compile-directive (directive)
  (let ((parameter-specs (parameter-specs (class-name (class-of directive)))))
    `(let ,(loop for parameter-spec in parameter-specs
                 collect `(,(car parameter-spec)
                            ,(getf (cdr parameter-spec) :default-value)))
       (declare (ignorable ,@(mapcar #'car parameter-specs)))
       (let ,(loop for parameter-spec in parameter-specs
                   and given-parameter in (given-parameters directive)
                   collect `(,(car parameter-spec)
                              ,(compile-parameter-value directive parameter-spec)))
         ;; this is not quite right, I think.
         (declare (ignorable ,@(loop for parameter-spec in parameter-specs
                                     and given-parameter in (given-parameters directive)
                                     collect (car parameter-spec))))
         ,(compile-format-directive directive)))))

(defun compile-item (item)
  (if (stringp item)
      `(write-string ,item *destination*)
      (compile-directive item)))

(defun compile-items (items)
  (map 'list #'compile-item items))

(defun compile-control-string (control-string)
  (let ((items (structure-items (split-control-string control-string) nil)))
    `(progn ,@(loop for item across items
                    collect (compile-item item)))))
