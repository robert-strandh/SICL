(cl:in-package #:sicl-stream)

(defgeneric stream-string (stream))

(defclass string-stream (fundamental-character-input-stream)
  ((%string :initarg :string :reader stream-string)))

(defgeneric start (stream))

(defgeneric end (stream))

(defgeneric index (stream))

(defclass string-input-stream (string-stream)
  ((%start :initarg :start :accessor start)
   (%end :initarg :end :accessor end)
   (%index :initform 0 :accessor index)))

(defun make-string-input-stream
    (string &optional (start 0) end)
  (unless (stringp string)
    (error 'type-error
           :datum string
           :expected-type 'string))
  (unless (typep start `(integer 0 ,(length string)))
    (error 'type-error
           :datum start
           :expected-type `(integer 0 ,(length string))))
  (unless (typep end `(or null (integer 0 ,(length string))))
    (error 'type-error
           :datum end
           :expected-type `(or null (integer 0 ,(length string)))))
  (assert (or (null end) (<= start end)))
  (make-instance 'string-input-stream
    :string string
    :start start
    :end end))

(defmacro with-input-from-string
    ((var string &key index start end)
     &body body)
  (multiple-value-bind (declarations forms)
      (cleavir-code-utilities:separate-ordinary-body body)
    `(let ((,var ,(if (null end)
                      `(make-string-input-stream
                        ,string ,@(if (null start) '() (list start)))
                      `(make-string-input-stream
                        ,string ,(if (null start) 0 start) ,end))))
       ,@declarations
       (multiple-value-prog1
           (progn ,@forms)
         ,@(unless (null index)
             `((setf ,index (index ,var))))))))

(defmethod stream-read-char ((stream string-input-stream))
  (if (>= (index stream) (end stream))
      :eof
      (prog1 (aref (stream-string stream) (index stream))
        (incf (index stream)))))

(defmethod stream-unread-char
    ((stream string-input-stream) (character character))
  (assert (> (index stream) (start stream)))
  (decf (index stream))
  (assert (eql (aref (stream-string stream) (index stream))
               character)))

(defclass string-output-stream (string-stream)
  ())

(defun make-string-output-stream (&key (element-type 'character))
  (assert (eq element-type 'character))
  (make-instance 'string-output-stream
    :string (make-array 10
             :element-type 'character
             :adjustable t
             :fill-pointer 0)))

(defmethod stream-write-char
    ((stream string-output-stream) (character character))
  (vector-push-extend character (stream-string stream)))

(defmacro with-output-to-string
    ((stream-variable
      &optional string-form
      &key (element-type 'character))
     &body body)
  (let ((string-variable (gensym)))
    `(let ((,string-variable ,string-form))
       (when (null ,string-variable)
         (setf ,string-variable
               (make-array 10
                           :element-type ',element-type
                           :adjustable t
                           :fill-pointer 0)))
       (let ((,stream-variable
               (make-instance 'string-output-stream
                 :string ,string-variable)))
         ,@body)
       ,string-variable)))
