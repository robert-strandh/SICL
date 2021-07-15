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
