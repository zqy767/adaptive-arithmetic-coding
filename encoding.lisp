(in-package #:arithmetic-coding)

(defmethod encoding-8bit-hex ((system code-system) hex)
  (update-limit system hex)
  (update-prefix system)
  (loop for i from hex to 255 do
       (incf (aref (alpha system) i)))
  (incf (total system)))

(defmethod encoding-buffer ((system code-system) buffer)
  (loop for hex being the elements of buffer do
       (encoding-8bit-hex system hex)))

(in-package #:arithmetic-coding)

(defmethod encoding-8bit-hex ((system code-system) hex)
  (update-limit system hex)
  (update-prefix system)
  (loop for i from hex to 255 do
       (incf (aref (alpha system) i)))
  (incf (total system)))

(defmethod encoding-buffer ((system code-system) buffer)
  (loop for hex being the elements of buffer do
       (encoding-8bit-hex system hex)))

(defmacro with-origin-file-to-buffer ((filename buffer-var position-var) &body body)
  (let ((stream (gensym))
        (seq (gensym))
        (system (gensym)))
    `(with-open-file (,stream ,filename
                              :direction :input
                              :element-type  '(unsigned-byte 8))
       (let ((,seq (make-array (file-length ,stream) :element-type '(unsigned-byte 8)))
             (,system (make-instance 'code-system)))
         (read-sequence ,seq ,stream)
         (encoding-buffer ,system ,seq)
         (multiple-value-bind (,buffer-var ,position-var)
             (output-hex ,system)
           ,@body)))))

(defun save-origin-buffer-to-file (buffer position filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence buffer stream)
    (write-byte position stream)))

(defun arithmetic-compression (input-filename output-filename)
  (with-origin-file-to-buffer (input-filename buffer position)
    (save-origin-buffer-to-file buffer position output-filename)))

;;; test part
(defun test-encoding ()
  (let ((system (make-instance 'code-system)))
    (encoding-buffer system '(#x00 #x01 #xef #xff #xac #x30 #x03 #x00))
    (slot-value (output-hex system) 'vector)))


(defun save-origin-buffer-to-file (buffer position filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence buffer stream)
    (write-byte position stream)))

(defun arithmetic-compression (input-filename output-filename)
  (with-origin-file-to-buffer (input-filename buffer position)
    (save-origin-buffer-to-file buffer position output-filename)))

;;; test part
(defun test-encoding ()
  (let ((system (make-instance 'code-system)))
    (encoding-buffer system '(#x00 #x01 #xef #xff #xac #x30 #x03 #x00))
    (slot-value (output-hex system) 'vector)))
