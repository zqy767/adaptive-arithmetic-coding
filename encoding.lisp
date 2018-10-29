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

(defmacro with-origin-file-to-buffer ((filename buffer-var) &body body)
  (let ((stream (gensym))
        (seq (gensym))
        (system (gensym)))
    `(with-open-file (,stream ,filename
                              :direction :input
                              :element-type  '(unsigned-byte 8))
       (let ((,seq (make-array (truncate (file-length ,stream)) :element-type '(unsigned-byte 8)))
             (,system (make-instance 'code-system))
             (,buffer-var nil))
         (read-sequence ,seq ,stream)
         (encoding-buffer ,system ,seq)
         (setf ,buffer-var (output-hex ,system))
         ,@body))))

(defun save-origin-buffer-to-file (buffer filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (loop for i from 0 to (1- (length buffer)) by 8 do
         (let ((tmp 0))
           (loop for j from 0 to 7 do
                (setf (ldb (byte 1 (- 7 j)) tmp) (if (< (+ i j) (length buffer))
                                                     (bit buffer (+ i j))
                                                     0)))
           (write-byte tmp stream)))
    (write-byte (- 8 (mod (length buffer) 8)) stream)))

(defun arithmetic-compression (input-filename output-filename)
  (with-origin-file-to-buffer (input-filename buffer)
    (save-origin-buffer-to-file buffer output-filename)))

;;; test part
(defun test-encoding ()
  (let ((system (make-instance 'code-system)))
    (encoding-buffer system '(#x00 #x01 #xef #xff #xac #x30 #x03 #x00))
    (output-hex system)))
