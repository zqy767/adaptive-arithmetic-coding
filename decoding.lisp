(in-package #:arithmetic-coding)

(defmethod interval-judgement ((system code-system) approx)
  (let ((len (- (high system) (low system))))
    (and
     (< approx
        (+ (low system) (truncate (* (/ (aref (alpha system) 0) (total system)) len))))
     (return-from interval-judgement 0))
    (loop for i from 1 to 255 do
         (and (<= (+ (low system) (truncate (* (/ (aref (alpha system) (1- i)) (total system)) len)))
                  approx)
              (<  approx
                  (+ (low system) (truncate (* (/ (aref (alpha system) i) (total system)) len))))
              (return-from interval-judgement i)))))


(defmethod decoding ((system code-system) stream)
  (loop while (< 64 (undecode-length stream))
     for approx = (read-byte stream)
     for it = (interval-judgement system approx) do
       (update-limit system it)
       (loop for i from it to 255 do
            (incf (aref (alpha system) i)))
       (incf (total system))
       (forword-index stream (update-prefix system))
     collect it))


(defmacro with-compressed-file-to-stream ((filename buffer-var) &body body)
  (let ((file-stream (gensym))
        (seq (gensym))
        (input (gensym))
        (system (gensym)))
    `(with-open-file (,file-stream ,filename
                                   :direction :input
                                   :element-type  '(unsigned-byte 8))
       (let ((,system (make-instance 'code-system))
             (,buffer-var nil)
             (,seq (make-array (file-length ,file-stream) :element-type '(unsigned-byte 8)))
             (,input (make-instance 'octet-bit-bidrection-stream)))
         (read-sequence ,seq ,file-stream)
         (write-sequence ,seq ,input)
         (setf ,buffer-var (decoding ,system ,input))
         ,@body))))

(defun save-compressed-buffer-to-file (buffer filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence buffer stream)
    (length buffer)))

(defun arithmetic-decompression (input-filename output-filename)
  (with-compressed-file-to-stream (input-filename buffer)
    (save-compressed-buffer-to-file buffer output-filename)))

;;; test part
(defun test (bit-array)
  (let ((system (make-instance 'code-system)))
    (decoding system bit-array)))
