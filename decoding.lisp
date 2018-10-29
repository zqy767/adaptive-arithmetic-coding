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


(defun turn-64bit-array-to-number (array)
  (let ((tmp 0))
    (declare (type (unsigned-byte 64) tmp))
    (loop for i from 0 to 63 do
         (setf (ldb (byte 1 (- 63 i)) tmp) (bit array i)))
    tmp))

(defmethod decoding ((system code-system) buffer)
  (if (< 64 (length buffer))
      (let ((approx (turn-64bit-array-to-number buffer)))
        (let ((it (interval-judgement system approx)))
          (update-limit system it)
          (loop for i from it to 255 do
               (incf (aref (alpha system) i)))
          (incf (total system))
          (cons it (decoding system (subseq buffer (update-prefix system))))))))


(defmacro with-compressed-file-to-buffer ((filename buffer-var) &body body)
  (let ((stream (gensym))
        (seq-8bit (gensym))
        (seq-1bit (gensym))
        (system (gensym)))
    `(with-open-file (,stream ,filename
                              :direction :input
                              :element-type  '(unsigned-byte 8))
       (let ((,seq-8bit (make-array (file-length ,stream) :element-type '(unsigned-byte 8)))
             (,seq-1bit (make-array (* 8 (file-length ,stream)) :element-type '(unsigned-byte 1)))
             (,system (make-instance 'code-system))
             (,buffer-var nil))
         (read-sequence ,seq-8bit ,stream)
         (loop for i from 0 to (- (length ,seq-8bit) 5 (aref ,seq-8bit (1- (length ,seq-8bit)))) do
              (loop for j from 0 to 7 do
                   (setf (bit ,seq-1bit (+ (* 8 i) j)) (ldb (byte 1 (- 7 j)) (aref ,seq-8bit i)))))
         (setf ,buffer-var (decoding ,system ,seq-1bit))
         ,@body))))

(defun save-compressed-buffer-to-file (buffer filename)
  (with-open-file (stream filename
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (write-sequence buffer stream)))

(defun arithmetic-decompression (input-filename output-filename)
  (with-compressed-file-to-buffer (input-filename buffer)
    (save-compressed-buffer-to-file buffer output-filename)))

;;; test part
(defun test (bit-array)
  (let ((system (make-instance 'code-system)))
    (decoding system bit-array)))
