(in-package #:arithmetic-coding)

(defclass bit-octet-bidirection-stream (fundamental-binary-input-stream fundamental-binary-output-stream)
  ((vector :initarg :vector :initform (make-array 1000 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 8)))
   (index :initform 0)
   (position :initform 0)))

(defmethod stream-write-byte ((stream bit-octet-bidirection-stream) bit)
  (with-slots (vector position) stream
    (if (= position 0)
        (vector-push-extend 0 vector))
    (setf (ldb (byte 1 (- 7 position)) (aref vector (1- (fill-pointer vector)))) bit
          position (mod (1+ position) 8))))

(defmethod stream-read-byte ((stream bit-octet-bidirection-stream))
  (with-slots (vector index) stream
    (cond
      ((< index (fill-pointer vector))
       (prog1
           (aref vector index)
         (incf index)))
      (t
       :eof))))

(defclass octet-bit-bidrection-stream (fundamental-binary-input-stream fundamental-binary-output-stream)
  ((vector :initarg :vector :initform (make-array 8000 :adjustable t :fill-pointer 0 :element-type '(unsigned-byte 1)))
   (index :initform 0)))

(defmethod stream-write-byte ((stream octet-bit-bidrection-stream) integer)
  (with-slots (vector) stream
    (loop for i from 7 downto 0 do
         (vector-push-extend (ldb (byte 1 i) integer) vector))))

(defmethod stream-read-byte ((stream octet-bit-bidrection-stream))
  (with-slots (vector index) stream
    (let ((tmp 0))
      (declare (type (unsigned-byte 64) tmp))
      (loop for i from index to (+ index 63) do
           (setf (ldb (byte 1 (- (+ 63 index) i)) tmp) (bit vector i)))
      tmp)))

(defmethod forword-index ((stream octet-bit-bidrection-stream) length)
  (incf (slot-value stream 'index) length))

(defmethod undecode-length ((stream octet-bit-bidrection-stream))
  (- (fill-pointer (slot-value stream 'vector)) (slot-value stream 'index)))

(defclass code-system ()
  ((prefix :accessor prefix
           :initform (make-instance 'bit-octet-bidirection-stream))
   (alpha-cdf :accessor alpha
              :initform (make-array 256 :element-type 'fixnum
                                    :initial-contents (loop for i from 1 to 256 collect i)))
   (total :accessor total
          :initform 256)
   (low :accessor low
        :type '(unsigned-byte 64)
        :initform 0)
   (high :accessor high
         :type '(unsigned-byte 64)
         :initform #.(ldb (byte 64 0) -1))))

(defmethod update-prefix ((system code-system))
  (let ((count 0))
    (labels ((update-body (system)
               (if (eq (logbitp 63 (low system)) (logbitp 63 (high system)))
                   (progn
                     (write-byte (ldb (byte 1 63) (low system)) (prefix system))
                     (incf count)
                     (setf (low system) (ash (ldb (byte 63 0) (low system)) 1)
                           (high system) (+ 1 (ash (ldb (byte 63 0) (high system)) 1)))
                     (update-body system)))))
      (update-body system)
      count)))

;;;remeber to reverse
(defmethod output-hex ((system code-system))
  (loop for i from 0 to 63 do
       (write-byte (ldb (byte 1 (- 63 i)) (low system)) (prefix system)))
  (values (slot-value (prefix system) 'vector)
          (mod (- 8 (slot-value (prefix system) 'position)) 8)))

(defmethod update-limit ((system code-system) hex)
  (let* ((len (- (high system) (low system)))
         (low (low system)))
    (if (= 0 hex)
        (setf (high system) (+ low (truncate (* (/ (aref (alpha system) 0) (total system)) len))))
        (setf (low system) (+ low (truncate (* (/ (aref (alpha system) (1- hex)) (total system)) len)))
              (high system) (+ low (truncate (* (/ (aref (alpha system) hex) (total system)) len)))))))



;;;; test part
(defun test ()
  (let ((stream (make-instance 'bit-octet-bidirection-stream)))
    (loop for i in '(1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 1 0) do
         (write-byte i stream))
    (read-byte stream)
    (read-byte stream)
    (read-byte stream)))

(defun test2 ()
  (let ((stream (make-instance 'octet-bit-bidrection-stream)))
    (loop for i in '(#xf0 #xf0 #xf0 #x0f #xf0 #xac #xac #xac) do
         (write-byte i stream))
    (read-byte stream)))
