(in-package #:arithmetic-coding)

(defclass code-system ()
  ((prefix :accessor prefix
           :initform '())
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
                     (push (logbitp 63 (low system)) (prefix system))
                     (incf count)
                     (setf (low system) (ash (ldb (byte 63 0) (low system)) 1)
                           (high system) (+ 1 (ash (ldb (byte 63 0) (high system)) 1)))
                     (update-body system)))))
      (update-body system)
      count)))

;;;remeber to revserse
(defmethod output-hex ((system code-system))
  (let ((buffer (make-array (+ 64 (length (prefix system))) :element-type '(unsigned-byte 1))))
    (loop for byte in (reverse (prefix system))
       for i from 0 to (1- (length (prefix system))) do
         (if byte
             (setf (bit buffer i) 1)
             (setf (bit buffer i) 0)))
    (loop for i from 0 to 63 do
         (setf (bit buffer (+ i (length (prefix system)))) (ldb (byte 1 (- 63 i)) (low system))))
    buffer))

(defmethod update-limit ((system code-system) hex)
  (let* ((len (- (high system) (low system)))
         (low (low system)))
    (if (= 0 hex)
        (setf (high system) (+ low (truncate (* (/ (aref (alpha system) 0) (total system)) len))))
        (setf (low system) (+ low (truncate (* (/ (aref (alpha system) (1- hex)) (total system)) len)))
              (high system) (+ low (truncate (* (/ (aref (alpha system) hex) (total system)) len)))))))
