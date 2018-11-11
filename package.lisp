(in-package #:cl)

(defpackage #:arithmetic-coding
  (:nicknames #:ac)
  (:use #:cl #:trivial-gray-streams)
  (:export
   #:save-origin-buffer-to-file
   #:with-origin-file-to-buffer
   #:arithmetic-compression
   #:save-compressed-buffer-to-file
   #:arithmetic-decompression
   #:with-compressed-file-to-buffer))
