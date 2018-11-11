(asdf:defsystem :cl-arithmetic-coding
  :serial t
  :depends-on ("trivial-gray-streams")
  :default-pathname "/home/zqy787/arith-coding"
  :components ((:file "package")
               (:file "base")
               (:file "encoding")
               (:file "decoding")))
