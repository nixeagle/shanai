(asdf:defsystem :shanai.util
  :depends-on (:alexandria :cl-who :usocket)
  :components
  ((:file "packages")
   (:file "symbols" :depends-on ("packages"))
   (:file "util" :depends-on ("packages"))))