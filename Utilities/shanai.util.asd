(asdf:defsystem :shanai.util
  :depends-on (:alexandria)
  :components
  ((:file "packages")
   (:file "symbols" :depends-on ("packages"))
   (:file "util" :depends-on ("packages"))))