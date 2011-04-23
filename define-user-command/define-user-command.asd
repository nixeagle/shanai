(asdf:defsystem :shanai.define-user-command
  :components
  ((:file "packages")
   (:file "define-user-command" :depends-on ("packages"))))