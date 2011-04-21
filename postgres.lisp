(in-package :cl-user)

(defun connect-shanai-dev (pass)
  "Connect to the shanai database on my laptop."
  (postmodern:connect-toplevel "shanai" "shanai" pass  "localhost" :port 5432))