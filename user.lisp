;;; Various helpers for repl users

(in-package :shanai.user)


(defun start-po-bot (&key name port host)
  (pokemon.po.client::@login-alpha-test-ai :name name
                                           :port port
                                           :host host))

(defun demo-regex (regex string)
  "Given a REGEX and a STRING return all matches."
  (ppcre:register-groups-bind (m) ((ppcre:create-scanner regex :case-insensitive-mode t) string)
    (format nil "~A contains ~A which matches regular expression: ~A"
            string m regex)))