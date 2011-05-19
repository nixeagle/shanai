;;; Various helpers for repl users

(in-package :shanai.user)


(defun start-po-bot (&key name port host)
  (pokemon.po.client::@login-alpha-test-ai :name name
                                           :port port
                                           :host host))