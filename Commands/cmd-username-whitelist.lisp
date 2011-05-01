(in-package :shanai.po.bot)


;;; Add the given user to the unmute list. Works only for shanaindigo on beta server.
;;; Note that the additions are temporary and will go away after the lisp session restarts.
;;; Additionally note that there is no checking that the username is valid.
;;; No way to remove a username from the whitelist through this interface.
(define-bot-command username-whitelist (con target user args)
  (when (= (po-client:channel-id (po-client:get-channel "shanaindigo" con)) (pokemon.po.client::channel-id target))
    (shanai.po.bot.user-warn-patterns:whitelist-username args)
    (reply (format nil "I have whitelisted ~S" (cl-who:escape-string args)))))

