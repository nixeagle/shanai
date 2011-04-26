(in-package :shanai.po.bot)

;; Just test what channel the command was done in.
(define-bot-command cmdtest (con target user args)
  (reply (pokemon.po.client::channel-id target)))