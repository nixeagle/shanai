(in-package :shanai.po.bot)

(defvar *am-i-currently-battling-p* nil)
(defvar *current-challenger* 0
  "Who is the bot currently challenging?")
(define-bot-command challenge-me (con target user args)
  (when (and (string= "Shanai" (name (shanai.po.client::get-channel (object-id target) con))))
    (if *am-i-currently-battling-p*
        (po-proto:write-channel-message "Sorry I'm currently battling!"
                                        (shanai.po.client::get-stream con)
                                        :channel-id (object-id target))
        (progn
          (if (string= "Shanai Cup" (tier (shanai.po.client::get-trainer user con)))
              (progn
                (setq *am-i-currently-battling-p* t
                      *current-challenger* (shanai.po.client::get-trainer user con))
                (pokemon.po.client::random-change-team con)
                (po-proto:write-channel-message (shanai.po.client::dprint con "I challenged ~A" user)
                                                (shanai.po.client::get-stream con)
                                                :channel-id (object-id target))
                (po-proto:write-challenge-stuff (object-id
                                                 (shanai.po.client::get-trainer user con))
                                                (shanai.po.client::get-stream con))
)
              (po-proto:write-channel-message (shanai.po.client::dprint con "~A: Sorry you are not in the 'Shanai Cup' tier!" user)
                                                (shanai.po.client::get-stream con)
                                                :channel-id (object-id target))))))
  (force-output (pokemon.po.client::get-stream con)))

