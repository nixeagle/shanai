(in-package :shanai.po.bot)

(defvar *am-i-currently-battling-p* nil)
(define-bot-command challenge-me (con target user args)
  (when (and (string= "Shanai" (name (shanai.po.client::get-channel (object-id target) con))))
    (if *am-i-currently-battling-p*
        (po-proto:write-channel-message "Sorry I'm currently battling!"
                                        (shanai.po.client::get-stream con)
                                        :channel-id (object-id target))
        (progn
          (setq *am-i-currently-battling-p* t)
          (pokemon.po.client::random-change-team con)
          (pokemon.po.client::write-challenge-stuff (object-id
                                                     (shanai.po.client::get-trainer user con))
                                                    (shanai.po.client::get-stream con))
          (po-proto:write-channel-message (shanai.po.client::dprint con "I'm going to challenge ~A" user)
                                          (shanai.po.client::get-stream con)
                                          :channel-id (object-id target)))))
  (force-output (pokemon.po.client::get-stream con)))

