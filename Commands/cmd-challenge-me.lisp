;;; handle challenges

(in-package :shanai.po.bot)

(defvar *am-i-currently-battling-p* nil)
(defun bot-currently-battling-p (con)
  "True if CON is currently in a battle."
  ;; Needs to actually return if the connection is in a battle rather then
  ;; some global variables.
  (declare (ignore con))
  *am-i-currently-battling-p*)
(defvar *current-challenger* 0
  "Who is the bot currently challenging?")
(define-bot-command challenge-me (con target user args)
  (when (or (string= "Shanai" (name (shanai.po.client::get-channel (object-id target) con)))
             (string= "Hackage" (name (shanai.po.client::get-channel (object-id target) con))))
    (if *am-i-currently-battling-p*
        (po-proto:write-channel-message "Sorry I'm currently battling!"
                                        (s-util:ensure-stream con)
                                        :channel-id (object-id target))
        (progn
          (if (string= "Shanai Cup" (tier user))
              (progn
                (setq *am-i-currently-battling-p* t
                      *current-challenger* user)
                (pokemon.po.client::random-change-team con)
                (po-proto:write-channel-message (s-util:esc (format nil "I challenged ~A" user))
                                                (s-util:ensure-stream con)
                                                :channel-id (object-id target))
                (po-proto:write-challenge-stuff (object-id
                                                 user)
                                                (s-util:ensure-stream con))
)
              (po-proto:write-channel-message (format nil "~A: Sorry you are not in the 'Shanai Cup' tier!" (s-util:esc (generic:name user)))
                                                (s-util:ensure-stream con)
                                                :channel-id (object-id target))))))
  (force-output (s-util:ensure-stream con)))



(define-bot-command please-challenge-me (con target user args)
  (reply "Can someone please issue the <i>,challenge-me</i> command!"))