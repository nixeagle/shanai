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
  (when (or (channel-equal "Shanai" target :con con)
            (channel-equal "Hackage" target :con con))
    (if (bot-currently-battling-p con)
        (privmsg target "Sorry I'm currently battling!"
                 :con con)
        (progn
          (if (string= "Shanai Cup" (tier user))
              (progn
                (setq *am-i-currently-battling-p* t
                      *current-challenger* user)
                (pokemon.po.client::random-change-team con)
                (privmsg target (s-util:esc (format nil "I challenged ~A"
                                                    (name user)))
                         :con con)
                (po-proto:write-challenge-stuff (object-id user)
                                                (s-util:ensure-stream con)))
              (privmsg target (format nil "~A: Sorry you are not in the 'Shanai Cup' tier!" (s-util:esc (generic:name user))) :con con)))))
  (force-output (s-util:ensure-stream con)))
