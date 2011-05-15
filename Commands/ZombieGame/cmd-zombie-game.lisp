(in-package :shanai.po.bot)

(defvar *zombie-channel* "Shanai"
  "Channel name that the zombie RPG is played in")
(define-bot-command signup (con target user args)
  (when (channel-equal target *zombie-channel*)
    (if (find (object-id user) (shanai.po.bot.zombie::game-players
                                (shanai.po.bot.zombie:zombie-game))
              :key #'object-id)
        (reply "You are already in the game!")
        (progn (alexandria:appendf (shanai.po.bot.zombie::game-players
                                    (shanai.po.bot.zombie:zombie-game))
                                   (list (shanai.po.bot.zombie::make-player (object-id user)
                                                                            (name user))))
               (reply (who:escape-string
                       (format nil "Thanks for signing up!")))))))

(define-bot-command start-zombie-game (con target user args)
  (when (channel-equal *zombie-channel* target :con con)
    (setf (shanai.po.bot.zombie:zombie-game)
          (shanai.po.bot.zombie:make-game-state))
    (privmsg target "Game started!")))

(define-bot-command info-zombie-game-state (con target user args)
  (privmsg target (po-htmlize (shanai.po.bot.zombie:zombie-game))))



