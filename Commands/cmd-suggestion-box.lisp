(in-package :shanai.po.bot)

(define-bot-command info-suggestion-box (con target user args)
  (when (channel-equal (object-id target) "Tournaments")
    (reply "The <i>suggestion box</i> is a way for users to have some input on which tour is next played. When a MU announces that the <b>suggestion box is open</b> in #tournaments. each user may suggest a single tier by putting that tier name, <font color=red><u>spelled correctly</u></font> on a single line by itself. Once the suggestion box has been closed, please do not spam more tiers. <small>This tidbit is <u>informational</u>, the suggestion box is <b><i>NOT</i></b> open unless a MU says it is.")))


(define-bot-command suggestion-box-open (con target user args)
  (when (channel-equal (object-id target) "Tournaments")
    (when (or (string-equal (name user) "nixeagle")
              #+ () (string-equal (name user) "CrashingBoomBang"))
      (reply "The <i>suggestion box</i> is open! Please suggest a single tier, spelled correctly, on one line. <small>Look up correct tier spellings in the <u>Tiers</u> dropdown menu. Incorrect spellings are simply ignored.</small>"))))