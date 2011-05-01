;;; all the jaz required to generate the movedex info.

(in-package :shanai.po.bot)

(defun move-to-html-string (move)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:TABLE :ALIGN :CENTER :CELLPADDING 2 :CELLSPACING 0 :STYLE
            "border-width:1px; border-style:solid; border-color:#000;"
            (:THEAD :STYLE "font-weight:bold;"
                    (:TR :STYLE "background-color:#b0b0b0;"
                         (:TD :ALIGN :CENTER "ID")
                         (:TD :ALIGN :CENTER "Name")
                         (:TD :ALIGN :CENTER "Type")
                         (:TD :ALIGN :CENTER "Category")
                         (:TD :ALIGN :CENTER "Power")
                         (:TD :ALIGN :CENTER "Acc.")
                         (:TD :ALIGN :CENTER "PP")
                         (:TD :ALIGN :CENTER "Priority")
                         (:TD :ALIGN :CENTER "Range")))
            (:TBODY
                    (:TR :STYLE "background-color:#c0c0c0; vertical-align:middle;"
                         (:TD :ALIGN :CENTER (cl-who:str (position move pokemon::*movedex*)))
                         (:TD :ALIGN :CENTER (cl-who:esc (pokemon::name move)))
                         (:TD :ALIGN :CENTER (:IMG :SRC  (format nil "Themes/Classic/types/type~A.png" (position (alexandria:make-keyword (pokemon::poketype move)) pokemon::+pokemon-types+))))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::damage-class move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::power move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::accuracy move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::pp move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::priority move)))
                         (:TD :ALIGN :CENTER (cl-who:str (pokemon::range move)))))
            (:TFOOT :STYLE "font-style:italic;"
                    (:TR :STYLE "background-color:#c2c2c2;"
                         (:TD :COLSPAN 9 (:STRONG :STYLE "font-style:normal;" "Description ")
                              (cl-who:esc (pokemon::effect-description move))))))))

(define-bot-command movedex (con target user args)
  (or (ppcre:register-groups-bind ((#'parse-integer move-id) move-name)
          ("^(?:(\\d+)|([a-zA-Z\\s-]+))$" args)
        (let ((move (pokemon::find-move (or move-id move-name))))
          (cond
            ((and move-name (not move))
             (reply (format nil "I cnat raed UR splellng! You gave me ~S which is probably a <b>typo</b> or <b>simply does not exist</b>!" move-name)))
            ((and move-id (not (< 0 move-id pokemon::+total-moves+)))
             (reply "Sorry your move-id is <b>not in the range 1 ... 559</b>!"))
            (move (reply (move-to-html-string move)))
            (t (reply "should not wind up here :D")))
          t))
      (reply "Give me either a <b>number</b> or the <b>name of a move</b>. 
<i>Move names must be english letters and spaces only.</i>")))