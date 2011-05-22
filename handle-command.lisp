(in-package :pokemon.po.client)

#+ ()
(defmethod handle-command ((cmd (eql :tiers)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/forums/showthread.php?1706\">http://pokemon-online.eu/forums/showthread.php?1706</a>"))

#+ ()
(defmethod handle-command ((cmd (eql :eval)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (when (or (string= "nixeagle" (car (parse-nickname-and-message msg)))
              #+ () (string= "zeroality" (car (parse-nickname-and-message msg))))
      (when (cdr cmd)
        (reply con msg (cl-who:with-html-output-to-string (s nil :indent nil)
                         (:code (cl-who:esc (with-output-to-string (s) (pprint
                                                                        (handler-case (eval (let ((*package* (find-package :pokemon.po.client))) (read-from-string (cdr cmd) nil "Sorry malformed input. Did you forget a closing paren?"))) (error (condition) condition)) s))))))))))


(defun html-escape (input)
  (ppcre:regex-replace "&#39;" (cl-who:escape-string input) "'"))


(defun html-escape-string (input)
  "Prepare a string for safe display in a webbrowser."
  (declare (type string input)
           (optimize (speed 3) (debug 0)))
  (cl-who:escape-string input))

(defun describe-to-html-string (object)
  (let ((*print-right-margin* 70)
        (*print-string-length* 20))
    (cl-who:with-html-output-to-string (s)      
      (:pre (cl-who:esc (with-output-to-string (s) (describe object s)))))))



(defun print-channel-name (chan stream)
  "Print CHAN's name to STREAM.

Format is #foobar"
  (declare (type channel chan))
  (write-char #\# stream)
  (princ (name chan) stream))

(defun handle-wikilinks (stg)
  ""
  #+ () (with-output-to-string (*standard-output*)
      (cl-ppcre:do-scans (ms me rs re "\\[\\[([^\\]]*)\\]\\]" stg) (princ "<a href=\"http://en.wikipedia.org/wiki/") (princ (hunchentoot:url-encode (subseq stg (aref rs 0) (aref re 0)))) (princ "\">") (princ "en:") (princ (cl-who:escape-string (subseq stg (aref rs 0) (aref re 0)))) (princ "</a>")(write-char #\space))))

(defun match-level-and-base-stat (command-string)
  (cl-ppcre:register-groups-bind (a b) ("(\\d+)\\s(\\d+)" command-string)
    (let ((level (parse-integer a))
          (base-stat (parse-integer b)))
      (values level base-stat))))

(defun other-stat-formula (level base iv ev nature)
  (pokemon::_*_ (+ (pokemon::_*_ (+ iv (* 2 base) (pokemon::_/_ ev 4))
               level 1/100) 5) (/ nature 100)))


(defun other-formula-intervals (level base)
  (flet ((formula (iv ev nature)
           (other-stat-formula level base iv ev nature)))
    (let ((result (make-array '(2 2 3) )))
      (setf (aref result 0 0 0) (formula 0 0 90)
            (aref result 0 0 1) (formula 0 0 100)
            (aref result 0 0 2) (formula 0 0 110)
            (aref result 0 1 0) (formula 0 252 90)
            (aref result 0 1 1) (formula 0 252 100)
            (aref result 0 1 2) (formula 0 252 110)
            (aref result 1 0 0) (formula 31 0 90)
            (aref result 1 0 1) (formula 31 0 100)
            (aref result 1 0 2) (formula 31 0 110)
            (aref result 1 1 0) (formula 31 252 90)
            (aref result 1 1 1) (formula 31 252 100)
            (aref result 1 1 2) (formula 31 252 110))
      result)))
#+ ()
(defmethod handle-command ((cmd (eql :statranges)) (con connection) (msg message))
  (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
    (multiple-value-bind (lvl basestats) (match-level-and-base-stat (cdr cmd))
      (when (and lvl basestats)
        (reply con msg (handle-ev-iv-chart (other-formula-intervals lvl basestats) lvl))))))

(defun find-pokemon-by-name (term)
  (loop for val being the hash-value of pokemon::*pokedex*
     when (string-equal (shanai.pokemon:pokemon-name val) term) return val))
(defmethod handle-command ((cmd (eql :scripts)) (con connection) (msg message))
  (reply con msg "<a href=\"http://pokemon-online.eu/scripts.js\">PO Beta server Scripts.js</a>"))


(defun handle-ev-iv-chart (array lvl)
  (declare (type integer lvl))
  (cl-who:with-html-output-to-string (*standard-output*)
    (:table :align :center :cellpadding 2 :cellspacing 0 :style"border-width:1px; border-style:solid; border-color:#000;"
            (:thead :style"font-weight:bold;"
                    (:tr :style"background-color:#b0b0b0;"
                         (:td :align :center (cl-who:fmt "Lvl ~A" lvl))
                         (:td :align :center "Negative")
                         (:td :align :center "Neutral")
                         (:td :align :center "Positive")))
            (:tbody (:tr :style"background-color:#c2c2c2;"
                         (:td :align :right :style"font-weight:bold;" "Min EVs")
                         (:td :align :center (str (aref array 1 0 0)))
                         (:td :align :center (str (aref array 1 0 1)))
                         (:td :align :center (str (aref array 1 0 2))))
                    (:tr :style"background-color:#c2c2c2;"
                         (:td :align :right :style"font-weight:bold;" "Max EVs")
                         (:td :align :center (str (aref array 1 1 0)))
                         (:td :align :center (str (aref array 1 1 1)))
                         (:td :align :center (str (aref array 1 1 2))))))))


(defvar *current-tour-suggestions* ())

#+ ()
(defmethod handle-command ((cmd (eql :typematchup)) (con connection) (msg message))
  (cl-ppcre:register-groups-bind (atk def def2)
    (".*,typematchup (\\S+)\\s+(?:on\\s)?(\\S+)(?:\\s+(\\S+))?" (message msg))
    (let ((atk (gethash atk shanai.pokemon.type::+pokemon-type-name-dictionary+))
          (def (gethash def shanai.pokemon.type::+pokemon-type-name-dictionary+))
          (def2 (gethash def2 shanai.pokemon.type::+pokemon-type-name-dictionary+ :???)))
      (if (and atk def def2)
          (reply con msg (princ-to-string (/ (shanai.pokemon.type::type-matchup atk def def2) 4)))
          (reply con msg "You have me confused! Try the following format: <b>attacking-type</b> on <b>defending-type1</b> <i>defending-type2<i>")))))

#+ ()
(defmethod handle-command ((cmd (eql :commands)) (con connection) (msg message))
  (reply con msg "commands: ,help ,commands ,source ,forums ,tiers ,client ,movedex ,pokedex ,typematchup ,statranges ,tres ,tren ,vote ,create-poll ,poll-info ,challenge-me ,shanai-cup"))

(in-package :shanai.po.bot)
(defparameter *bot-commands*
  (make-hash-table :test #'equalp))

(defun bot-command (name)
  (declare (type string name))
  (gethash name *bot-commands*
           (lambda (con msg nick args)
             (values con msg nick args))))

(defun (setf bot-command) (value name)
  (declare (type string name)
           (type (or function symbol) value))
  (setf (gethash name *bot-commands*) value))

(defun make-bot-command-key (key)
  "Convert KEY into a format appropriate for *USER-COMMANDS*."
  (string-upcase (princ-to-string key)))


(defun reply (con target m)
  (pokemon.po.client::reply con target (princ-to-string m)))

(defmacro define-bot-command (name (con target user args) &body body)
  `(setf (bot-command (make-bot-command-key ',name))
         (lambda (,con ,target ,user ,args)
           (declare (ignorable ,user ,args))
           (flet ((reply (m)
                    (reply ,con ,target m)))
             ,@body))))


(define-bot-command help (con tar user args)
  (reply "ooh look, <a href=\"http://google.com/\">google</a>! To get a list of commands I'll listen to please type <i>,commands</i>"))

(define-bot-command info-nickname (con tar user args)
  (reply (format nil "I think my name is ~A" (pokemon.po.client::nickname con))))

(define-bot-command info-channels (con tar user args)
  (reply "Sorry I have no clue!"))

(define-bot-command find-battle (con target user args)
  (reply "Looking to pick a fight? Best way to get started is clicking that <b>Find Battle</b> button that is directly below where you type text. Try it out!"))

(define-bot-command config (con target user args)
  (reply "For future use to adjust configuration options."))

(define-bot-command source (con target user args)
   (reply  "I'm licensed under the GNU GPL version 3 or later. Find me on <a href=\"http://github.com/nixeagle/shanai\">github</a>!"))

;;; more bot command stuff
(defun handle-send-message (value &key (con (global:current-connection)))
  (let ((pmsg (parse-server-send-message value)))
    (or (when pmsg
          (let ((uid (getf pmsg :user-id))
                (cid (getf pmsg :channel-id))
                (command (getf pmsg :command))
                (args (getf pmsg :args)))
            (case command
              
              (:n (po-client:raw-notice uid cid "<i>You moved north!</i> Next should come info about the new area that you moved into!"))
              (:s (po-client:raw-notice uid cid "<i>You moved south!</i> Next should come info about the new area that you moved into!"))
              (:e (po-client:raw-notice uid cid "<i>You moved east!</i> Next should come info about the new area that you moved into!"))
              (:w (po-client:raw-notice uid cid "<i>You moved west!</i> Next should come info about the new area that you moved into!"))

          
              (otherwise
               (let ((action (rpg-cmd:rpg-command-call value (get-trainer uid con)
                                                       (get-channel cid con)
                                                       con)))
                 (if action
                   (case (car action)
                     (:raw-notice-reply
                      (po-client:raw-notice uid cid (second action)))
                     (:raw-channel-reply
                      (po-client:privmsg cid (second action) :con con)))
                   (progn (po-client:raw-notice uid cid "<timestamp/>Sorry this command does not actually exist yet!")
                          (po-client:privmsg "Shanai"
                                             (format nil "User ~A in channel ~A tried to use a non-existant command!:<br>!~A"
                                                     (who:escape-string (name (get-trainer uid con)))
                                                     (if (get-channel cid con)
                                                         (who:escape-string (name (get-channel cid con)))
                                                         cid)
                                                     (who:escape-string (princ-to-string pmsg)))
                                             :con con)))))))))))

(defun parse-server-send-message (msg)
  (declare (type string msg))
  (ppcre:register-groups-bind ((#'s-util:make-keyword type)
                               (#'parse-integer chanid)
                               (#'parse-integer userid)
                               (#'s-util:make-keyword comchar)
                               (#'s-util:make-keyword command) rest)
      ("^([^\\s]+) (\\d+) (\\d+) :([!?;|])([^\\s]+) ?(.*)" msg)
    (declare (type keyword type command comchar)
             (type alexandria:non-negative-fixnum chanid userid)
             (type string rest))
    (list :type type :channel-id chanid :user-id userid
          :comchar comchar :command command :args rest)))

