(in-package :shanai.po.protocol)

(define-condition blank-message-error (error)
  ((channel-id :initarg :channel-id :reader blank-message-error-channel-id)
   (stream :initarg :stream :reader blank-message-error-stream)
   (connection :initform (global:current-connection)
               :reader blank-message-error-connection)))

(define-condition invalid-channel-name-error (error)
  ((name :initarg :name :reader invalid-channel-name-error-name
         :type 'string)
   (connection :initform (global:current-connection)
               :reader invalid-channel-name-error-connection
               :initarg :connection)))

(defun with-forced-output-function (thunk stream)
  "After THUNK is called, force output on STREAM."
  (declare (type function thunk)
           (type stream stream))
  (funcall thunk)
  (force-output stream))

(defmacro with-forced-output (stream &body body)
  `(with-forced-output-function
       #'(lambda () ,@body) ,stream))

(defun valid-channel-name-p (name)
  "True if NAME is a valid PO channel name.

This implies that the name does not:
  - start with a space.
  - completely blank
  - contain a banned char, which is one of:
    - ~
    - +
    - *"
  (declare (type string name))
  (not (or (string= "" name) (char= #\Space (char name 0))
           (find #\~ name) (find #\+ name) (find #\* name))))
(eos:test valid-channel-name-p
  (eos:is (eq nil (valid-channel-name-p "")))
  (eos:is (eq nil (valid-channel-name-p " ")))
  (eos:is (eq nil (valid-channel-name-p " a")))
  (eos:is (eq nil (valid-channel-name-p "+abc")))
  (eos:is (eq nil (valid-channel-name-p "*abc")))
  (eos:is (eq nil (valid-channel-name-p "ab~c")))
  (eos:is (eq t (valid-channel-name-p "abc")))
  (eos:is (eq t (valid-channel-name-p "abc def"))))

(defun write-join-channel (channel out)
  "Join CHANNEL given as a string."
  (declare (type string channel)
           (type stream out))
  (block :outer
    (with-yielding-restart-case (skip-join-channel () () (return-from :outer))
      (unless (valid-channel-name-p channel)
        (error 'invalid-channel-name-error :name channel)))
    (write-u2 (1+ (qtstring-length channel)) out) ; Size of packet
    (write-u1 46 out)                             ; packetid
    (write-qtstring channel out)))                ; String encoded

(defun write-leave-channel (channel out)
  "Part CHANNEL given as an id."
  (declare (type u4 channel)
           (type stream out))
  (write-u2 5 out)
  (write-u1 47 out)
  (write-u4 channel out))

(defun %write-target-message (packet-id out target message)
  "Write a MESSAGE addressed to TARGET."
  (declare (type u1 packet-id target)
           (type string message)
           (type stream out))
  (write-u2 (+ 5 (qtstring-length message)) out)
  (write-u1 packet-id out)
  (write-u4 target out)
  (write-qtstring message out))


(defun write-channel-message (message out &key (channel-id 0))
  (declare (type (or null string) message)
           (type stream out)
           (type u1 channel-id))
  (if message
      (%write-target-message 51 out channel-id message)
      (restart-case
          (error 'blank-message-error :stream out :channel-id channel-id)
        (skip-write-channel-message () nil))))

(defun write-challenge-stuff (user stream &key (flags 0) (clauses #x00) (mode 0))
  (write-u2 11 stream)
  (write-u1 7 stream)
  (write-u1 flags stream)
  (write-u4 user stream)
  (write-u4 clauses stream)
  (write-u1 mode stream))

(defun write-battle-switch-pokemon (battle-id stream &key
                                    pokemon-slot)
  (write-u2 8 stream) ; message size
  (write-u1 10 stream) ; message type
  (write-u4 battle-id stream) ; battle id
  (write-u1 0 stream) ; player slot...
  (write-u1 2 stream) ; subtype
  (write-u1 pokemon-slot stream)
  (force-output stream))









(in-package :shanai.po.client)

;;;; channel
(defclass channel ()
  ((id :initarg :id :reader channel-id)
   (name :initarg :name :reader channel-name)))

(defmethod generic:object-id ((chan channel))
  (slot-value chan 'id))
(defmethod generic:name ((chan channel))
  (slot-value chan 'name))

(defun get-channel (ref connection)
  (gethash ref (pokemon.po.client::channels connection)))

(defgeneric privmsg (target message &key con &allow-other-keys)
  (:documentation "Write MESSAGE to TARGET over CON."))

(defmethod privmsg ((target channel) (msg string) &key (con pokemon.po.client::*po-socket*))
  (po-proto:write-channel-message msg (s-util:ensure-stream con)
                                  :channel-id (object-id target)))

(defmethod privmsg ((target pokemon.po.client::channel-message) (msg string)
                    &key (con pokemon.po.client::*po-socket*))
  (po-proto:write-channel-message msg (s-util:ensure-stream con)
                                  :channel-id (object-id target)))

(defmethod privmsg ((name string) (msg string) &key (con pokemon.po.client::*po-socket*)
                    (target :channel))
  (case target
    (:channel (privmsg (get-channel name con) msg :con con :target target))))

(defmethod privmsg ((id integer) (msg string) &key (con (global:current-connection))
                    (target :channel))
  (case target
    (:channel (po-proto:write-channel-message msg (s-util:ensure-stream con) :channel-id id))))

(defmethod privmsg :after (target msg &key (con pokemon.po.client::*po-socket*))
  "After writing a message, we want to flush the stream."
  (declare (ignore target msg))
  (force-output (s-util:ensure-stream con)))


(defgeneric notice (destination message &key con target &allow-other-keys))
(defmethod notice :around (destination message &key (con (global:current-connection))
                                       (target :channel))
  (declare (ignore destination message target))
  (let ((global:*current-connection* con))
    (call-next-method)))

(defmethod raw-notice ((user string) destination (message string)
                       &key (con (global:current-connection)) &allow-other-keys)
  (privmsg destination (format nil "/sendhtmlmessage ~A:::~A"
                               user
                               message)
           :target :channel :con con))

(defmethod raw-notice ((user integer) destination (message string)
                       &key (con (global:current-connection)) &allow-other-keys)
  (raw-notice (name (get-trainer user con)) destination message :con con))

(defgeneric channel-equal (chan1 chan2 &key con))
(defmethod channel-equal :around (chan1 chan2 &key (con pokemon.po.client::*po-socket*))
  (call-next-method chan1 chan2 :con con))
(defmethod channel-equal ((chan1 channel) (chan2 channel) &key con)
  (declare (ignore con))
  (equal chan1 chan2))
(defmethod channel-equal ((chan1 string) chan2 &key con)
  (channel-equal chan2 (get-channel chan1 con) :con con))
(defmethod channel-equal ((chan1 channel) chan2 &key con)
  (channel-equal chan2 chan1 :con con))
(defmethod channel-equal ((chan1 integer) chan2 &key con)
  (channel-equal chan2 (get-channel chan1 con)))
(defmethod channel-equal ((chan1 pokemon.po.client::channel-message) chan2 &key con)
  (channel-equal chan2 (get-channel (object-id chan1) con)))
(defvar *current-engage-battle* nil)

(defvar *current-battle* nil
  "Hold the single battle that the AI is participating or spectating in.")
;;; handling battle stuff here for now

(defun shanai-channel-id (&optional (con (global:current-connection)))
  (po-client:channel-id (po-client:get-channel "Shanai" con)))

(defun shanai-user-id (con)
  "Get my user-id"
  (and (get-trainer (generic:name con) con)
       (trainer-id (get-trainer (generic:name con) con))))

(defun  get-stream (thing)
  (pokemon.po.client::get-stream thing))

(defun get-my-battle-slot-id (battle con)
  (shanai.po.battle::get-client-battle-slot-id battle con))
(defun get-opponent-battle-slot-id (battle con)
  (if (zerop (get-my-battle-slot-id battle con))
      1 0))
(defun make-opponent-battle-pokemon (lst)
  "Given a list construct the opponent's battle pokemon."
  (let ((pid (getf lst :pokemon-id))
        (formeid (getf lst :forme-id)))
    (let ((dpoke (gethash pid pokemon::*pokedex*)))
      (make-instance 'shanai.pokemon:battle-pokemon
                     :id pid
                     :forme formeid
                     :type (list (generic:type1 dpoke)
                                 (generic:type2 dpoke))
                     :nickname (getf lst :pokemon-nick)
                     :current-hp (getf lst :percent-health)
                     :level (getf lst :level)))))


(defun find-move (name-or-id)
  (pokemon::find-move name-or-id))
(defun find-pokemon (name-or-id)
  (gethash name-or-id pokemon::*pokedex*))

(defun score-move-on-pokemon (move opp-poke)
  "Indicate how powerful MOVE will be on OPP-POKE.

Range is between 0 and 16 with 16 indicating maximum damage.

A value of 4 indicates that the move will do normal damage."
  (let ((move (typecase move
                (pokemon::move move)
                (list (find-move (getf move :movenum)))
                (fixnum (find-move move)))))
    (when move
      (shanai.pokemon.type::type-matchup (type1 move) (type1 opp-poke) (type2 opp-poke)))))

(defun stabp (move pokemon)
  "Does MOVE do 1.5x damage when used by POKEMON?"
  (or (eq (type1 move) (type1 pokemon))
      (eq (type1 move) (type2 pokemon))))

(defun compute-move-scores-by-position (battle-pokemon opp-poke)
  "Given a BATTLE-POKEMON compute scores for each of its 4 moves."
  (mapcar (lambda (move)
            (let ((move (find-move (getf move :movenum))))
              (if move
                  (* (score-move-on-pokemon move opp-poke) (if (stabp move battle-pokemon)
                                                               3/2 1)
                     (pokemon::move-power move)
                     ; frost breath hack
                     (if (= 524 (object-id move)) 2 1)
                     (if (= 486 (object-id move)) 60 1))
                  0)))
          (shanai.pokemon:pokemon-moves battle-pokemon)))

(defun compute-next-pokemon-switch-scores-by-position (trainer opp-poke)
  (loop for poke across (shanai.team:team-pokemon trainer)
       if (shanai.pokemon:pokemon-koedp poke) collect 0 else 
       collect (loop for i in (compute-move-scores-by-position poke opp-poke)
               for defense =
                    (shanai.pokemon.type::type-matchup (type1 opp-poke)
                                                       (type1 poke)
                                                       (type2 poke))
                  for defense2 = (shanai.pokemon.type::type-matchup (type2 opp-poke)
                                                                    (type1 poke)
                                                                    (type2 poke))
                  for mindef = (max defense defense2) 
                  maximizing (/ (/ i (if (= mindef 0) 1/8 mindef))
                                (if (< 0 i)
                                    5/3
                                    1)))))

(defun handle-sendout (con battle value)
  (if (and (= (getf value :battle-message-spot) (get-my-battle-slot-id battle con))
             (not (= 0 (getf value :from-spot))))
      (setq *i-wanna-switch-p* t))
  (when (= (getf value :battle-message-spot) (get-my-battle-slot-id battle con))
    (let ((opp-team (shanai.team:team-pokemon (generic:challenged battle))))
      (unless (= (getf value :to-spot) (getf value :from-spot))
        (swap-active-team-pokes-by-id opp-team (getf value :from-spot)))
      (setf (aref opp-team (getf value :to-spot))
            (make-opponent-battle-pokemon value)))))

(defun handle-battle-finished (con value)
  "A battle finished on CON.

This does not imply the bot itself was in the battle!"
  (when (and (shanai-user-id con)
             (or (= (getf value :winner-id)
                    (shanai-user-id con))
                 (= (getf value :loser-id)
                    (shanai-user-id con))))
    (setf (shanai.po.battle:battle-in-progress-p *current-battle*)
          nil)
    (setq shanai.po.bot::*am-i-currently-battling-p* nil)))
(defun handle-battle-player-list (con value)
  (when shanai.po.bot::*am-i-currently-battling-p*
    (when (equal (generic:object-id shanai.po.bot::*current-challenger*)
                 (generic:object-id (get-trainer (nth 1 value) con)))
      (unless (string= "Shanai Cup" (generic:tier (get-trainer (nth 1 value) con)))
        (setq shanai.po.bot::*am-i-currently-battling-p* nil)))))
(defun handle-battle-event (con value type id)
  (declare (ignore id))
  (case type
    (:begin-turn (setq *choice-made* nil)
                 (shanai.battle::!incf-turn *current-battle*))
    (:send-out (handle-sendout con *current-battle* value))
    (:battle-end (setq shanai.po.bot::*am-i-currently-battling-p* nil)
                 (setf (shanai.po.battle:battle-in-progress-p *current-battle*)
                       nil))
    (:tier-section 
     (reinitialize-instance *current-battle*
                            :tier (nth 10 value)))
    (:rated (reinitialize-instance *current-battle* :ratedp (nth 10 value)))
    (:spectator-chat)
    (:make-your-choice (handle-battle-choice con *current-battle* (getf value :battle-message-spot)))
    (:ko (handle-battle-ko con *current-battle* (getf value :battle-message-spot)))))

(defvar *pokemon-alive-p* t)
(defvar *current-poke-slot* 0)
(defvar *depolyed-poke-slot* 0)
(defvar *choice-made* nil)
(defvar *koedp* nil)
(defvar *possible-pokes*
  '(1 2 3 4 5))
(defvar *i-wanna-switch-p* nil)
(defun get-random-possible-poke-hack ()
  (alexandria:random-elt *possible-pokes*))

(defun get-active-pokemon (trainer)
  "Get the current active pokemon of TRAINER."
  (aref (shanai.team:team-pokemon trainer) 0))

(defun swap-active-team-pokes-by-id (team poke-id)
  (let ((active (aref team 0))
        (new (aref team poke-id)))
    (setf (aref team 0) new
          (aref team poke-id) active)
    team))

(defun select-random-non-koed-poke (team)
  (alexandria:random-elt
   (loop for p across (shanai.team:team-pokemon team)
      unless (shanai.pokemon:pokemon-koedp p)
      collect p)))
(defun position-of-pokemon-in-team (poke team)
  (position poke (shanai.team:team-pokemon team)))

(defun handle-battle-choice (con battle spot)
  (let* ((me (generic:challenger battle))
        (my-team (shanai.team:team-pokemon me)))
    (if *koedp*
        (progn (setq *koedp* nil)
               (let ((deploypoke (select-poke battle (challenger battle)
                                    (get-active-pokemon (challenged battle)))))
                 (swap-active-team-pokes-by-id my-team deploypoke)
                 (po-proto::write-battle-switch-pokemon (shanai.po.battle:battle-id battle)
                                                        (s-util:ensure-stream con)
                                                        :pokemon-slot deploypoke)))
        (if (and (< 1 (length my-team)) #+ () *i-wanna-switch-p*)
            (progn
              (let ((r (select-poke battle (challenger battle)
                                    (get-active-pokemon (challenged battle)))))
                (if (= r 0)
                    (pokemon.po.client::write-battle-use-attack
                     (shanai.po.battle:battle-id battle) (s-util:ensure-stream con)
                     :attack-slot (select-attack battle
                                         (get-active-pokemon (challenger battle))
                                        (get-active-pokemon (challenged battle)))
                     :attack-target (get-opponent-battle-slot-id battle con))
                    (progn (setq *i-wanna-switch-p* nil)
                           (swap-active-team-pokes-by-id my-team r)
                           (po-proto::write-battle-switch-pokemon (shanai.po.battle:battle-id battle)
                                                                  (s-util:ensure-stream con)
                                                                  :pokemon-slot r)))))
            (pokemon.po.client::write-battle-use-attack
             (shanai.po.battle:battle-id battle) (s-util:ensure-stream con)
             :attack-slot (select-attack battle
                                         (get-active-pokemon (challenger battle))
                                         (get-active-pokemon (challenged battle)))
             :attack-target (get-opponent-battle-slot-id battle con))))))
(defun select-attack (battle active-pokemon opp-pokemon)
  "Returns the id of the attack to execute."
  (declare (ignore battle))
  (let ((scored-moves (compute-move-scores-by-position active-pokemon opp-pokemon)))
    (position (loop for move in scored-moves
                 maximizing (or move 0))
              scored-moves)))

(defun select-poke (battle trainer opp-pokemon)
  (declare (ignore battle))
  (let ((scored-poke (compute-next-pokemon-switch-scores-by-position trainer opp-pokemon)))
    (position (loop for move in scored-poke
                 maximizing (or move 0))
              scored-poke)))
(defun get-current-pokemon-slot-id (battle)
  (declare (ignore battle))             ; Right now just hacking this!
  *current-poke-slot*)
(defun handle-battle-ko (con battle spot-id)
  (case spot-id
    (0 (shanai.pokemon:!mark-koed (get-active-pokemon (generic:challenger battle))))
    (1 (shanai.pokemon:!mark-koed (get-active-pokemon (generic:challenged battle)))))
  (when (= spot-id (get-opponent-battle-slot-id battle con))
    (setq *koedp* t)))

(defun create-battle-trainer (trainer)
  "Create a trainer suited for battle.

Each new battle the bot participates in, we will create new battle
trainers to participate in it."
  (make-instance 'shanai.battle::battle-trainer
                 :gen (generic:gen trainer)
                 :tier (generic:tier trainer)
                 :name (generic:name trainer)))

(defun handle-engage-battle (con value)
  (let ((battle-id (getf value :battle-id))
        (user1 (get-trainer (nth 3 value) con))
        (user2 (get-trainer (nth 5 value) con)))
    (unless (and user1 #+ () user2)
      (when (and (member :challenger value)
                 (member :challenged value))
        (let ((me (getf value :me)))
          (setq *current-engage-battle* value
                *pokemon-alive-p* t
                *koedp* nil
                *depolyed-poke-slot* 0
                *i-wanna-switch-p* nil
                *possible-pokes* (list 1 2 3 4 5))
          (setq *current-poke-slot* 0)
          (let ((challenger (get-trainer (getf value :challenger) con))
                (challenged (get-trainer (getf value :challenged) con)))
            (setq *current-battle*
                  (make-instance 'shanai.po.battle:battle
                                 :id (getf value :battle-id)
                                 :in-progress-p t
                                 :challenged (create-battle-trainer challenged)
                                 
                                 :challenger (create-battle-trainer challenger)
                                 
                                 :clauses (getf value :clauses)
                                 :spectatingp nil
                                 :spectators ()
                                 :gen (getf value :gen))))
          (when (eq :am-challenged me)
            (setf (shanai.team:team-pokemon (shanai.battle:battle-challenged *current-battle*))
                  (apply #'vector (getf value :team))))
          (when (eq :am-challenger me)
            (setf (shanai.team:team-pokemon (shanai.battle:battle-challenger *current-battle*))
                  (apply #'vector (getf value :team)))))))))