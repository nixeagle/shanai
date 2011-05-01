(in-package :shanai.po.protocol)

(defmacro with-forced-output (stream &body body)
  `(progn ,@body (force-output ,stream)))

(defun write-join-channel (channel out)
  "Join CHANNEL given as a string."
  (declare (type string channel)
           (type stream out))
  (write-u2 (1+ (qtstring-length channel)) out) ; Size of packet
  (write-u1 46 out)                             ; packetid
  (write-qtstring channel out))                 ; String encoded

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
  (declare (type string message)
           (type stream out)
           (type u1 channel-id))
  (%write-target-message 51 out channel-id message))

(defun write-challenge-stuff (user stream &key (flags 0) (clauses #x20) (mode 0))
  (write-u2 11 stream)
  (write-u1 7 stream)
  (write-u1 flags stream)
  (write-u4 user stream)
  (write-u4 clauses stream)
  (write-u1 mode stream))

















(in-package :shanai.po.client)

(defclass trainer ()
  ((id :initarg :id :reader trainer-id)
   (name :initarg :name :reader trainer-name)
   (info :initarg :info)
   (flags :initarg :flags)
   (auth :initarg :auth :reader trainer-server-auth)
   (rating :initarg :rating)
   (pokemon :initarg :pokemon :accessor trainer-pokemon)
   (avatar :initarg :avatar)
   (tier :initarg :tier)
   (color :initarg :color)
   (gen :initarg :gen)
   (losing-msg :initarg :losing-msg)
   (winning-msg :initarg :winning-msg)))

(defmethod print-object ((obj trainer) out)
  (print-unreadable-object (obj out :type t)
    (with-slots (id name info auth rating tier gen) obj
      (format out "#~A ~A has a rating of ~A on the generation ~A ~A tier. Has auth level ~A and says '~A'."
              id name rating gen tier auth info))))
(defun trainers (connection)
  (pokemon.po.client::trainers connection))

(defun get-trainer (name connection)
  #+ () (declare (type string name))
  (gethash name (trainers connection)))

(defun (setf get-trainer) (value connection)
  (declare            (type trainer value))
#+ ()  (assert (string= name (slot-value value 'name)))
  (setf (gethash (slot-value value 'name) (trainers connection)) value
        (gethash (slot-value value 'id) (trainers connection)) value))

(flet ((trainer-arglist (list)
         (list :id (nth 1 list)
                 :name (nth 3 list)
                 :info (nth 5 list)
                 :auth (nth 7 list)
                 :flags (nth 9 list)
                 :rating (nth 11 list)
                 :pokemon (nth 13 list)
                 :avatar (nth 15 list)
                 :tier (nth 17 list)
                 :color (nth 19 list)
                 :gen (nth 21 list))))
  (defun make-trainer-from-packet-list (list)
    (apply #'make-instance 'trainer
           (trainer-arglist list)))
  (defun reinitialize-trainer-from-packet-list (trainer list)
    (apply 'reinitialize-instance trainer (trainer-arglist list))))

(defun handle-add-trainer-to-trainers (connection trainers-list)
  (setf (get-trainer connection) (make-trainer-from-packet-list trainers-list)))

(defun handle-player-send-team (connection trainers-list)
  (setf (get-trainer connection) (reinitialize-trainer-from-packet-list  trainers-list)))


;;;; channel
(defclass channel ()
  ((id :initarg :id :reader channel-id)
   (name :initarg :name :reader channel-name)))


(defun get-channel (ref connection)
  (gethash ref (pokemon.po.client::channels connection)))

(defvar *current-engage-battle* nil)

(defvar *current-battle* nil
  "Hold the single battle that the AI is participating or spectating in.")
;;; handling battle stuff here for now

(defun shanai-channel-id ()
  (po-client:channel-id (po-client:get-channel "Shanai" pokemon.po.client::@po-socket@)))

(defun  get-stream (thing)
  (pokemon.po.client::get-stream thing))
(defun dprint (con &rest args)
  (let ((cs (get-stream con)))
    (po-proto:write-channel-message (cl-who:escape-string (apply #'format nil args))
                                    cs :channel-id (shanai-channel-id))
    (force-output cs)))
(defun get-my-battle-slot-id (battle con)
  (shanai.po.battle::get-client-battle-slot-id battle con))
(defun get-opponent-battle-slot-id (battle con)
  (if (zerop (get-my-battle-slot-id battle con))
      1 0))
(defun handle-sendout (con battle value)
  (when (and (= (getf value :battle-message-spot) (get-my-battle-slot-id battle con))
             (not (= 0 (getf value :from-spot))))
    (setq *i-wanna-switch-p* t)
    #+ () (pokemon.po.client::write-battle-switch-pokemon (shanai.po.battle:battle-id battle)
                                                    (get-stream con)
                                                    :pokemon-slot (get-random-possible-poke-hack)) ))
(defun handle-battle-event (con value type id)
  (declare (ignore id))
  (case type
    (:begin-turn (setq *choice-made* nil))
    (:send-out (handle-sendout con *current-battle* value))
    (:tier-section 
     (reinitialize-instance *current-battle*
                            :tier (nth 10 value)))
    (:rated (reinitialize-instance *current-battle* :ratedp (nth 10 value)))
    (:spectator-chat
     #+ () (when (equal (get-trainer (getf value :spectator-user-id 0) con)
                  (get-trainer "nixeagle" con))
       (let ((msg (getf value :message)))
         (cond
           ((string= ":attack" msg) )))))
    (:make-your-choice (handle-battle-choice con *current-battle* (getf value :battle-message-spot)))
    (:ko (handle-battle-ko con *current-battle* (getf value :battle-message-spot))))
  
  )
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
(defun handle-battle-choice (con battle spot)
  (if *koedp*
      (progn (setq *koedp* nil)
             (alexandria:deletef *possible-pokes* *depolyed-poke-slot*)
             (let ((deploypoke (get-random-possible-poke-hack)))
               
               (setq *depolyed-poke-slot* deploypoke)
               (pokemon.po.client::write-battle-switch-pokemon (shanai.po.battle:battle-id battle)
                                                               (get-stream con)
                                                               :pokemon-slot deploypoke)))
      (if *i-wanna-switch-p*
          (progn
            (let ((r (get-random-possible-poke-hack)))
              (dprint con "~A" r)
              (setq *i-wanna-switch-p* nil)
              (alexandria:appendf *possible-pokes* (list *depolyed-poke-slot*))
              (alexandria:deletef *possible-pokes* r)
              (pokemon.po.client::write-battle-switch-pokemon (shanai.po.battle:battle-id battle)
                                                              (get-stream con)
                                                              :pokemon-slot r)))
          (pokemon.po.client::write-battle-use-attack
           (shanai.po.battle:battle-id battle) (get-stream con)
           :attack-slot 0
           :attack-target (get-opponent-battle-slot-id battle con)))))
(defun get-current-pokemon-slot-id (battle)
  (declare (ignore battle))             ; Right now just hacking this!
  *current-poke-slot*)
(defun handle-battle-ko (con battle spot-id)
  (dprint con "Something in spot ~A got KOd in battle: ~A" spot-id battle)
  (when (= spot-id (get-opponent-battle-slot-id battle con))
    #+ () (alexandria:deletef *possible-pokes* *depolyed-poke-slot*)
    (setq *koedp* t)))


(defun handle-engage-battle (con value)
  (let ((battle-id (getf value :battle-id))
        (user1 (get-trainer (nth 3 value) con))
        (user2 (get-trainer (nth 5 value) con)))
    (unless (and user1 user2)
      (let ((me (getf value :me)))
        (setq *current-engage-battle* value
              *pokemon-alive-p* t
              *koedp* nil
              *depolyed-poke-slot* 0
              *i-wanna-switch-p* nil
              *possible-pokes* (list 0 1 2 3 4 5))
        (setq *current-poke-slot* 0)
        (setq *current-battle*
              (make-instance 'shanai.po.battle:battle
                             :id (getf value :battle-id)
                             :challenged (cons (get-trainer (getf value :challenged) con)
                                               (when (eq :am-challenged me)
                                                 (getf value :team)))
                             :challenger (cons (get-trainer (getf value :challenger) con)
                                               (when (eq :am-challenger me)
                                                 (getf value :team)))
                             :clauses (getf value :clauses)
                             :spectatingp nil
                             :spectators ()
                             :gen (getf value :gen)))
        #+ () (dprint con "~A" value)
        (dprint con "I'm in a battle! This battle's id is ~A." battle-id)))
    
   #+ () (dprint con "~A battling ~A" user1 user2)))