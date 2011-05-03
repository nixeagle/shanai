(in-package :pokemon.po.client)





(defvar *po-protocol-handlers*
  (make-array 60 :element-type '(or symbol function)
              :initial-element (lambda (in)
                                 (declare (ignore in))
                                 (values nil nil))))

(defvar *po-battle-protocol-handlers*
  (make-array 60 :element-type '(or symbol function)
              :initial-element (lambda (in)
                                 (declare (ignore in))
                                 (values nil nil))))




(defun read-server-announcement (in)
  (values (binary-data:read-qtstring in) :server-announcement))

(defun read-channels-list (in)          ; 44
  (let ((channel-count (read-u4 in)))
    (values (loop for i from 1 to channel-count
               collect (cons (read-u4 in) (binary-data:read-qtstring in)))
            :channels-list)))

(defun read-leave-channel (in)          ; 47
  (values (list :channel-id (read-u4 in)
                :player-id  (read-u4 in))
          :leave-channel))

(defun read-join-channel (in)
  (values (list :channel-id (read-u4 in)
                :player-id  (read-u4 in))
          :join-channel))

(defun read-engage-battle (in)          ; 8
  (values '()
          :engage-battle))

(defun read-what-are-you (in)           ; 0
  (values '()
          :what-are-you))
(defun read-battle-outcome (in)
  (case (read-u1 in)
    (0 :forfeit)
    (1 :win)
    (2 :tie)
    (3 :close)))

(defun read-tier-node (in)
  (let ((level (read-u1 in)))
    (multiple-value-bind (string length) (binary-data:read-qtstring in)
      (values (list :level level :name string)
              (+ 1 length)))))

(defun protocol-handler (id)
  (aref *po-protocol-handlers* id))

(defun (setf protocol-handler) (value id)
  (declare (type (or function symbol) value)
           (type (integer 0 100) id))
  (setf (aref *po-protocol-handlers* id) value))

(defun battle-protocol-handler (id)
  (aref *po-battle-protocol-handlers* id))

(defun (setf battle-protocol-handler) (value id)
  (declare (type (or function symbol) value)
           (type (integer 0 100) id))
  (setf (aref *po-battle-protocol-handlers* id) value))

(defun read-player-info (s)
  (values (list (read-u4 s)
                (binary-data:read-qtstring s)
                (binary-data:read-qtstring s)
                (read-u1 s)
                (cons :flags (read-u1 s))
                (read-u2 s))
          :player-info))


(defmacro define-po-protocol-reader (name id args &body body)
  "Defines a function to read messages hot off the wire.

These should not refer to any global state. That is no lookup of channel
names to match channel ids or user names to match user ids and so on."
  (multiple-value-bind (bod decl doc)
      (tcr.parse-declarations-1.0::parse-body body :documentation t)
    `(progn
       (defun ,(alexandria:format-symbol t "READ-~A" name) ,args
         ,doc
         ,@decl
         (values (progn ,@bod)
                 ,(alexandria:make-keyword name)
                 ,id))
       (setf (protocol-handler ,id)
             ',(alexandria:format-symbol t "READ-~A" name)))))
(defmacro define-po-battle-protocol-reader (name id args &body body)
  "Defines a function to read messages hot off the wire.

These should not refer to any global state. That is no lookup of channel
names to match channel ids or user names to match user ids and so on."
  (multiple-value-bind (bod decl doc)
      (tcr.parse-declarations-1.0::parse-body body :documentation t)
    `(progn
       (defun ,(alexandria:format-symbol t "READ-BATTLE-COMMAND-~A" name) ,args
         ,doc
         ,@decl
         (values (progn ,@bod)
                 ,(alexandria:make-keyword name)
                 ,id))
       (setf (battle-protocol-handler ,id)
             ',(alexandria:format-symbol t "READ-BATTLE-COMMAND-~A" name)))))

(define-po-protocol-reader what-are-you 0 (in))

(define-po-protocol-reader who-are-you 1 (in))

(define-po-protocol-reader login 2 (in)
  "Read all the info about us the server tells us when connecting."
  )

(define-po-protocol-reader logout 3 (in))
(define-po-protocol-reader send-message 4 (in)
  "A message sent directly from the server itself."
  (binary-data:read-qtstring in))
(flet ((puid (stream)
         (cons (read-u2 stream) (read-u1 stream))))
  (define-po-protocol-reader player-list 5 (in)
    (list :player-id (read-u4 in)
          :player-name (binary-data:read-qtstring in)
          :player-info (binary-data:read-qtstring in)
          :player-auth (read-u1 in)
          :player-flags (read-u1 in)
          :player-rating (read-u2 in)
          :player-pokemon
          (list (puid in) (puid in) (puid in)
                (puid in) (puid in) (puid in))
          :player-avatar (read-u2 in)
          :player-tier (binary-data:read-qtstring in)
          :player-color (list (read-u1 in) (read-u2 in) (read-u2 in)
                              (read-u2 in) (read-u2 in) (read-u2 in))
          :player-gen (read-u1 in))))
(define-po-protocol-reader send-team 6 (in)
  (funcall #'read-player-list in))
(define-po-protocol-reader challenge-stuff 7 (in)
  (list :info-description (read-u1 in)
        :info-opponent-id (read-u4 in)
        :info-clauses     (read-u4 in)
        :info-mode        (read-u1 in)))
(defun read-battle-configuration (in)
  (list :gen (read-u1 in)
        :mode (read-u1 in)
        :challenger (read-u4 in)
        :challenged (read-u4 in)
        :clauses (read-u4 in)))
(defun read-poke-battle-data (in)
  (let ((pnum (read-u2 in))
        (forme (read-u1 in))
        (nick (binary-data:read-qtstring in))
        (max-hp (read-u2 in)))
    (make-instance 'shanai.pokemon:battle-pokemon
                   :id pnum
                   :forme forme
                   :nickname nick
                   :current-hp (read-u2 in)
                   :gender (read-u1 in)
                   :shinyp (if (= 1 (read-u1 in)) t nil)
                   :level (read-u1 in)
                   :item (read-u2 in)
                   :ability (read-u2 in)
                   :happiness (read-u1 in)
                   :base-stats (make-stats max-hp (read-u2 in) (read-u2 in) (read-u2 in)
                                           (read-u2 in) (read-u2 in))
                   :moves (list (read-poke-battle-move in)
                                (read-poke-battle-move in)
                                (read-poke-battle-move in)
                                (read-poke-battle-move in))
                   :evs (make-stats (read-u4 in) (read-u4 in) (read-u4 in)
                                    (read-u4 in) (read-u4 in) (read-u4 in))
                   :ivs (make-stats (read-u4 in) (read-u4 in) (read-u4 in)
                                    (read-u4 in) (read-u4 in) (read-u4 in)))))

(defun read-poke-battle-move (in)
  (list :movenum (read-u2 in)
        :pp (read-u1 in)
        :max-pp (read-u1 in)))
(define-po-protocol-reader engage-battle 8 (in)
  (let* ((battle-id (read-u4 in))
         (user-id (read-u4 in))
         (user-id2 (read-u4 in)))
    (if (or (= 0 user-id)
            (= 0 user-id2))
        (append (list :battle-id battle-id
                      :user-id user-id
                      :user-id2 user-id2
                      :me (if (= 0 user-id)
                              :am-challenger
                              :am-challenged))
                (read-battle-configuration in)
                (list :team (list (read-poke-battle-data in)
                                  (read-poke-battle-data in)
                                  (read-poke-battle-data in)
                                  (read-poke-battle-data in)
                                  (read-poke-battle-data in)
                                  (read-poke-battle-data in))))
        (list :battle-id battle-id
              :user-id user-id
              :user-id2 user-id2))))

(define-po-protocol-reader battle-finished 9 (in)
  (list :battle-id (read-u4 in)
        :outcome (read-battle-outcome in)
        :winner-id (read-u4 in)
        :loser-id  (read-u4 in)))

(define-po-protocol-reader battle-message 10 (in)
  (let* ((id (read-u4 in))
         (size (read-u4 in))
         (type (read-u1 in))
         (spot (read-u1 in))
         (bytes (loop for b from 3 to size
                                     collect (read-byte in))))
    (with-input-from-octet-vector (in2 bytes)
      (multiple-value-bind (val sub-type sub-id)
          (funcall (battle-protocol-handler type) in2)
        (declare (type list val)
                 (ignore sub-id))
        (append (list 
                 :battle-id id
                 :battle-message-type type
                 :battle-message-spot spot
                 :battle-message-bytes bytes
                 :battle-message-sub-type sub-type)
                val)))))
(define-po-protocol-reader battle-chat 11 (in))
(define-po-protocol-reader keep-alive 12 (in)) ; This is a ping.
(define-po-protocol-reader ask-for-pass 13 (in)
  "Server tells us we need to send a pass.

We return a salt that has to be appended to the hashed password and then
the whole thing has to be hashed again."
  (binary-data:read-qtstring in))
(defun write-ask-for-pass (hash out)
  (write-u2 out (+ 5 (ccl:string-size-in-octets hash :external-format :utf-16be)))
  (write-u1 out 13)
  (binary-data:write-qtstring hash out))
(define-po-protocol-reader register 14 (in))

(define-po-protocol-reader player-kick 15 (in)
  (list :kickee-id (read-u4 in)
        :kicker-id (read-u4 in)))

(define-po-protocol-reader player-ban 16 (in)
  (list :bannee-id (read-u4 in)
        :banner-id (read-u4 in)))
(define-po-protocol-reader serv-num-change 17 (in))
(define-po-protocol-reader serv-desc-change 18 (in))
(define-po-protocol-reader serv-name-change 19 (in))
(define-po-protocol-reader send-pm 20 (in))
(define-po-protocol-reader away 21 (in)
  "Format is 4 bytes to the user's id and then the next byte is either zero
or one. 1 = away, 0 = ready for battles."
  (list :player-id (read-u4 in)
        :player-away-p (read-u1 in)))
(define-po-protocol-reader get-user-info 22 (in))
(define-po-protocol-reader get-user-alias 23 (in))
(define-po-protocol-reader get-ban-list 24 (in))
(define-po-protocol-reader c-p-ban 25 (in)
  "client player ban:"
  (binary-data:read-qtstring in))
(define-po-protocol-reader c-p-unban 26 (in)
  "Client player unban:"
  (binary-data:read-qtstring in))
(define-po-protocol-reader spectate-battle 27 (in)
 #+ () (list :battle-id (read-u4 in)
        :battle-message-spot (read-u1 in)
        :??? (read-u1 in)
        :user-id??? (read-u4 in)
        :user-id (read-u4 in)))
(defun write-spectate-battle (value out)
  (declare (type u4 value))
  (write-u2 out 5)
  (write-u1 out 27)
  (write-u4 out value))
(define-po-protocol-reader spectating-battle-message 28 (in)
  (let* ((id (read-u4 in))
         (size (read-u4 in))
         (type (read-u1 in))
         (spot (read-u1 in))
         (bytes (loop for b from 3 to size
                                     collect (read-byte in))))
    (with-input-from-octet-vector (in2 bytes)
      (multiple-value-bind (val sub-type sub-id)
          (funcall (battle-protocol-handler type) in2)
        (declare (type list val)
                 (ignore sub-id))
        (append (list 
                 :battle-id id
                 :battle-message-type type
                 :battle-message-spot spot
                 :battle-message-bytes bytes
                 :battle-message-sub-type sub-type)
                val)))))
(define-po-protocol-reader spectating-battle-chat 29 (in))
(define-po-protocol-reader spectating-battle-finished 30 (in))
(define-po-protocol-reader ladder-change 31 (in))
(define-po-protocol-reader show-team-change 32 (in))
(define-po-protocol-reader version-control 33 (in))
(define-po-protocol-reader tier-selection 34 (in)
  (loop with size = (read-u4 in)
     while (< 0 size)
     collect (multiple-value-bind (node len) (read-tier-node in)
               (decf size len)
               node)))
(define-po-protocol-reader serv-max-change 35 (in))
(define-po-protocol-reader find-battle 36 (in))
(define-po-protocol-reader show-rankings 37 (in))
(define-po-protocol-reader announcement 38 (in)
  (binary-data:read-qtstring in))

(define-po-protocol-reader c-p-t-ban 39 (in))
(define-po-protocol-reader c-p-t-unban 40 (in))
(define-po-protocol-reader player-t-ban 41 (in))
(define-po-protocol-reader get-t-ban-list 42 (in))
(define-po-protocol-reader battle-list 43 (in))
(define-po-protocol-reader channels-list 44 (in)
  (loop for i from (read-u4 in) downto 1
     collecting (cons (read-u4 in) (binary-data:read-qtstring in))))
(define-po-protocol-reader channel-players 45 (in))
(define-po-protocol-reader join-channel 46 (in)
  (list :channel-id (read-u4 in)
        :user-id (read-u4 in)))
(define-po-protocol-reader leave-channel 47 (in)
  (list :channel-id (read-u4 in)
        :user-id (read-u4 in)))
(define-po-protocol-reader channel-battle 48 (in))

(define-po-protocol-reader remove-channel 49 (in)
  "Channel no longer exists.

Reasons a channel might stop existing:
  - No more users remain in the channel.
  - Staff forcefully removes the channel."
  (list :channel-id (read-u4 in)))

(define-po-protocol-reader add-channel 50 (in)
  "Handle new channel creation.

Note that the format of the packets for this is 'backwards'. 
That is the qtstring comes before the channel-id."
  (let ((name (binary-data:read-qtstring in)))
    (list :channel-id (read-u4 in)
          :channel-name name)))

(define-po-protocol-reader channel-message 51 (in)
  "Standard channel message."
  (list :channel-id (read-u4 in)
        :channel-name (binary-data:read-qtstring in)))
(defun write-channel-message (value stream &key id)
  (print-po-raw stream (encode-message (make-instance 'channel-message :channel-id id :message value))))

(define-po-protocol-reader chan-name-change 52 (in)
  "Sent when the server changes the name of the main channel.

The main channel is always id number 0."
  (funcall #'read-add-channel in))
(define-po-protocol-reader html-message 53 (in)
  "`send-message' but with html turned on."
  (binary-data:read-qtstring in))
(define-po-protocol-reader channel-html 54 (in)
  "Same as a normal channel message, but with some html parsing.

Most common usage is for whenever a user does /me."
  (list :channel-id (read-u4 in)
        :channel-html (binary-data:read-qtstring in)))
(define-po-protocol-reader server-name 55 (in)
  (binary-data:read-qtstring in))
(define-po-protocol-reader special-pass 56 (in))

(define-po-battle-protocol-reader blank-message 28 (in)
  "Used to indicate to the client to print a blank line."
  ;; Is there a reason we don't juse use \n like the rest of the world?
  (declare (ignore in)))
(define-po-battle-protocol-reader send-out 0 (in)
  "Sent when a player sends out a pokemon."
  (list :to-spot (read-u1 in)
        :from-spot (read-u1 in)
        :pokemon-id (read-u2 in)
        :forme-id (read-u1 in)
        :pokemon-nick (binary-data:read-qtstring in)
        :percent-health (read-u1 in)
        :status-flags (read-u4 in) ; only 10 bytes of this are used...
        :gender (read-u1 in)
        :shinyp (= 1 (read-u1 in))
        :level (read-u1 in)))
(define-po-battle-protocol-reader send-back 1 (in)
  "Sent when a player returns a pokemon."
  (declare (ignore in)))
(define-po-battle-protocol-reader use-attack 2 (in)
  (list :move-id (read-u2 in)))
(define-po-battle-protocol-reader begin-turn 4 (in)
  "Sent at the start of each turn."
  (list :turn (read-u4 in)))
(define-po-battle-protocol-reader change-hp 6 (in)
  "New value of HP for indicated pokemon"
  ;;; this might not be right in the case of a double battle instead of
  ;;; treating the HP as a 16 bit number we might have to treat it as a 8
  ;;; bit number with the other number meaning to indicate which pokemon on
  ;;; the player's team is meant to be targeted/changed.
  (list :hp (read-u2 in)))

(define-po-battle-protocol-reader ko 7 (in)
  "Sombody's pokemon got knocked out."
  (declare (ignore in)))

(define-po-battle-protocol-reader effective 8 (in)
  nil)
(define-po-battle-protocol-reader missed 9 (in)
  "Says that the pokemon in SLOT missed."
  (declare (ignore in)))


(define-po-battle-protocol-reader critical-hit 10 (in)
  "Sent when a pokemon was nailed with a critical hit."
  (declare (ignore in)))
(define-po-battle-protocol-reader stat-change 12 (in)
  (list :stat-index (read-u1 in)
        :stat-value (binary-data:read-s1  in)))
(define-po-battle-protocol-reader status-change 13 (in)
  "The pokemon in SLOT has it's status flags changed."
  (list :status-flags (read-u1 in)))
(define-po-battle-protocol-reader status-message 14 (in)
  "Somebody had a status effect do something to it.

For example poison damage or paralysis."
  (list :flags (read-u1 in)))
(define-po-battle-protocol-reader failed 15 (in)
  "Move failed."
  (declare (ignore in)))
(define-po-battle-protocol-reader battle-chat 16 (in)
  "Sent when someone speaks in the battle chat."
  (list :message (binary-data:read-qtstring in)))

(define-po-battle-protocol-reader move-message 17 (in)
  "Sent when someone speaks in the battle chat."
  (list :msg-id (read-u2 in)
        :??? (read-u2 in)))

(define-po-battle-protocol-reader item-message 18 (in)
  "Sent when someone speaks in the battle chat."
  (list :msg-id (read-u2 in)
        #+ () :??? #+ () (read-u2 in)))

(define-po-battle-protocol-reader weather-message 22 (in)
  "Some sort of weather effect."
  ;; Format is confusing stlil. (2 3) and (0 3) for sandstorm. (0 3)
  ;; happens first than the (2 3) shows up a bunch of times. I'm guessing
  ;; the (2 3) is something about the sandstorm raging and buffeting each
  ;; of hte pokemon. But I can't tell how the (0 3) works into that
  ;; narrative. The 3 is likely the 'weather-id' and stands for when
  ;; weather is causing some sort of effects.
  nil)                                

(define-po-battle-protocol-reader straight-damage 23 (in)
  "Amount of damage an attack has done in percentage points."
  (list :??? (read-u1 in) ;;; this might be the specific hp count lost...
        :hp-lost (read-u1 in)))
(define-po-battle-protocol-reader ability-message 24 (in)
  "Message saying an ability triggered."
  (list :??? :???))
(define-po-battle-protocol-reader abs-status-change 25 (in)
  "Sent when someone speaks in the battle chat."
  (list :pokemon-ball-position (read-u1 in)
        :status-flags (read-u1 in)))
(define-po-battle-protocol-reader substitute 26 (in)
  ;; status seems to be a 0 when the substitute fades and a 1 when the
  ;; substitute is put up. What happens when the substitute takes the
  ;; damage?
  (list :substitute-status (read-u1 in)))
(define-po-battle-protocol-reader battle-end 27 (in)
  "Sent when someone speaks in the battle chat."
  nil)

(define-po-battle-protocol-reader dynamic-info 31 (in)
  "Indicates the current stat modifiers.

Modifiers include attack, defense, sp-attack, sp-def, speed.

There are 3 other slots that are right now unknown as to what they are for,
but they likely are used to measure spike count among other temporary
effects." 
  (list :attack (binary-data:read-s1  in) :defense (binary-data:read-s1  in)
        :special-attack (binary-data:read-s1  in) :special-defense (binary-data:read-s1  in)
        :speed (binary-data:read-s1  in) :unknown1 (binary-data:read-s1  in)
        :unknown2 (binary-data:read-s1  in) :unknown3 (binary-data:read-s1  in)))
(define-po-battle-protocol-reader spectating 33 (in)
  "Applies for when a spectator joins and or parts."
  (list :spectator-status (case (read-u1 in)
                            (0 :part)
                            (1 :join))
        :spectator-user-id (read-u4 in)))

(define-po-battle-protocol-reader spectator-chat 34 (in)
  (list :spectator-user-id (read-u4 in)
        :message (binary-data:read-qtstring in)))

(define-po-battle-protocol-reader clock-start 37 (in)
  (list :remaining-time (read-u2 in)))
(define-po-battle-protocol-reader clock-stop 38 (in)
  (list :remaining-time (read-u2 in)))


(define-po-battle-protocol-reader rated 39 (in)
  "Is the battle rated or not?"
  (list (= 1 (read-u1 in))))

(define-po-battle-protocol-reader tier-section 40 (in)
  "Is the battle rated or not?"
  (list (binary-data:read-qtstring in)))

(define-po-battle-protocol-reader make-your-choice 43 (in)
  "Sent when it is time for a person to make a move."
  (declare (ignore in))
  nil)


(defun hash-pass (password salt)
  (declare (type string password salt))
  (flet ((digest (thing)
           (ironclad:byte-array-to-hex-string
            (ironclad:digest-sequence :md5 thing))))
    (let ((pass (to-ascii password))
          (s (to-ascii salt)))
      (ironclad:byte-array-to-hex-string
       (ironclad:digest-sequence :md5
                                 (concatenate '(simple-array (unsigned-byte 8) (*)) (to-ascii (digest pass)) s))))))


(defun demo-protocol-read (num)
  (multiple-value-bind (r type)
      (with-input-from-octet-vector
          (s (cdr (find num *po-socket-recv-log* :key #'alexandria:ensure-car)))
        (funcall (protocol-handler num) s))
    (cons type r)))

(defun find-last-packet-by-id (num)
  (find num *po-socket-recv-log* :key #'alexandria:ensure-car))

(defun to-ascii (str)
  (declare (type string str))
  (ccl:encode-string-to-octets str :external-format :ascii))


(defun info-average-of-all-player-info-messages ()
  (float (/ (reduce #'+ (mapcar (lambda (l)
                                  (if (eql 5 (alexandria:ensure-car l))
                                      (length (cdr l))
                                      0))
                                *po-socket-recv-log*))
            (count 5 *po-socket-recv-log* :key #'alexandria:ensure-car))))



(defun info-average-of-all-unparsed-messages ()
  (let ((seq (mapcar (lambda (l)
                       (if (and (eql 5 (alexandria:ensure-car l)))
                                      (length (cdr l))
                                      0))
                     *po-socket-recv-log*)))
    (float (/ (reduce #'+ seq)
              (length seq)))))

(defun info-average-of-all-unparsed-messages-excluding-player-info ()
  (let ((seq (mapcar (lambda (l)
                       (if (and (not (eql 5 (alexandria:ensure-car l))) (numberp (alexandria:ensure-car l)))
                                      (length (cdr l))
                                      0))
                     *po-socket-recv-log*)))
    (float (/ (reduce #'+ seq)
              (length seq)))))


(defun find-user-by-id (id)
  (declare (type u4 id))
  (loop for packet in *po-socket-recv-log*
     when (and (eql 5 (alexandria:ensure-car packet))
               (= id (nth 1 (with-input-from-octet-vector (s (cdr packet))
                               (read-player-list s)))))
       return (with-input-from-octet-vector (s (cdr packet))
                               (read-player-list s))))

(defun find-user-by-name (name)
    (loop for packet in *sock-rcv-log*
     when (and (eql :player-list (car packet))
               (string-equal name (nth 3 (cdr packet))))
       return (cdr packet)))


(defun find-battles-by-user-id (id)
  (declare (type u4 id))
  (loop for packet in *sock-rcv-log*
     when (and (eql :engage-battle (car packet))
               (let ((battleinfo (cdr packet)))                 
                 (or (= id (nth 3 battleinfo))
                     (= id (nth 5 battleinfo)))))
     collect (cdr packet)))

(defun demo-spectate-battle (name)
  (typecase name
    (integer (write-spectate-battle name (usocket:socket-stream @po-socket@)))
    (string (demo-spectate-battle (nth 1 (car (find-battles-by-user-id (nth 1 (find-user-by-name name)))))))))



(defun write-idle (idlep stream)        ; Could also be write-away...
  "When IDLEP indicate to server no battles are wanted."
  (declare (type boolean idlep))
  (write-u2 stream 2)
  (write-u1 stream 21)
  (write-u1 stream (if idlep 1 0))
  (force-output stream))


(defun write-change-team (name stream &key
                          (nickname name)
                          info
                          lose
                          win
                          avatar
                          default-tier
                          (generation 5)
                          (pkminfo (cl-user::test-pkminfo)))
  (declare (type string nickname info lose win default-tier)
           (type (integer 0 500) avatar)
           (type (integer 1 5) generation))
  (let ((out-vector (with-output-to-vector (s nil :external-format :utf-16be)
                      (write-u1 s 6)
                      (binary-data:write-qtstring nickname s)
                      (binary-data:write-qtstring info s)
                      (binary-data:write-qtstring lose s)
                      (binary-data:write-qtstring win s)
                      (write-u2 s avatar)
                      (binary-data:write-qtstring default-tier s)
                      (write-u1 s generation)
                      (loop for p in pkminfo
                         do (%write-poke-personal-from-import-list p s)))))
    (write-u2 stream (length out-vector))
    (write-sequence out-vector stream)
    (force-output stream)))


(defun %write-poke-personal (pokemon stream
                             &key (forme 0) (nickname "") (item 0) (ability 0) (nature 0)
                             (gender 2) (shinyp t) (happiness 255) (level 5)
                             (move1 0) (move2 0) (move3 0) (move4 0)
                             (hp-iv 31) (atk-iv 31) (def-iv 31)
                             (spatk-iv 31) (spdef-iv 31) (speed-iv 31)
                             (hp 0) (atk 0) (def 0) (satk 0)
                             (sdef 0) (spd 0))
  (declare (type string nickname)
           (type boolean shinyp)
           (type (integer 0 2) gender)
           (type (integer 0 31) hp-iv atk-iv def-iv spatk-iv spdef-iv speed-iv)
           (type (integer 0 1000) item)
           (type (integer 0 1000) ability)
           (type (integer 0 16) nature)
           (type (integer 0 560) move1 move2 move3 move4)
           (type (integer 0 255) happiness)
           (type (integer 0 255) hp atk def satk sdef spd)
           (type (integer 1 100) level)
           (type (integer 0 649) pokemon)
           (type (integer 0 30) forme)
           (type stream stream))
  (write-u2 stream pokemon)
  (write-u1 stream forme)
  (binary-data:write-qtstring nickname stream)
  (write-u2 stream item)
  (print ability)
  (write-u2 stream ability)
  (write-u1 stream nature)
  (write-u1 stream gender)
  (write-u1 stream (if shinyp 1 0))
  (write-u1 stream happiness)
  (write-u1 stream level)
  (write-u4 stream move1)
  (write-u4 stream move2)
  (write-u4 stream move3)
  (write-u4 stream move4)
  (write-u1 stream hp-iv)
  (write-u1 stream atk-iv)
  (write-u1 stream def-iv)
  (write-u1 stream spatk-iv)
  (write-u1 stream spdef-iv)
  (write-u1 stream speed-iv)
  (write-u1 stream hp)
  (write-u1 stream atk)
  (write-u1 stream def)
  (write-u1 stream satk)
  (write-u1 stream sdef)
  (write-u1 stream spd))

(defun %locate-trait-position-hack (trait-string)
  (cond ((string= "Own Tempo" trait-string) 20)
        ((string= "Mold Breaker" trait-string) 104)
        ((string= "Unnerve" trait-string) 127)
        ((string= "Klutz" trait-string) 103)
        ((string= "Snow Cloak" trait-string) 81)
        ((string= "Compoundeyes" trait-string) 14)
        ((string= "Limber" trait-string) 7)
        ((string= "Honey Gather" trait-string) 118)
        ((string= "Magic Guard" trait-string) 98)
        ((string= "Sturdy" trait-string) 5)
        ((string= "Sheer Force" trait-string) 125)
        ((string= "Sand Force" trait-string) 159)
        ((string= "Guts" trait-string) 62)
        ((string= "Run Away" trait-string) 50)
        ((string= "Anticipation" trait-string) 107)
        ((string= "Keen Eye" trait-string) 51)
        ((string= "Pickup" trait-string) 53)))
(defun %write-poke-personal-from-import-list (pokelist out)
  "Write a pokemon's data from the import list."
  (destructuring-bind ((poke gender item) trait evlist nature moves) pokelist
    (let ((moves (loop for move in moves collect (position move pokemon::*movedex*))))
      (print (%locate-trait-position-hack trait))
      (apply #'%write-poke-personal (pokemon::number poke) out
             :level 5
             :move1 (first moves)
             :move2 (if (< 1 (length moves)) (second moves) 0)
             :move3 (if (< 2 (length moves)) (third moves) 0)
             :move4 (if (< 3 (length moves)) (fourth  moves) 0)
             :ability (%locate-trait-position-hack trait)
             evlist))))

(defun write-challenge-stuff (user stream &key (flags 0) (clauses #x20) (mode 0))
  (let ((out-vector (with-output-to-vector (s nil :external-format :utf-16be)
                      (write-u1 s 7)
                      (write-u1 s flags)
                      (write-u4 s user)
                      (write-u4 s clauses)
                      (write-u1 s mode))))
    (write-u2 stream (length out-vector))
    (write-sequence out-vector stream)
    (force-output stream)))

(defun demo-beta-challenge (user)
  (write-challenge-stuff (shanai.po.client::trainer-id
                          (shanai.po.client::get-trainer user @po-socket@))
                         (get-stream @po-socket@)))

(defun write-battle-use-attack (battle-id stream &key
                                attack-slot attack-target)
  (write-u2 stream 9) ; message size
  (write-u1 stream 10) ; message type
  (write-u4 stream battle-id) ; battle id
  (write-u1 stream 0) ; player slot
  (write-u1 stream 1) ; subtype
  (write-u1 stream attack-slot)
  (write-u1 stream attack-target)
  (force-output stream))


(defun write-show-team (showp stream)   ; 32
  (write-u2 stream 2)
  (write-u1 stream 32)
  (write-u1 stream (if showp 1 0))
  (force-output stream))

(defun write-ladder-change (ladderp stream)   ; 32
  (write-u2 stream 2)
  (write-u1 stream 31)
  (write-u1 stream (if ladderp 1 0))
  (force-output stream))

(defun write-find-battle (value stream &key ratedp (same-tier-p t) rangedp (range 0) (mode 0))
  (write-u2 stream 8)
  (write-u1 stream 36)
  (let ((result 0))
    (setf (ldb (byte 1 0) result) (if ratedp 1 0)
          (ldb (byte 1 1) result) (if same-tier-p 1 0)
          (ldb (byte 1 2) result) (if rangedp 1 0))
    (write-u4 stream result))
  (write-u2 stream range)
  (write-u1 stream mode)
  (force-output stream))