
(deftype u1 ()
  "Shorthand notation for saying `unsigned-byte'."
  '(unsigned-byte 8))

(deftype u2 ()
  "Shorthand notation for 2 `unsigned-byte'."
  '(unsigned-byte 16))

(deftype u4 ()
  "Shorthand notation for 4 `unsigned-byte'."
  '(unsigned-byte 32))

(defun read-u1 (in)
  "Read a single byte from IN stream."
  (declare (optimize (speed 3) (debug 0) (safety 0))
           )
  (the u1 (read-byte in)))

(defun write-u1 (out value)
  (declare 
           (type u1 value))
  (write-byte value out))

(defun read-u2 (in)
  "Read a 2 byte unsigned integer."

  (let ((result 0))
    (declare (type u2 result))
    (setf (ldb (byte 8 8) result) (read-u1 in)
          (ldb (byte 8 0) result) (read-u1 in))
    result))

(defun write-u2 (out value )
  (declare 
           (type u2 value))
  (write-u1 out (ldb (byte 8 8) value))
  (write-u1 out (ldb (byte 8 0) value)))

(defun read-u4 (in)
 
  (let ((result 0))
    (declare (type u4 result))
    (setf (ldb (byte 16 16) result) (read-u2 in)
          (ldb (byte 16 0)  result) (read-u2 in))
    result))

(defun write-u4 (out value )
  (declare 
           (type u4 value))
  (write-u1 out (ldb (byte 8 24) value) )
  (write-u1  out (ldb (byte 8 16) value))
  (write-u1  out (ldb (byte 8  8) value) )
  (write-u1 out  (ldb (byte 8  0) value) ))


(defun read-qtstring (in)
  (let ((len (read-u4 in)))
    (when (not (= #xffffffff len))
      (let ((s (make-string (/ len 2))))
        (setf (stream-external-format in)
              (make-external-format :character-encoding :utf-16be))
        (read-sequence s in)
        (values s (+ len 4))))))

(defun write-qtstring (value out)
  (write-u4 out (ccl:string-size-in-octets value :external-format :utf-16be))
  (setf (stream-external-format out)
        (make-external-format :character-encoding :utf-16be))
  (write-string value out))

(defmacro with-input-from-octet-vector ((var list) &rest body)
  (alexandria:once-only ((l list))
    `(with-input-from-vector (,var (make-array (length ,l)
                                               :element-type '(unsigned-byte 8)
                                               :initial-contents
                                               (typecase ,l
                                                 (array (loop for i across ,l
                                                             collect i))
                                                 (otherwise ,l))))
       ,@body)))



(defvar *po-protocol-handlers*
  (make-array 60 :element-type '(or symbol function)
              :initial-element (lambda (in)
                                 (values nil nil))))


(defun read-server-version (in)
  (values (read-qtstring in) :server-version))

(defun read-server-name (in)
  (values (read-qtstring in) :server-name))

(defun read-server-announcement (in)
  (values (read-qtstring in) :server-announcement))

(defun read-channels-list (in)          ; 44
  (let ((channel-count (read-u4 in)))
    (values (loop for i from 1 to channel-count
               collect (cons (read-u4 in) (read-qtstring in)))
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
    (multiple-value-bind (string length) (read-qtstring in)
      (values (list :level level :name string)
              (+ 1 length)))))
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

(define-po-protocol-reader what-are-you 0 (in)
  "my documentation string!!!!"
  1
  "whatareyou!"  )

(define-po-protocol-reader who-are-you 1 (in))

(define-po-protocol-reader login 2 (in)
  "Read all the info about us the server tells us when connecting."
  )

(define-po-protocol-reader logout 3 (in))
(define-po-protocol-reader send-message 4 (in)
  "A message sent directly from the server itself."
  (read-qtstring in))
(flet ((puid (stream)
         (cons (read-u2 stream) (read-u1 stream))))
  (define-po-protocol-reader player-list 5 (in)
    (list :player-id (read-u4 in)
          :player-name (read-qtstring in)
          :player-info (read-qtstring in)
          :player-auth (read-u1 in)
          :player-flags (read-u1 in)
          :player-rating (read-u2 in)
          :player-pokemon
          (list (puid in) (puid in) (puid in)
                (puid in) (puid in) (puid in))
          :player-avatar (read-u2 in)
          :player-tier (read-qtstring in)
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
(define-po-protocol-reader engage-battle 8 (in)
  (list :battle-id (read-u4 in)
        :user-id (read-u4 in)
        :user-id2 (read-u4 in)))
(define-po-protocol-reader battle-finished 9 (in)
  (list :battle-id (read-u4 in)
        :outcome (read-battle-outcome in)
        :winner-id (read-u4 in)
        :loser-id  (read-u4 in)))
(define-po-protocol-reader battle-message 10 (in))
(define-po-protocol-reader battle-chat 11 (in))
(define-po-protocol-reader keep-alive 12 (in))
(define-po-protocol-reader ask-for-pass 13 (in)
  "Server tells us we need to send a pass.

We return a salt that has to be appended to the hashed password and then
the whole thing has to be hashed again."
  (read-qtstring in))
(defun write-ask-for-pass (hash out)
  (write-u2 out (+ 5 (ccl:string-size-in-octets hash :external-format :utf-16be)))
  (write-u1 out 13)
  (write-qtstring hash out))
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
  (read-qtstring in))
(define-po-protocol-reader c-p-unban 26 (in)
  "Client player unban:"
  (read-qtstring in))
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
         (spot (read-u1 in)))
    (list 
          :battle-id id
          :battle-message-type type
          :battle-message-spot spot
          :battle-message-bytes (loop for b from 3 to size
                                     collect (read-byte in)))))
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
  (read-qtstring in))

(define-po-protocol-reader c-p-t-ban 39 (in))
(define-po-protocol-reader c-p-t-unban 40 (in))
(define-po-protocol-reader player-t-ban 41 (in))
(define-po-protocol-reader get-t-ban-list 42 (in))
(define-po-protocol-reader battle-list 43 (in))
(define-po-protocol-reader channels-list 44 (in)
  (loop for i from (read-u4 in) downto 1
     collecting (cons (read-u4 in) (read-qtstring in))))
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
  (let ((name (read-qtstring in)))
    (list :channel-id (read-u4 in)
          :channel-name name)))

(define-po-protocol-reader channel-message 51 (in)
  "Standard channel message."
  (list :channel-id (read-u4 in)
        :channel-name (read-qtstring in)))
(defun write-channel-message (value stream &key id)
  (print-po-raw stream (encode-message (make-instance 'channel-message :channel-id id :message value))))

(define-po-protocol-reader chan-name-change 52 (in)
  "Sent when the server changes the name of the main channel.

The main channel is always id number 0."
  (funcall #'read-add-channel in))
(define-po-protocol-reader html-message 53 (in)
  "`send-message' but with html turned on."
  (read-qtstring in))
(define-po-protocol-reader channel-html 54 (in)
  "Same as a normal channel message, but with some html parsing.

Most common usage is for whenever a user does /me."
  (list :channel-id (read-u4 in)
        :channel-html (read-qtstring in)))
(define-po-protocol-reader server-name 55 (in))
(define-po-protocol-reader special-pass 56 (in))


(defun protocol-handler (id)
  (aref *po-protocol-handlers* id))

(defun (setf protocol-handler) (value id)
  (declare (type (or function symbol) value)
           (type (integer 0 100) id))
  (setf (aref *po-protocol-handlers* id) value))

(defun read-player-info (s)
  (values (list (read-u4 s)
                (read-qtstring s)
                (read-qtstring s)
                (read-u1 s)
                (cons :flags (read-u1 s))
                (read-u2 s))
          :player-info))


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


(defun google-translate (word from to)
  (destructuring-bind ((data (translations ((translated . text)))))
      (with-input-from-string (s 
                               (let ((drakma:*text-content-types* '(("application" . "json"))))
                                 (drakma:http-request "https://www.googleapis.com/language/translate/v2"
                                                      :parameters `(("key" . "AIzaSyAWx_LJDTojLZ0tJEnw2Nn6m-FnnV77KL8")
                                                                    ("q" . ,word)
                                                                    ("source" . ,from)
                                                                    ("target" . ,to))
                                                      )))
        (json:decode-json s))
    text))


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

