(in-package :pokemon.po.client)

(defclass server-version ()
  ((version :initarg :version :reader version :type 'string)))
(defclass server-name ()
  ((name :initarg :name :reader name :type 'string)))

(defclass channel ()
  ((id :initarg :id :reader channel-id)
   (name :initarg :name :reader name)
   (trainers :initarg :trainers :accessor trainers)))

(defclass packet ()
  ((contents :initarg :contents :reader contents)
   (command-id :allocation :class :reader command-id)))


(defun read-u1 (in-stream)
  (loop with value = 0
     for low-bit downfrom (* 8 (1- 1)) to 0 by 8 do
     (setf (ldb (byte 8 low-bit) value) (read-byte in-stream))
     finally (return value)))

(defclass channel-list-packet (packet)
  ((command-id :initform 44 :allocation :class)))

(defclass channel-id-mixin ()
  ((channel-id :initarg :channel-id :reader channel-id :type '(unsigned-byte 32)
               :documentation "Numeric identifier cooresponding to a channel name.")))

(defclass channel-players-packet (packet channel-id-mixin)
  ((command-id :initform 45 :allocation :class)))

(defclass trainer ()
  ((user-id :initarg :user-id :reader user-id)
   (name :initarg :name :reader name)
   (info :initarg :info :reader info)
   (losing-msg :initarg :losing-msg :reader losing-msg)
   (winning-msg :initarg :winning-msg :reader winning-msg)))

(defclass join ()
  ((channel-id :initarg :channel-id :reader channel-id)
   (user-id :initarg :user-id :reader user-id)))

(defun read-u2 (in-stream)
  (loop with value = 0
     for low-bit downfrom (* 8 (1- 2)) to 0 by 8 do
       (setf (ldb (byte 8 low-bit) value) (read-byte in-stream))
     finally (return value)))
(defun read-u4 (in-stream)
  (loop with value = 0
     for low-bit downfrom (* 8 (1- 4)) to 0 by 8 do
       (setf (ldb (byte 8 low-bit) value) (read-byte in-stream))
     finally (return value)))

(defun write-u1 (out-stream u1)
  (loop for low-bit downfrom (* 8 (1- 1)) to 0 by 8
     do (write-byte (ldb (byte 8 low-bit) u1) out-stream)))

(defun write-u2 (out-stream u2)
  (loop for low-bit downfrom (* 8 (1- 2)) to 0 by 8
     do (write-byte (ldb (byte 8 low-bit) u2) out-stream)))

(defun write-u4 (out-stream u4)
  (loop for low-bit downfrom (* 8 (1- 4)) to 0 by 8
     do (write-byte (ldb (byte 8 low-bit) u4) out-stream)))

(defun read-po-message (socket)
  "Read the toplevel portion of a PO message.

Messages are of the format <message length (2 octets)><message>."
  ;; need to convert this to returning a flexi stream and or an in memory
  ;; stream..
  (let ((len (read-u2 (usocket:socket-stream socket))))
    (loop for i from 1 to len
       collecting (read-byte (usocket:socket-stream socket)))))

(defun client-ping (socket)
  "Ping the server at SOCKET."
  (loop for b in '(0 1 #x0c)
       do (write-byte b (usocket:socket-stream socket)))
  (force-output (usocket:socket-stream socket)))
(defvar *sock-rcv-log* nil)

(defun demo-find-battle ()
  (write-channel-message "I'm looking for an <b>unrated</b> <i>Shanai Cup</i> battle! Please hit the <b>find-battle</b>!" (usocket:socket-stream @po-socket@) :id (po-client:channel-id (po-client:get-channel "Shanai" @po-socket@)))
  (write-find-battle nil (usocket:socket-stream @po-socket@)))

(defvar *current-battle-id* 0)
(defun handle-battle-message (con value)
  (setq *current-battle-id* (second value))
 #+ () (write-channel-message (princ-to-string value) (usocket:socket-stream con)
                         :id (shanai.po.client::shanai-channel-id)))
(defun get-battle-message-subtype (value)
  (nth 9 value))
(defun handle-packet (socket value type id)
  (log-packet value type id)
  
  (case type
    (:battle-finished (shanai.po.client::handle-battle-finished socket value))
    (:challenge-stuff (progn (setq shanai.po.bot::*am-i-currently-battling-p* nil)))
    (:engage-battle (shanai.po.client::handle-engage-battle socket value)
)
    (:player-list (progn  #+ () (maybe-write-username (nth 3 value) socket)
                          (shanai.po.client::handle-add-trainer-to-trainers socket value)
                          (shanai.po.client::handle-battle-player-list socket value)
                          (maybe-tell-about-name (nth 3 value) socket)))
    (:send-team (progn  #+ () (maybe-write-username (nth 3 value) socket)
                          (shanai.po.client::handle-add-trainer-to-trainers socket value)
                          #+ () (maybe-tell-about-name (nth 3 value) socket)))
    (:battle-message (progn (shanai.po.client::handle-battle-event socket value (get-battle-message-subtype value) id)
                            (handle-battle-message socket value)))
    (:channels-list (progn (loop for chan in value
                              do (let ((chan (make-instance 'shanai.po.client::channel
                                                            :name (cdr chan)
                                                            :id (car chan)))
                                       (id (car chan))
                                       (name (cdr chan)))
                                   (setf (gethash id (channels socket)) chan
                                         (gethash name (channels socket)) chan)))))
    (:add-channel
     (let ((chan (make-instance 'shanai.po.client::channel
                                :name (nth 3 value)
                                :id (nth 1 value)))
           (id (nth 1 value))
           (name (nth 3 value)))
       (unless *isalpha*
         (po-proto:write-channel-message (format nil "Channel #~A was created!"
                                                 name)
                                         (get-stream socket)
                                         :channel-id (po-client:channel-id (po-client:get-channel "shanaindigo" socket))))
       (force-output (get-stream socket))
       (setf (gethash id (channels socket)) chan
             (gethash name (channels socket)) chan)))
    (:remove-channel
     (let ((chan (gethash (nth 1 value) (channels socket)))
           (id (nth 1 value)))
       (when chan
         (unless *isalpha*
           (po-proto:write-channel-message (format nil "Channel #~A was removed!"
                                                   (po-client:channel-name chan))
                                           (get-stream socket)
                                           :channel-id (po-client:channel-id (po-client:get-channel "shanaindigo" socket))))
         (force-output (get-stream socket))
         (remhash (shanai.po.client::channel-name chan) (channels socket))
         (remhash id (channels socket)))))
    (:send-message
     (shanai.po.bot::handle-send-message value))))



(defparameter *channelnames* nil)
(defun maybe-write-username (string stream)
  (unless *isalpha*
    (alexandria:appendf *channelnames* (list string))
    (when (< 19 (length *channelnames*))
      (write-channel-message (pprint-to-string *channelnames*) stream :id +PO-shanaindigo-id+)
      (setq *channelnames* nil))))


(defun demo-regex (regex string)
  (ppcre:register-groups-bind (m) ((ppcre:create-scanner regex :case-insensitive-mode t) string)
    (format nil "~A contains ~A which matches regular expression: ~A"
            string m regex)))

(defun maybe-tell-about-name (name stream)
  (unless *isalpha*
    (or (loop for regex in (shanai.po.bot.user-warn-patterns:blacklisted-username-pattern-list)
           do
             (ppcre:register-groups-bind (badword) ((ppcre:create-scanner regex :case-insensitive-mode t) name)
               (and (not (loop for s in (shanai.po.bot.user-warn-patterns:whitelisted-username-list)
                            when (string-equal name s)
                            return t))
                    (write-channel-message (format nil "<b>Problematic name! <i>~A</i> contains ~A which matches regular expression: ~A</b>" (html-escape-string name)
                                                   (html-escape-string badword)
                                                   (html-escape-string regex)) stream :id
                                                   (po-client:channel-id (po-client:get-channel "shanaindigo" @po-socket@)))))))))

(defun log-packet (value type id)
  (if value
      (setf *sock-rcv-log*
            (cons (cons type value) *sock-rcv-log*))
      (setf *sock-rcv-log* (cons (cons type nil) *sock-rcv-log*))))

(defun handle-po-message (socket octet-list)
  (let ((po-octets (read-po-message socket)))
    (let ((len (1- (length po-octets))))
      (let ((cmd (nth 0 po-octets)))
        (with-input-from-octet-vector (s2 po-octets)
          (case cmd
            (#x0c (progn (client-ping socket)
                         (when (and (not *isalpha*) (po-client:get-channel "Shanai" socket) (= 0 (mod (incf *pingcount*) 20)))
                           (reply *po-socket* (make-instance 'channel-message :channel-id
                                                             (po-client:channel-id (po-client:get-channel "Shanai" socket))
                                                             :message "/players") "/players"))))
            (otherwise 
             (progn (read-u1 s2)
                    (setq *temp* (list po-octets cmd len))
                    (multiple-value-bind (v msgtype msgid) (funcall (protocol-handler cmd) s2)
                      (if v (handle-packet socket  v msgtype msgid)
                          (setq *temp* (list msgtype v))
                          #+ () (log-packet (list (cdr po-octets)) msgtype msgid)))
                    (handle-event socket (decode cmd (cdr po-octets)))))))))))

(defun reply (con target msg)
  (typecase target
    (channel-message
     (when (or *isalpha* (or (= (po-client:channel-id (po-client:get-channel "Shanai" con)) (channel-id target))
                             (= (po-client:channel-id (po-client:get-channel "shanaindigo" con)) (channel-id target) (channel-id target))
))
       (po-proto:write-channel-message msg (get-stream con)
                                       :channel-id (channel-id target))
       (force-output (get-stream con))))
    (private-message
     (print-po-raw con (encode-message (make-instance 'private-message :user-id (user-id target) :message msg))))))

(defun handle-msg (con msg)
  (handle-command% con msg))

(defun split-at-first (item sequence)
  (let ((pos (position item sequence)))
    (if pos
        (cons (subseq sequence 0 pos)
              (subseq sequence (1+ pos)))
        (cons sequence
              nil))))
(defvar *last-possible-command* "")
(defun parse-possible-command (command-string)
  (setf *last-possible-command* command-string)
  (cl-ppcre:register-groups-bind (nick cmd args) ("^(?:([^:]*): )?(?:,|[sS]hanai, )([^\\s]*) ?(.*)" command-string)
    (values nick cmd args)))

(defun parse-possible-user-alias (string)
  (cl-ppcre:register-groups-bind (nick rest) ("^([^:]*): (.*)" string)
    (values nick rest)))


(defun parse-nickname-and-message (msg)
  (if (and (find #\ (message msg)) (find #\: (message msg)))
      (if (typep msg 'private-message)
        (let ((cmd (split-at-first #\ (message msg))))
          (cons nil (message msg)))
        (let ((cmd (split-at-first #\ (message msg))))
          (setf (car cmd) (subseq (car cmd) 0 (1- (length (car cmd)))))
          cmd))
      (cons nil (message msg))))


(defvar *last-time* (GET-UNIVERSAL-TIME))
(defun handle-broken-po-command (message)
  (let ((cmd (cdr (parse-nickname-and-message message))))
    (when (and (or (char= (aref cmd 0) #\/)
                   (char= (aref cmd 0) #\!))
               (< 1 (length cmd))
               (< (+ 60 *last-time*) (GET-UNIVERSAL-TIME)))
      
      (setq *last-time* (GET-UNIVERSAL-TIME))
      "Scripts are down. Please try again later. Abusing them may get you kicked.")))

;;; Needs to eventually move somewhere else other the here.
(defvar *shanai-channel-messages*
  (list)
  "List of channel messages recived in a specific channel.")

(defmethod handle-event ((con connection) (msg channel-message))
  "Handle a message sent to us somehow :P"
  (when (and *isalpha*
             (eq (pokemon.po.client::channel-id msg)
                 (po-client:channel-id (po-client:get-channel "Hackage" con))))
    (alexandria:appendf *shanai-channel-messages*
                        (list (message msg))))
  (multiple-value-bind (nick cmd args) (parse-possible-command (message msg))
    (cond
      ((string-equal (parse-possible-user-alias (message msg)) "+CountBot")
       (reply con (make-instance 'channel-message :channel-id (po-client:channel-id (po-client:get-channel "Shanai" con))
                                 :message "")
              (multiple-value-bind (n m) (parse-possible-user-alias (message msg))
                m)))
      (t
       (unless (string-equal nick "Shanai")
         (unless (= 0 (length  (message msg)))
           (let ((wl (handle-wikilinks (message msg)))
                 (scripts-broken (handle-broken-po-command msg)))
             (cond  (scripts-broken (reply con msg scripts-broken))
                    ((string= "" wl)
                     (handle-msg con msg))
                    (t
                     (reply con msg wl))))))))))

(defmethod handle-event ((con connection) (msg private-message))
  "Handle a message sent to us somehow :P"
  (handle-msg con msg))

(defmethod handle-event ((socket connection) obj)
  "anything else we get."
  )
(defmethod handle-event :after ((socket connection) obj)
  (setf *po-socket-recv-log* (cons obj *po-socket-recv-log*)))


(defun print-po-raw (socket octet-list)
  (typecase octet-list
    (list (loop for oct in octet-list
         do (write-byte oct (get-stream socket))))
    (vector (loop for oct across octet-list
         do (write-byte oct (get-stream socket)))))
  (force-output (get-stream socket)))


(defun po-login-ai (&optional (socket @po-socket@))
  "Log the AI bot in as user 'AI'."
  (print-po-raw socket
                `(0 17 2 0 0 0 12 ,@(loop for i across (flexi-streams:string-to-octets "Shanai" :external-format :utf-16) collect i))))


(defmethod decode ((id (eql 45)) s &key)
  (make-instance 'channel-players-packet :channel-id (read-u4 s)
                 :contents (loop for i from 1 to (read-u4 s)
                              collect (read-u4 s))))

(defmethod decode ((id (eql 2)) connection &key len external-format)
  #+ () (decode-message-2 len connection)
  (cons id (apply #'vector (loop for i from 1 to (or len 0)
                               collecting (read-byte connection)))))

(defmethod decode (id stream &key len external-format)
  (cons id (apply #'vector (loop for i from 1 to (or len 0)
                               collecting (read-byte stream)))))

(defmethod decode :around (id connection &key len (external-format :utf-16))
  (call-next-method id connection :len len :external-format external-format))

(defmethod decode :around (id (connection list) &key len (external-format :utf-16))
  (let ((len (1- (length connection))))
    (with-input-from-vector (s (make-array (1+ len) :element-type '(unsigned-byte 8) :initial-contents connection) :external-format external-format)
      (decode id s :len len :external-format external-format))))

(defmethod decode :around (id (con usocket:stream-usocket) &key (external-format :utf-16) len)
  (decode id (usocket:socket-stream con) :external-format external-format :len len))

(defun read-po-octet-string (stream &key (external-format :utf-16))
  (setq *temp* (list :rpos))
  (let ((len (read-u4 stream)))
    (prog1 (flexi-streams:octets-to-string (loop for i from 1 to len
                                                do       (setq *temp* (list i len))
                                              collect (read-byte stream))
                                           :external-format external-format))))

(defvar *isalpha* nil)
(defun pprint-to-string (thing)
  (with-output-to-string (s) (pprint thing s)))
(defun po-tell-about-error (c)
  (po-proto:write-channel-message (format nil "<center><font color=\"red\">Something went wrong!</font> This means that I will be unable to...</center> <ul><li>complete the current command I was given if any.</li><li>respond to any more commands</li><li>finish any battles I am currently in</li></ul> <font size=\"2\">~A</font>"
                                          
                                          (describe-to-html-string c))
                                  (get-stream *po-socket*)
                                  :channel-id (shanai.po.client::shanai-channel-id *po-socket*))
  (force-output (get-stream *po-socket*))
  (error c))
(defun po-start-listen-loop (&key (port 5777) (host  "nixeagle.org"))
  (bt:make-thread (lambda ()
                    (let ((*po-socket* (connect host port :nickname "Shanai")))
                      (setf @po-socket@ *po-socket*)
                      (unwind-protect
                           (handler-bind ((error #'po-tell-about-error))
                             (progn (loop for sock = (usocket:wait-for-input *po-socket*)
                                       while sock 
                                       do (handle-po-message sock '()))))
                        (usocket:socket-close *po-socket*))
                      (usocket:socket-close *po-socket*))
                    ) :name "PO Socket loop" :initial-bindings `((*standard-output* . *standard-output*))))
(defvar @po-alpha-socket@ nil)
(defun po-start-alpha-listen-loop (&key (port 5777) (host  "nixeagle.org"))
  (bt:make-thread (lambda ()
                    (let* ((*po-socket* (connect host port :nickname "Shanai"))
                          (*isalpha* t)
                          (global:*current-connection* *po-socket*))
                      (setf @po-alpha-socket@ *po-socket*)
                                        ;let ((*po-socket-recv-log* '())) (removnig let expression)
                      (unwind-protect
                           
                           (progn (loop for sock = (usocket:wait-for-input *po-socket*)
                                     while sock 
                                     do (handle-po-message sock '()#+ () (read-po-message sock))))
                        
                        (usocket:socket-close *po-socket*))
                      (usocket:socket-close *po-socket*))
                    ) :name "PO alpha Socket loop" :initial-bindings `((*standard-output* . *standard-output*))))


(defmethod print-object ((obj packet) s)
  (print-unreadable-object (obj s :type t)
    (format s "#~A ~A" (command-id obj) (contents obj))))


(defun to-flexi-stream (s &key (external-format :utf-16))
  (flexi-streams:make-flexi-stream s :external-format (flexi-streams:make-external-format external-format)))



(defun connect (host port &rest args &key (nickname "Shanai"))
  "Create a socket connected to HOST on PORT."
  (declare (ignore nickname))
  (let ((sock (apply #'change-class
                     (usocket:socket-connect host port :element-type '(unsigned-byte 8))
                     'connection args)))
    (reinitialize-instance sock
                           :stream (to-flexi-stream (usocket:socket-stream sock)))))

(defmethod print-object ((obj message) s)
  (print-unreadable-object (obj s :type t)
    (with-slots (message id) obj
      (format s "#~A: ~S" id message))))

(defmethod print-object ((obj server-version) s)
  (print-unreadable-object (obj s :type t)
    (princ (version obj) s)))

(defmethod print-object ((obj server-name) s)
  (print-unreadable-object (obj s :type t)
    (princ (name obj) s)))

(defmethod decode ((id (eql 51)) s &key len)
  (make-instance 'channel-message
                 :channel-id (read-u4 s)
                 :message (read-po-octet-string s)))
(defmethod decode ((id (eql 20)) s &key len)
  (make-instance 'private-message
                 :user-id (read-u4 s)
                 :message (read-po-octet-string s)))

(defmethod decode ((id (eql 33)) s &key)
  (make-instance 'server-version
                 :version (read-po-octet-string s)))

(defmethod decode ((id (eql 55)) s &key)
  (make-instance 'server-name
                   :name (read-po-octet-string s)))




(defmethod decode-message-2 (len connection)
  (let ((s connection #+ () (usocket:socket-stream connection)))
    (make-instance 'trainer :user-id (read-u4 s) :name (read-po-octet-string s)
                   :info (read-po-octet-string s) :losing-msg (loop for i = (read-byte s nil) while i collect i) :winning-msg nil)))


(defmacro with-output-to-sequence ((s) &body body)
  `(flexi-streams:with-output-to-sequence (,s)
     (let ((,s (to-flexi-stream ,s)))
       ,@body)))
(defmethod encode-message ((msg channel-message))
  (with-output-to-sequence (s)
    (write-u2 s (+ 9 (flexi-streams:octet-length (message msg) :external-format :utf-16)))
    (write-u1 s 51) ; channel message ID
    (write-u4 s (channel-id msg))
    (write-u4 s (flexi-streams:octet-length (message msg) :external-format :utf-16))
    (princ (message msg) s)))

(defmethod encode-message ((msg private-message))
  (with-output-to-sequence (s)
    (write-u2 s (+ 9 (flexi-streams:octet-length (message msg) :external-format :utf-16)))
    (write-u1 s 20) ; channel message ID
    (write-u4 s (user-id msg))
    (write-u4 s (flexi-streams:octet-length (message msg) :external-format :utf-16))
    (princ (message msg) s)))

(defmethod encode-message ((msg join))
  (with-output-to-sequence (s)
    (write-u2 s (sizeof msg))
    (write-u1 s 46)
    (write-u4 s (flexi-streams:octet-length (user-id msg) :external-format :utf-16))
    (princ  (user-id msg) s)
   #+ () (write-u4 s (user-id msg))))

(defmethod sizeof ((obj join))
  (+ 5 (flexi-streams:octet-length (user-id obj) :external-format :utf-16)))

(defmethod sizeof ((obj server-name))
  (+ 5 (flexi-streams:octet-length (name obj) :external-format :utf-16)))

(defun encode-join (channel-name)
  (encode-message (make-instance 'join :user-id channel-name)))



(defmethod handle-command (cmd (con connection) (msg message))
  #+ () (reply con msg "Sorry I don't know about that one."))

(defun handle-command% (con msg)
  (multiple-value-bind (nick cmd args) (parse-possible-command (message msg))
    (when cmd
      (handle-command (intern (string-upcase cmd) :keyword)
                      con msg)
      (if nick
          (funcall (shanai.po.bot::bot-command cmd)
                   con msg (shanai.po.client:get-trainer nick con) args)
          (progn (po-proto:write-channel-message (cl-who:escape-string (format nil "Attempted command by private message from: ~A." (shanai.po.client:get-trainer (user-id msg)  *po-socket*)))
                                                 (get-stream con) :channel-id (shanai.po.client::shanai-channel-id))
                 (force-output (get-stream con)))))))




(defun @login-test-ai ()
  "Test function to log the AI in and join Shanai"
  (po-login-ai (progn (po-start-listen-loop :port 5085 :host "91.121.73.228") (sleep 3) @po-socket@)))

(defun @login-alpha-test-ai ()
  "Test function to log the AI in and join Shanai"
  (po-login-ai (progn (po-start-alpha-listen-loop :port 5888 :host "nixeagle.org") (sleep 3) @po-alpha-socket@))
  (po-proto:write-join-channel "Shanai" (get-stream @po-alpha-socket@)))

(defun @debug-login-test-ai ()
  (po-login-ai (progn (let ((*po-socket* (connect "91.121.73.228" 5080 :nickname "Shanai")))
                      (setf @po-socket@ *po-socket*
                            *po-socket-recv-log* '()
                            *sock-rcv-log* '())
                      (progn (loop for sock = (usocket:wait-for-input *po-socket*)
                                         while sock 
                                         do (handle-po-message sock '()#+ () (read-po-message sock)))))
                      @po-socket@)))



(defun random-change-team (con)
  (write-change-team "Shanai" (usocket:socket-stream con)
                     :nickname "Shanai" :info "A pokemon battle bot."
                     :lose ""
                     :win ""
                     :avatar 249
                     :default-tier "Shanai Cup"
                     :pkminfo (cl-user::random-pkminfo)))