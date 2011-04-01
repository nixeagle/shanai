(defpackage #:pokemon.po.client
  (:import-from :split-sequence #:split-sequence))

(in-package :pokemon.po.client)
(defvar *po-socket*
  "Global dynamic variable for holding pokemon online sockets.

Local to each thread that is created.")

(defvar *po-socket-recv-log*
  "Continuing log of all recieved messages from all PO servers.")
(defvar @po-socket@
  "Contains a pointer to the last created pokemon online socket.")

(defclass connection (usocket:stream-usocket)
  ((nickname :initarg :nickname :reader nickname)
   (channels :initarg :channels :accessor channels)))

(defclass channel-message (message)
  ((id :initarg :channel-id :reader channel-id)))

(defclass message ()
  ((message :initarg :message :reader message :type 'string)
   (id :type '(unsigned-byte 32))))

(defclass private-message (message)
  ((id :initarg :user-id :reader user-id)))

(defclass server-version ()
  ((version :initarg :version :reader version :type 'string)))
(defclass server-name ()
  ((name :initarg :name :reader name :type 'string)))

(defun read-u1 (in-stream)
  (loop with value = 0
     for low-bit downfrom (* 8 (1- 1)) to 0 by 8 do
     (setf (ldb (byte 8 low-bit) value) (read-byte in-stream))
     finally (return value)))

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

(defun make-po-input-utf16-stream (list)
  "Given one of the po strings from a message, convert that into an in memory stream.

This means once converted we can use READ-CHAR and other normal common lisp
stream operations."
  (declare (type (or vector list) list))
  (flexi-streams:make-flexi-stream (flexi-streams:make-in-memory-input-stream list)
                                   :external-format (flexi-streams:make-external-format :utf-16)))


(defun make-po-output-utf16-stream (stream)
  (flexi-streams:make-flexi-stream stream :external-format (flexi-streams:make-external-format :utf-16)))

(defun read-po-message-length (socket)
  (read-u2 (usocket:socket-stream socket)))

(defun read-po-message (socket)
  "Read the toplevel portion of a PO message.

Messages are of the format <message length (2 octets)><message>."
  ;; need to convert this to returning a flexi stream and or an in memory
  ;; stream..
  (loop for i from 1 to (read-u2 socket)
     collecting (read-byte (usocket:socket-stream socket))))

(defun client-ping (socket)
  "Ping the server at SOCKET."
  (loop for b in '(0 1 #x0c)
       do (write-byte b (usocket:socket-stream socket)))
  (force-output (usocket:socket-stream socket)))

(defun handle-po-message (socket octet-list)
  (let ((len (1- (read-po-message-length socket))))
    (let ((cmd (read-byte (usocket:socket-stream socket))))
      (case cmd
        (#x0c (client-ping socket))
        (otherwise (handle-event socket (decode cmd socket :len len)))))))

(defun reply (con target msg)
  (typecase target
    (channel-message
     (print-po-raw con (encode-message (make-instance 'channel-message :channel-id (channel-id target) :message msg))))
    (private-message
     (print-po-raw con (encode-message (make-instance 'private-message :user-id (user-id target) :message msg))))))

(defun handle-msg (con msg)
  (handle-command% con msg))

(defmethod handle-event ((con connection) (msg channel-message))
  "Handle a message sent to us somehow :P"
  (unless (search "Shanai:" (message msg) :start2 0 :end2 7);ignore messages sent from us.
    (let ((wl (handle-wikilinks (message msg))))
      (if (string= "" wl)
          (handle-msg con msg)
          (reply con msg wl)))))

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
         do (write-byte oct (usocket:socket-stream socket))))
    (vector (loop for oct across octet-list
         do (write-byte oct (usocket:socket-stream socket)))))
  (force-output (usocket:socket-stream socket)))


(defun po-login-ai (&optional (socket @po-socket@))
  "Log the AI bot in as user 'AI'."
  (print-po-raw socket
                `(0 17 2 0 0 0 12 ,@(loop for i across (flexi-streams:string-to-octets "Shanai" :external-format :utf-16) collect i))))


(defun po-send-string (socket string)
  "Sends a string to the main channel.

This is intended for user interaction vie the REPL more then anything
else."
  (let* ((len (+ 9 (* 2 (length string))))
        (octs `(00 ,len 51 0 0 0 0 0 0 0 ,(* 2 (length string))
                                   ,@(loop for s across string
                                        appending (list 0 (char-code s))))))
    (print-po-raw socket octs)
    octs))

(defun print-po-msg (socket string)  
  )

(defun make-po-octet-string (string &key (external-format :utf-16))
  (declare (type string string))
  (flexi-streams:with-output-to-sequence (s)
    (let ((s (make-po-output-utf16-stream s)))
      (write-u4 s (flexi-streams:octet-length string :external-format external-format))
      (princ string s))))

(defun read-array-octet-string (array &key (external-format :utf-16))
  (with-input-from-vector (s array :external-format :utf-16)
    (read-po-octet-string s :external-format external-format)))

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
  (let ((len (read-u4 stream)))
    (flexi-streams:octets-to-string (loop for i from 1 to len
                                       collect (read-byte stream))
                                    :external-format external-format)))


(defun po-start-listen-loop ()
  (bt:make-thread (lambda ()
                    (let ((*po-socket* (connect "nixeagle.org" 5777 :nickname "AI")))
                      (setf @po-socket@ *po-socket*
                            *po-socket-recv-log* '())
                      (unwind-protect
                           (handler-case 
                               (progn (loop for sock = (usocket:wait-for-input *po-socket*)
                                         while sock 
                                         do (handle-po-message sock '()#+ () (read-po-message sock))))
                             (error (condition) (print-po-msg *po-socket* (princ-to-string condition))))
                        (usocket:socket-close *po-socket*))
                      (usocket:socket-close *po-socket*))
                    ) :name "PO Socket loop" :initial-bindings `((*standard-output* . *standard-output*))))



(defclass channel ()
  ((id :initarg :id :reader channel-id)
   (name :initarg :name :reader name)
   (trainers :initarg :trainers :accessor trainers)))

(defclass packet ()
  ((contents :initarg :contents :reader contents)
   (command-id :allocation :class :reader command-id)))

(defmethod print-object ((obj packet) s)
  (print-unreadable-object (obj s :type t)
    (format s "#~A ~A" (command-id obj) (contents obj))))
(defclass channel-list-packet (packet)
  ((command-id :initform 44 :allocation :class)))

(defclass channel-id-mixin ()
  ((channel-id :initarg :channel-id :reader channel-id :type '(unsigned-byte 32)
               :documentation "Numeric identifier cooresponding to a channel name.")))

(defclass channel-players-packet (packet channel-id-mixin)
  ((command-id :initform 45 :allocation :class)))

(defun to-flexi-stream (s &key (external-format :utf-16))
  (flexi-streams:make-flexi-stream s :external-format (flexi-streams:make-external-format external-format)))

(defun connect (host port &rest args &key (nickname "Shanai"))
  "Create a socket connected to HOST on PORT."
  (declare (ignore nickname))
  (let ((sock (apply #'change-class
                     (usocket:socket-connect host port
                                             :element-type '(unsigned-byte 8))
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
                   :info (read-po-octet-string s) :losing-msg (loop for i = (read-byte s nil) while i collect i) :winning-msg nil
                   ;:info (read-po-octet-string s) :losing-msg (read-po-octet-string s)
    ;               :winning-msg (read-po-octet-string s)
                   )
    ))

(defclass trainer ()
  ((user-id :initarg :user-id :reader user-id)
   (name :initarg :name :reader name)
   (info :initarg :info :reader info)
   (losing-msg :initarg :losing-msg :reader losing-msg)
   (winning-msg :initarg :winning-msg :reader winning-msg)))

(defmethod print-object ((obj trainer) s)
  (print-unreadable-object (obj s)
    (format s "~A #~A ~S loss-msg: ~S win-msg: ~S"
            (name obj) (user-id obj) (info obj)
            (losing-msg obj) (winning-msg obj))))

(defclass join ()
  ((channel-id :initarg :channel-id :reader channel-id)
   (user-id :initarg :user-id :reader user-id)))

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

(defun split-at-first (item sequence)
  (let ((pos (position item sequence)))
    (if pos
        (cons (subseq sequence 0 pos)
              (subseq sequence (1+ pos)))
        (cons sequence
              nil))))


(defun commandp (con msg-string)
  (not (not (find #\, msg-string :end 1))))



(defmethod handle-command (cmd (con connection) (msg message))
  (reply con msg "Sorry I don't know about that one."))
(defun handle-command% (con msg)

  (when (commandp con (cdr (parse-nickname-and-message msg)))
    (let ((cmd (split-at-first #\ (cdr (parse-nickname-and-message msg)))))
      (when cmd
        (handle-command (intern (string-upcase (subseq (car cmd) 1)) :keyword)

                        con msg)
        ))))

(defun parse-nickname-and-message (msg)
  (if (typep msg 'private-message)
      (let ((cmd (split-at-first #\ (message msg))))
        (cons nil (message msg)))
      (let ((cmd (split-at-first #\ (message msg))))
        (setf (car cmd) (subseq (car cmd) 0 (1- (length (car cmd)))))
        cmd)))