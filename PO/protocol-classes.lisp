(in-package :shanai.po.protocol-classes)

(defclass po-event ()
  ((recv-time :initform (get-universal-time)
              :documentation "Time of message reception.")
   (connection :initform (global:current-connection)
               :documentation "Connection object associated with event.")))

(defclass channel-id-mixin ()
  ((channel-id :initarg :channel-id)))

(defclass keep-alive (po-event) ())
(defclass abstract-send-message (po-event)
  ((message :initarg :message)))
(defclass send-message (abstract-send-message) ())
(defclass html-message (abstract-send-message) ())
(defmethod generic:message ((msg abstract-send-message))
  (slot-value msg 'message))

(defclass battle-finished (po-event)
  ((id :initarg :id)
   (outcome :initarg :outcome)
   (winner :initarg :winner)
   (loser :initarg :loser)))

(defclass away (po-event)
  ((id :initarg :id
       :documentation "Player's identifier")
   (awayp :initarg :awayp :type 'boolean)))

(defclass join-channel (po-event channel-id-mixin)
  ((user-id :initarg :user-id)))

(defclass leave-channel (po-event channel-id-mixin)
  ((user-id :initarg :user-id)))

(defclass abstract-channel-message (po-event)
  ((channel :initarg :channel)
   (message :initarg :message)))

(defmethod generic:message ((msg abstract-channel-message))
  (slot-value msg 'message))
(defmethod generic:object-id ((msg abstract-channel-message))
  (slot-value msg 'channel))
(defclass channel-message (abstract-channel-message) ())
(defclass channel-html (abstract-channel-message) ())
