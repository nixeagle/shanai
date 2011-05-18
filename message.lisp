(in-package :pokemon.po.client)

(defclass message ()
  ((message :initarg :message :reader message :type 'string)
   (id :type '(unsigned-byte 32))))

(defmethod generic:object-id ((chan-msg message))
  "Returns u4 value for server's identifier of MESSAGE.

For a channel this is the channel's identifier. For a private message this
is the user's identifier."
  (slot-value chan-msg 'id))

(defclass channel-message (message)
  ((id :initarg :channel-id :reader channel-id)))

(defclass private-message (message)
  ((id :initarg :user-id :reader user-id)))
