;;; Pokemon Online Trainer information.
;;; Trainers are the term used for users on Pokemon Online servers.

(in-package :shanai.po.client)

(defclass trainer ()
  ((id :initarg :user-id :reader user-id)
   (name :initarg :name :reader name)
   (info :initarg :info :reader info)
   (losing-msg :initarg :losing-msg :reader losing-msg)
   (winning-msg :initarg :winning-msg :reader winning-msg)))
