;;; Pokemon Online Trainer information.
;;; Trainers are the term used for users on Pokemon Online servers.

(in-package :shanai.po.client)

(defclass basic-trainer ()
  ((name :initarg :name :reader trainer-name)
   (pokemon :initarg :pokemon :accessor trainer-pokemon)
   (tier :initarg :tier)
   (gen :initarg :gen)))

(defclass trainer (basic-trainer)
  ((id :initarg :id :reader trainer-id)
   (info :initarg :info)
   (flags :initarg :flags)
   (auth :initarg :auth :reader trainer-server-auth)
   (rating :initarg :rating)
   (avatar :initarg :avatar)
   (color :initarg :color)
   (losing-msg :initarg :losing-msg)
   (winning-msg :initarg :winning-msg)))

(defmethod generic:object-id ((trainer trainer))
  "Server's identifying number for TRAINER."
  (slot-value trainer 'id))

(defmethod generic:name ((trainer basic-trainer))
  "Get TRAINER's current name or handle.

Some also call these nicknames or aliases."
  (slot-value trainer 'name))

(defmethod generic:gen ((trainer basic-trainer))
  "Generation of pokemon this PO user is currently playing with."
  (slot-value trainer 'gen))

(defmethod generic:tier ((trainer basic-trainer))
  "Current tier TRAINER is playing on."
  (slot-value trainer 'tier))

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

;; produces compiler warning about missing function arg, until I know what
;; it is supposed to pass, just leave this commented out as nothing calls
;; it.
#+ () (defun handle-player-send-team (connection trainers-list)
  (setf (get-trainer connection) (reinitialize-trainer-from-packet-list  trainers-list)))



