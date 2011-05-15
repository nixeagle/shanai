(in-package :shanai.po.battle)

(defclass battle (basic-battle)
  ((ratedp :initarg :ratedp :reader battle-rated-p)
   (id :initarg :id :reader battle-id)
   (clauses :initarg :clauses :reader clauses)
   (spectators :initarg :spectators :accessor battle-spectators)
   (spectatingp :initarg :spectatingp :reader battle-spectating-p
                :documentation "True when we are spectating a battle, not
                participating in it.")
   (in-progress-p :initarg :in-progress-p :accessor battle-in-progress-p
                  :type 'boolean
                  :documentation "Indicates current status of a battle.

If we are currently fighting this battle out, set this to T.")))

(defmethod generic:object-id ((battle battle))
  (slot-value battle 'id))
(defun get-client-battle-slot-id (battle con)
  "Required for messaging the PO server.

Basically each client is assigned an ID at the start of the battle which is
based on their 'position'. The two trainers in the pokemon battle are
assigned slots 0 and 1 respectively. All spectators are given slot numbers
greater then 1 in sequence."
  (let ((us (po-client:get-trainer "Shanai" con))); Assume we are Shanai.
    (if (equal us (battle-challenged battle))
        0
        1)))