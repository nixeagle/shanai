(in-package #:shanai.battle)

(defclass battle-trainer (shanai.po.client::basic-trainer
                          shanai.team::basic-team)
  ())


(defclass basic-battle ()
  ((challenger :initarg :challenger
               :accessor battle-challenger)
   (challenged :initarg :challenged
               :accessor battle-challenged)
   (tier :initarg :tier :reader battle-tier
         :type 'string)
   (gen :initarg :gen :reader battle-gen)
   (turn :initarg :turn :accessor battle-turn
         :initform 0)))

(defmethod generic:gen ((thing basic-battle))
  (slot-value thing 'gen))

(defmethod generic:challenger ((thing basic-battle))
  "Get challenging entity."
  (slot-value thing 'challenger))

(defmethod generic:challenged ((thing basic-battle))
  "Get challenged entity."
  (slot-value thing 'challenged))

(defmethod generic:tier ((thing basic-battle))
  "Get battle's tier."
  (slot-value thing 'tier))

(defun !incf-turn (battle)
  "Increase the BATTLE's turn by one."
  (declare (type basic-battle battle))
  (incf (slot-value battle 'turn)))