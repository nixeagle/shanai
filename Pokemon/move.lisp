(in-package :pokemon)

(deftype move-categories ()
  "Possible categories for pokemon move."
  '(member :physical :status :special))

(deftype valid-move-number ()
  "Range of valid numbers for Generation V pokemon games."
  '(integer 1 559))

(defclass move ()
  ((name :initarg :name :type 'string :reader name)
   (number :initarg :number :type 'valid-move-number :reader number)
   (type :initarg :type :reader poketype :reader move-type)
   (learning)                           ; How the move is learned...
   (pp :initarg :pp :reader pp)
   (priority :initarg :priority :reader priority)
   (effect-description :initarg :effect-description :reader effect-description)
   (effect-fun :initarg :effect-fun :reader effect-fun)
   (power :initarg :power :reader power :reader move-power)
   (effect :initarg :effect :reader effect)
   (accuracy :initarg :accuracy :reader accuracy)
   (effect-accuracy :initarg :effect-accuracy :reader effect-accuracy)
   (category :initarg :category :reader category)
   (description :initarg :description :reader description)
   (critical-rate :initarg :critical-rate :reader critical-rate)
   (recoil :initarg :recoil :reader recoil)
   (range :initarg :range :reader range)
   (max-turns :initarg :max-turns :reader max-turns)
   (min-turns :initarg :min-turns :reader min-turns)
   (min-max-hits :initarg :min-max-hits :reader min-max-hits)
   (healing :initarg :healing :reader healing)
   (status-kind :initarg :status-kind :reader status-kind)
   (damage-class :initarg :damage-class :reader damage-class)
   (stat-modifier-chances :initarg :stat-modifier-chances :reader stat-modifier-chances)
   (stat-modifier :initarg :stat-modifier :reader stat-modifier)
   (flags :initarg :flags :reader flags
          :documentation "See the flags on veekun's pokedex.

For example FlyingFlag is for gravity blocking and SubstituteFlag is for
moves that bypass substitute such as Taunt. PO's C++ enumeration is
           enum Flags     {         ContactFlag = 1,
             ChargeFlag = 2,         RechargeFlag = 4,
             ProtectableFlag = 8,         MagicCoatableFlag = 16,
             SnatchableFlag = 32,         MemorableFlag = 64,
             PunchFlag = 128,         SoundFlag = 256,
             FlyingFlag = 512,         UnthawingFlag = 1024,
             PulsingFlag = 2048,         HealingFlag = 4096,
             Substitute = 8192     };")
   (flinch-chance :initarg :flinch-chance :reader flinch-chance)
   (generation :initarg :generation :type 'valid-generation-number :reader generation))
  (:documentation "Description of a pokemon move."))


(defmethod generic:type1 ((move move))
  (alexandria:make-keyword (slot-value move 'type)))

(defmethod generic:object-id ((move move))
  (slot-value move 'number))



(defmethod print-object ((obj move) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A named ~A: POW ~A ACC ~A ~A"
            (category obj) (poketype obj)
            (name obj) (power obj) (accuracy obj)
            (pp obj))))