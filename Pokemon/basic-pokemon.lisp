(in-package :shanai.pokemon)

(defstruct (stats
             (:constructor make-stats (hp atk def satk sdef spd))
             (:copier)
             (:predicate statsp)
             :named
             (:type vector))
  (hp 0 :type shanai.binary-data:u2)
  (atk 0 :type shanai.binary-data:u2)
  (def 0 :type shanai.binary-data:u2)
  (satk 0 :type shanai.binary-data:u2)
  (sdef 0 :type shanai.binary-data:u2)
  (spd 0 :type shanai.binary-data:u2))

(deftype valid-generation-number ()
  "Range of valid pokemon generations.

Only released generations are I, II, III, IV, and V. So this corresponds to
  a range of 1 to 5."
  '(integer 1 5))

(defclass basic-pokemon ()
  ((id :initarg :id :reader pokemon-id)
   (forme :initarg :forme :reader pokemon-forme)
   (name :initarg :name :type 'string :reader pokemon-name)
   (type :initarg :type :reader pokemon-type)
   (weight :initarg :weight :reader pokemon-weight)
   (base-stats :initarg :base-stats :reader pokemon-base-stats
               :type 'stats)
   (height :initarg :height :reader pokemon-height)
   (gen :initarg :gen :initform 5 :reader pokemon-gen
        :type 'valid-generation-number)))

(defmethod generic:name ((poke basic-pokemon))
  (slot-value poke 'name))
(defmethod generic:forme-id ((poke basic-pokemon))
  (slot-value poke 'forme))

(defmethod generic:type1 ((poke basic-pokemon))
  (alexandria:make-keyword (princ-to-string (car (slot-value poke 'type)))))

(defmethod generic:type2 ((poke basic-pokemon))
  (alexandria:make-keyword (princ-to-string (second (slot-value poke 'type)))))


(defclass pokemon (basic-pokemon)
  ((gender :initarg :gender :reader pokemon-gender)
   (level :initarg :level :reader pokemon-level)
   (nickname :initarg :nickname :reader pokemon-nickname)
   (shinyp :initarg :shinyp :reader pokemon-shiny-p
           :type 'boolean)))

(defclass battle-pokemon (pokemon)
  ((ivs :initarg :ivs :accessor pokemon-ivs)
   (current-hp :initarg :current-hp :accessor pokemon-current-hp)
   (evs :initarg :evs :accessor pokemon-evs)
   (item :initarg :item :reader pokemon-item)
   (ability :initarg :ability :reader pokemon-ability)
   (happiness :initarg :happiness :reader pokemon-happiness)
   (moves :initarg :moves :reader pokemon-moves)
   (status :initarg :status :accessor pokemon-status
           :initform :none)))

(defun pokemon-koedp (poke)
  "True if POKE is knocked out."
  (declare (type battle-pokemon poke))
  (eql (pokemon-status poke) :ko))

(defun !mark-koed (poke)
  "Destructively mark POKE as koed."
  (declare (type battle-pokemon poke))
  (setf (pokemon-status poke) :ko))

