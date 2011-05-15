(in-package :pokemon)

(defparameter +total-moves+ 560 "Total number of known moves in the PO database.")

(defparameter *movedex*
  (make-array +total-moves+ :element-type 'move
              :initial-contents (loop for i from 0 to (1- +total-moves+)
                                   collect (make-instance 'move)))
  "Global in memory database of all possible pokemon moves.

For now we are restricting this to generation 5 moves only.")

(defparameter +damage-classes+'(:other :physical :special)
  "Pokemon has 3 different 'types' of attack damage.

Physical is for things that make contact, special is for things that don't
   like most ghost moves. Finally OTHER is for moves that inflict
   primarally status effects.")

(defparameter +status-kind+ '(:no-kind :simple-kind :turn-kind :attract-kind :wrap-kind)
  "How does the status effect mechanic work when the move hits and inflicts
status effects. Based off of the following PO enum:

enum StatusKind { NoKind = 0, SimpleKind = 1,
                  TurnKind = 2, AttractKind = 3, WrapKind = 4
};")

(defparameter +move-range-enumeration+
  '(:chosen-target :partner-or-user :partner :me-first-target :all-but-self :opponents :team-party :user :all :random-target :field :opposing-team :team-side :indeterminate-target)
  "Names for the corresponding range ids.

Ordering is critical in this list.")

(defparameter +move-categories+
  '(:standard-move :status-inducing-mvoe :stat-changing-move :healing-move
    :offensive-status-inducing-move :stat-and-status-move
    :offensive-stat-changing-move :offensive-self-stat-changing-move
    :absorbing-move :ohko-move :field-move :team-zone-move :roaring-move
    :special-move)
  "List of various broad move categories.

Ordering is important!")

(defparameter +status+
  '(:unusual :fine :paralysed :asleep :frozen :burnt :poisoned :confused :attracted
    :wrapped :nightmared :skip10 :skip11 :tormented :disabled :drowsy :heal-blocked :sleuthed :seeded :embergood :requiemed :rooted)
  "List of status conditions that can be inflicted on a pokemon in PO.

Ordering is important! Also based off of the following PO enum:

enum Status { Fine = 0, Paralysed = 1,
              Asleep = 2,  Frozen = 3,  Burnt = 4,
              Poisoned = 5, Confused = 6, Attracted = 7,
              Wrapped = 8, Nightmared = 9, Tormented = 12,
              Disabled = 13,  Drowsy = 14, HealBlocked = 15,
              Sleuthed = 17, Seeded = 18, Embargoed = 19, 
              Requiemed = 20, Rooted = 21")


(defclass stat-modifier ()
  ((hp :initarg :hp :reader hp)
   (attack :initarg :attack :reader attack)
   (defense :initarg :defense :reader defense)
   (special-attack :initarg :special-attack :reader special-attack)
   (special-defense :initarg :special-defense :reader special-defense)
   (speed :initarg :speed :reader speed)
   (accuracy :initarg :accuracy :reader accuracy)
   (evasion :initarg :evasion :reader evasion))
  (:documentation "In battle stat modifiers. Ranges from -6 to 6.")
  (:default-initargs :hp 0 :attack 0 :defense 0 :special-attack 0
                      :special-defense 0 :speed 0 :accuracy 0 :evasion 0))

(defmethod print-object ((obj stat-modifier) stream)
  "Print out unreadable stat modifier info."
  (print-unreadable-object (obj stream :type t)
    (format stream ":hp ~A :atk ~A :def ~A :sp-atk ~A :sp-def ~A :spd ~A :acc ~A :eva ~A"
            (hp obj) (attack obj) (defense obj) (special-attack obj)
            (special-defense obj) (speed obj) (accuracy obj) (evasion obj))))

;;; Utility function that we need for parsing stat boost information
(defun twos-complement (num size)
  "Change an unsigned NUM to a signed number representation."
  ;; This exists because we read in unsigned octets of information, but we
  ;; wish to treat 255 as -1 and so forth.
  ;; TODO: 128 should be changed to -128... or something! Kinda loopy due to
  ;; backpain, so check this later!
  (if (<= (/ size 2) num)
      (- (mod size num))
      num))

(defun parse-stat-modifier-boosts (encoded-stat-number)
  "Fish out the 3 modifier rates for a move's stat modifiers."
  (loop for b from 23 downto 0 by 8
     collect (twos-complement (ldb (byte 8 (- b 7)) encoded-stat-number) 256)))

(defun parse-stat-modifier-chance (encoded-stat-number)
  (loop for b from 23 downto 0 by 8
       collect (ldb (byte 8 (- b 7)) encoded-stat-number)))

(defun parse-stat-modifier-target (encoded-stat)
  "Based off of the PO C++ enumeration.

enum Stat { Hp = 0,     Attack = 1,     Defense = 2,
            SpAttack = 3,     SpDefense = 4,     Speed = 5,
            Accuracy = 6,     Evasion = 7,     AllStats = 8 };"
  (loop for b from 23 downto 0 by 8
     collect (case (ldb (byte 8 (- b 7)) encoded-stat)
               (0 :hp)
               (1 :attack)
               (2 :defense)
               (3 :special-attack)
               (4 :special-defense)
               (5 :speed)
               (6 :accuracy)
               (7 :evasion)
               (8 :all))))


(defun load-movedex (&optional (po-directory *po-directory*))
  "Load all the moves from PO-DIRECTORY."
  (declare (type string po-directory))
  (let ((*po-directory* po-directory))
    (macrolet ((with-move ((filename line i) &body body)
                 `(with-open-file (s (make-po-data-filepath ,filename) :direction :input)
                    (loop for ,line = (parse-integer-or-string (read-line s nil))
                       while ,line
                       for ,i from 0 to (1- +total-moves+)
                       do (reinitialize-instance (svref *movedex* ,i) ,@body)))))
      (with-move ("moves/moves.txt" line i) :name line :number i)
      (with-move ("moves/5G/type.txt" line i) :type (nth line *typedex*))
      (with-move ("moves/5G/pp.txt" line i) :pp line)
      (with-move ("moves/5G/power.txt" line i) :power line)
      (with-move ("moves/5G/accuracy.txt" line i) :accuracy line)
      (with-move ("moves/5G/crit_rate.txt" line i) :critical-rate line)
      (with-move ("moves/5G/priority.txt" line i) :priority line)
      (with-move ("moves/5G/effect_chance.txt" line i) :effect-accuracy line)
      (with-move ("moves/5G/effect.txt" line i) :effect-description line)
      (with-move ("moves/5G/recoil.txt" line i) :recoil line)
      (with-move ("moves/5G/range.txt" line i) :range (nth line +move-range-enumeration+))
      (with-move ("moves/5G/category.txt" line i) :category (nth line +move-categories+))
      (with-move ("moves/5G/caused_effect.txt" line i) :effect (nth (1+ line) +status+))
      (with-move ("moves/5G/flinch_chance.txt" line i) :flinch-chance line)
      (with-move ("moves/5G/healing.txt" line i) :healing line)
      (with-move ("moves/5G/min_max_hits.txt" line i) :min-max-hits line)
      (with-move ("moves/5G/max_turns.txt" line i) :max-turns line)
      (with-move ("moves/5G/min_turns.txt" line i) :min-turns line)
      (with-move ("moves/5G/status.txt" line i) :status-kind (nth line +status-kind+))
      (with-move ("moves/5G/flags.txt" line i) :flags line)
      (with-move ("moves/5G/damage_class.txt" line i) :damage-class (nth line +damage-classes+))
      (with-move ("moves/move_description.txt" line i) :description line)
      (with-move ("moves/5G/None2.txt" line i) :stat-modifier-chances (car (parse-stat-modifier-chance line)))
      (with-move ("moves/5G/None0.txt" line i) :stat-modifier (parse-stat-modifier-target line))
      (with-move ("moves/5G/None1.txt" line i) :stat-modifier
                 (flet ((all-values (value)
                          "Every stat is the same VALUE."
                          (list :hp value :attack value
                                :defense value :special-attack value
                                :special-defense value :speed value
                                :accuracy value :evasion value)))
                   (apply #'make-instance 'stat-modifier
                          (iter (for key :in (stat-modifier (svref *movedex* i)))
                                (for value :in (parse-stat-modifier-boosts line))
                                (unless (= 0 value)
                                  (if (eq :all key)
                                      (appending (all-values value))
                                      (appending (list key value)))))))))))

(defun find-move (name-or-id)
  "Look up a move by NAME-OR-ID."
  (declare (type (or fixnum string) name-or-id))
  (the (or move null)
    (typecase name-or-id
      (fixnum (and (< 0 name-or-id +total-moves+) (aref *movedex* name-or-id)))
      (string (find name-or-id *movedex* :test #'string-equal :key #'name)))))