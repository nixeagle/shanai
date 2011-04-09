

(in-package :pokemon)

(eos:def-suite :pokemon)




(defun probability-of-hit (base-accuracy accuracy-of-user evasion)
  "Accuracy calculations.

Returns the percentage chance that a particular hit will occur."
  (* base-accuracy (/ accuracy-of-user evasion)))


(defclass pokemon ()
  (;(shinyp :initarg :shinyp :type 'boolean :readers (shinyp))
   (number :initarg :number :type 'fixnum :readers (number))
   (name :initarg :name :type 'string :readers (name))
   (nickname :initarg :nickname :type 'string :accessor nickname)
   (type :initarg :type :reader poketype) ; :type should be a LIST or single type.
   ;(species :initarg :species :readers (species))
   (abilities :initarg :abilities :readers (abilities))
   ;(lv100exp :initarg :lv100exp :type 'fixnum)
;   (height :initarg :height :type 'fixnum) ; units are tenths of a meter.
   (weight :initarg :weight :type 'fixnum :reader weight) ; units are tenths of a kilogram.
   ;(catch-rate :initarg :catch-rate :type 'fixnum :readers (catch-rate))
   ;(gender-ratio :initarg :gender-ratio)
   ;(breeding :initarg :breeding)
   ;(ev-yield :initarg :ev-yield)
   (ev :initarg :ev :accessor ev)
   ;(base-exp :initarg :base-exp)
   ;(battle-exp :initarg :battle-exp)
   (base-stats :initarg :base-stats :type 'battle-statistics :accessor base-stats)
   (moves :initarg :moves :accessor moves)
   (type-effectiveness :initarg :type-effectiveness)
   (nature :initarg :nature :accessor nature)
   (level :initarg :level :type '(integer 0 100) :reader level)
   (item :initarg :item); what item is it holding?
   (gender :initarg :gender)
   (ability :initarg :ability)
   (dv :initarg :dv :type 'battle-statistics :accessor dv)
   (happiness :initarg :happiness) ; only has effect on moves.
   #+ () (steps-walked-with :initarg :steps-walked-with :accessor steps-walked-with
                      :documentation "Total number of steps this pokemon
   has been in the active party of the player."
                      :type 'fixnum)))

(defclass battle-pokemon ()
  ((base-pokemon :initarg :base-pokemon :type 'base-pokemmon)
   (leechedp :initarg :leeched :accessor leechedp)
   (confusedp :initarg :confusedp :accessor confusedp)
   (hp-remaining :initarg :hp-remaining :type 'fixnum :readers (hp-remaining))
   (status :initarg :status :accessor status)
   (attractedp :initarg :attractedp :accessor attractedp :type 'boolean)
   (stat-boosts :initarg :stat-boosts :accessor stat-boosts)
   (physical-stats :initarg :ps :accessor physical-stats :type 'battle-statistics)
   (flinchp :initarg :flinchp :accessor flinchp :type 'boolean)
   (user-switch-p :initarg :user-switch-p :accessor user-switch-p
            :documentation "The condition that indicates if the pokemon can
            be changed out by the user or not.")
   (opponent-switch-p :initarg :opponent-switch-p :accessor opponent-switch-p
                  :documentation "The condition that indicates if the
            pokemon can be changed out by the AI or not."))
  (:documentation "Additional information about POKEMON needed for battles."))

(defclass battle-statistics ()
  ((hp :name :hp :initarg :hp :type 'fixnum :readers (hp))
   (attack :initarg :attack :type 'fixnum :readers (attack))
   (defense :initarg :defense :type 'fixnum :readers (defense))
   (special-attack :initarg :special-attack :type 'fixnum :readers (special-attack))
   (special-defense :initarg :special-defense :type 'fixnum :readers (special-defense))
   (speed :initarg :speed :type 'fixnum :readers (speed))))

(defun pokemon-stats (atk def sp-atk sp-def spd)
  "Create an instance of BATTLE-STATISTICS."
  (battle-stats 0 atk def sp-atk sp-def spd))

(defmethod print-object ((obj battle-statistics) stream)
  "Print details of a BATTLE-STATISTICS object."
  (print-unreadable-object (obj stream :type :t)
    (princ (list (attack obj) (defense obj)
                 (special-attack obj) (special-defense obj)
                 (speed obj) (hp obj)) stream)))

;;; Custom generations will probably break this...
(deftype valid-generation-number ()
  "Range of valid pokemon generations.

Only released generations are I, II, III, IV, and V. So this corresponds to
  a range of 1 to 5."
  '(integer 1 5))



;;; Functions for manipulating an in memory movedex.
(defmacro defmove (name description &rest r)
  "Define and intern a new move.")



#+ () (defclass poketype () ()
  (:documentation "Base class for pokemon types."))

#+ () (defmethod immunep ((attacking-type poketype) (defending-type poketype))
  "True if ATTACKING-TYPE does not affect DEFENDING-TYPE.")

(defun calc-damage (level bp attack opp-defense)
  (+ (floor (/ (* (floor (+ (* level 2/5) 2)) bp attack) (* 50 opp-defense)))
     2))


(defun make-gen-ds-pokemon (dex-no id nature lvl-caught lvl-current exp pokestats ability)
  )

(defun my-pokemon (dex-no nature lvl-caught lvl-current exp pokestats ability)
  (make-gen-ds-pokemon dex-no 21278 nature lvl-caught lvl-current exp pokestats ability))

(defmacro my-pokemon* (dex-no nickname nature lvl-caught ability day month year &rest stats)
  `(make-gen-ds-pokemon ,dex-no 21278 ,nature ,lvl-caught nil nil nil ,ability))



(defun _/_ (num den)
  "Divide NUM by DEN and flooring the result."
  (floor (/ num den)))

(defun _*_ (&rest numbers)
  (flet ((mul (a b)
           (floor (* a b))))
    (reduce #'mul numbers)))

;;; pokemon damage calculations.
;;;
;;; Functions that follow are based on rather complex mathematical formulas
;;; specified on various opkemon fan sites. Thus the code is not going to
;;; be pretty. However effort has been done to test these functions to
;;; verify that all the outputs are correct given certain inputs.
(defun damagecalc (level move-power attack defense mod1 critical-hit mod2 r stab type1 type2 mod3)
  "Compute how much damage an attack does."
  (_*_ (_/_ (_*_ (+ (_*_ (_/_ (_/_ (_*_ (+ (_/_ (_*_ level 2) 5) 2) move-power attack)
                                           50) defense)
                                 mod1) 2) critical-hit mod2 r) 100) stab type1 type2 mod3))

(test (damagecalc :suite :pokemon)
  "Verify DAMAGECALC gives correct output for known correct example."
  (is (= 114 (damagecalc 47 60 140 77 1 1 1 85 1.5 2 1 1)))
  (is (= 134 (damagecalc 47 60 140 77 1 1 1 100 1.5 2 1 1))))

(defun calculate-power (move-base-power user-ability foe-ability item-multiplier helping-hand-p
                        chargep mud-sport-p water-sport-p)
  (_*_ helping-hand-p move-base-power item-multiplier chargep mud-sport-p
       water-sport-p user-ability foe-ability))

(defun calculate-attack (stat stat-modifier ability-modifier item-modifier)
  (_*_ stat stat-modifier ability-modifier item-modifier))

(defun compute-attack-stat-modifier (number)
  (declare (type (integer -6 6) number))
  (if (>= number 0)
      (/ (+ 2 number) 2)
      (/ 2 (- 2 number))))

(test (compute-attack-stat-modifier :suite :pokemon)
  "Must match table at http://www.smogon.com/dp/articles/damage_formula#atk_stat"
  (is (= 2/8 (compute-attack-stat-modifier -6)))
  (is (= 2/3 (compute-attack-stat-modifier -1)))
  (is (= 2/2 (compute-attack-stat-modifier 0)))
  (is (= 3/2 (compute-attack-stat-modifier 1)))
  (is (= 8/2 (compute-attack-stat-modifier 6))))

(defun calculate-defense (stat stat-modifier sx extra-modifier)
  "Compute defending pokemon's defense value.

Docs: http://www.smogon.com/dp/articles/damage_formula#defense"
  (_*_ stat stat-modifier sx extra-modifier))

(defun compute-modifier1 (burn reflect-lightscreen 2v2-modifier sunny-day-rain-dance flash-fire)
  "Compute value of first modifier to pokemon damage calculations.

Docs: http://www.smogon.com/dp/articles/damage_formula#mod1"
  (_*_ burn reflect-lightscreen 2v2-modifier sunny-day-rain-dance flash-fire))

(defun compute-modifier3 (solid-rock-filter expert-belt tinted-lens type-resisting-berry)
  (_*_ solid-rock-filter expert-belt tinted-lens type-resisting-berry))

(defun random-battle-number ()
  "Random number generator for simulating attack damage."
  (/ (_/_ (+ (random 38) 218) 2.55) 100))

(defun same-type-attack-bonus-p (pokemon-type move-type1)
  (eq pokemon-type move-type1))

;;; Notice that 3 values are known to the player, but we don't know the defense value.
(defun simple-damagecalc-min-max (level move-power attack defense)
  "Simplistic damage calculation using only 4 variables to yeild min/max."
  (list (list (damagecalc level move-power attack defense 1 1 1 85 1 1 1 1)
              (damagecalc level move-power attack defense 1 1 1 100 1 1 1 1))
        (list (damagecalc level move-power attack defense 1 2 1 85 1 1 1 1)
              (damagecalc level move-power attack defense 1 2 1 100 1 1 1 1))))





;;; ok so let us attempt to encorporate critical hits into our simple damage
;;; calculator.


(defun critical-hit-stage-to-percentage (stage-number)
  "Given a STAGE-NUMBER indicate which ratio applies for critical hits."
  (declare (type (integer 1 6) stage-number))
  (case stage-number
    (1 1/16)
    (2 1/8)
    (3 1/4)
    (4 1/3)
    (5 1/2)
    (6 1)))

(defun bst (hp atk def spatk spdef speed)
  "Compute the base statistics total given HP ATK DEF SPATK SPDEF and SPEED."
  (+ hp atk def spatk spdef speed))

(test (bst :suite :pokemon)
  "Must be the sum of all the base sstatistics."
  ;; Base statistics for Celebi according to zeroality.
  (is (= 600 (bst 100 100 100 100 100 100))))

(defun create-pokemon (level dv-stats move-power)
  ;; For now is mean to create a very simple pokemon.
  ;; To do so we ignore most of the attributes that pokemon have in favor of
  ;; modeling only obvious/simplistic 
  (make-instance 'pokemon :level level :dv dv-stats :moves move-power))

(defun create-battle-pokemon (level dv-stats move-power)
  ;; For now is mean to create a very simple battle-pokemon.
  ;; To do so we ignore most of the attributes that pokemon have in favor of
  ;; modeling only obvious/simplistic 
  (make-instance 'battle-pokemon :level level :dv dv-stats :moves move-power
                 :hp-remaining (hp dv-stats)))

(defmethod print-object ((obj battle-pokemon) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((stats (dv (base-pokemmon obj))))
      (format stream "LvL: ~A HP: ~A/~A ATK: ~A DEF: ~A SP-ATK ~A SP-DEF ~A SPD ~A
    ~A
    ~A
    ~A
    ~A"
              (level obj) (hp-remaining obj)
              (hp stats) (attack stats) (defense stats)
              (special-attack stats) (special-defense stats)
              (speed stats)
              (car (moves obj))
              (cadr (moves obj))
              (caddr (moves obj))
              (cadddr (moves obj))))))

(defun battle-stats (hp attack defense special-attack special-defense speed)
  "Helper function for creating instances of BATTLE-STATISTICS."
  (make-instance 'battle-statistics
                 :hp hp :attack attack :defense defense :special-attack special-attack
                 :special-defense special-defense :speed speed))


(defun do-battle (poke1 poke2)
  (declare (type battle-pokemon poke1 poke2))
  (list poke1 poke2))

(deftype battle-box ()
  "List of 1 to 6 BATTLE-POKEMON being brought along for a fight."
  '(or (cons battle-pokemon null)
    (cons battle-pokemon (cons battle-pokemon null))
    (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon null)))
    (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon null))))
    (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon null)))))
    (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon (cons battle-pokemon null))))))))

(defclass player ()
  ((battle-team :initarg :team :type 'battle-box :accessor battle-team)
   (name :initarg :name :type 'string :reader name))
  (:documentation "Pokemon trainer information"))

(defclass battle-player (player)
  ((entry-hazards :initarg :entry-hazards :accessor entry-hazards)
   (exit-hazards :initarg :exit-hazards :accessor exit-hazards)))

(defclass battle-configuration ()
  ())
(defclass team-battle ()
  ())

(defclass poke-battle ()
  ())

(defun execute-move (move)
  ;; Execute a particular move by poke1.
  )



(defun pokemon-population-count ()
  "Return the total number of pokemon. Base + all forms.")

(defun pokemon-visible-population-count ()
  "Return the total number of visible pokemon. Base + all forms.")


(defgeneric type1 (pokemon generation))
(defgeneric type2 (pokemon generation))
;(defgeneric moves (pokemon generation))
(defgeneric egg-moves (pokemon generation))
(defgeneric level-moves (pokemon generation))
(defgeneric tutor-moves (pokemon generation))
(defgeneric tm-moves (pokemon generation))
(defgeneric pre-evo-moves (pokemon generation))
(defgeneric special-moves (pokemon generation))
(defgeneric regular-moves (pokemon generation))
(defgeneric dream-world-moves (pokemon generation))

(defgeneric number-of-formes (pokemon generation))

;(defgeneric a-forme-shown)

(defgeneric formep (pokemon generation))

(defgeneric aestheticp (pokemon generation))

(defgeneric non-aesthetic-forme (pokemon generation))
(defgeneric original-forme (pokemon generation))

(defgeneric has-formes-p (pokemon generation))

(defgeneric formes (pokemon generation))

(defgeneric visible-formes (pokemon generation))

(defgeneric evos (pokemon generation))
(defgeneric direct-evos (pokemon generation))

(defgeneric has-eveloutions-p (pokemon generation))

(defgeneric original-evolution (pokemon generation))
(defgeneric pre-evolution (pokemon generation))
(defgeneric in-evolution-chain-p (pokemon generation))
(defgeneric base-stats (pokemon))
(defgeneric exists (pokemon generation))
;(defgeneric abilities (pokemon generation))
;Stat+Fullstat are what exactly!

; Should not return Missingno
(defun random-pokemon (generation))



(defgeneric sleep-clause-p (battle)
  (:documentation "Is the sleep clause turned on?

In PO, the sleep clause means that only one pokemon may be asleep on a team
at one time."))



;;; around roughly line 86 battle.h
(defgeneric team (player))
(defgeneric battle-team (player))
(defgeneric (setf battle-team) (player value))


(defgeneric poke (battle player poke)
  ;; poke seems to be an "optional" argument in the C++ source.
  )

(defgeneric random-opponent (battle slot))
(defgeneric random-valid-opponent (battle slot))

(defgeneric slot (battle player poke))

(defgeneric can-target-p (battle attack attacker defender))
(defgeneric are-adjacent-p (battle attacker defender))
(defgeneric multiplesp (battle))

(defgeneric are-partners-p (battle player1 player2))

(defgeneric battle-log-filename (battle))

(defgeneric turn (battle))
(defgeneric (setf turn) (battle value)
  (:documentation "Set a new non-negative-fixnum VALUE for turn in BATTLE>"))
(defgeneric players (thing))
(defgeneric (setf players) (thing value))

(defgeneric sort-by-speed (battle))
(defgeneric weather (thing))
(defgeneric (setf weather) (thing value))

(defgeneric name (thing)
  (:documentation "Return the name of THING as a string."))

(defgeneric object-id (thing)
  ;; Probably not useful in common lisp but who knows!
  (:documentation "Integer identifier of THING."))

(defgeneric player1 (thing))
(defgeneric player2 (thing))


(defclass battle ()
  ((players :documentation "Player list."
             :initarg :players
             :accessor players)
   (turn :documentation "Current turn number."
         :type 'non-negative-fixnum
         :initarg :turn :accessor turn)
   (trick-room-p :initarg :trick-room-p :accessor trick-room-p)
   (gravityp :initarg :gravityp :accessor gravityp)
   (wonder-room-p :initarg :wonder-room-p :accessor wonder-room-p)
   (magic-room-p :initarg :magic-room-p :accessor magic-room-p)
   (battlelog :documentation "Log of all actions during this battle."
              :initarg :battlelog)
   (weather :documentation "Current weather effect on the battle."
            :initarg :weather :accessor weather))
  (:default-initargs :turn 0 :weather :normal-weather))

(deftype spot ()
  "What position is the player in during a battle?

:player1 corresponds to 0, :player2 corresponds to 1 and :spectator corresponds to -1."
  '(member :spectator :player1 :player2))

(defun make-ai-player (&key name)
  (make-instance 'player :name name :team (list (create-battle-pokemon 5 (battle-stats 21 9 6 10 8 12) 50))))

(defun make-test-battle ()
  (make-instance 'battle :players (list (make-instance 'player :name "AI" :team (list (create-battle-pokemon 5 (battle-stats 21 9 6 10 8 12) 50)))
                                        (make-ai-player :name "AI-OPP"))))

(defparameter *battle* nil
  "Current 'battle', use let bindings with this, not setf.")

(defparameter *last-battle* nil
  ;; For testing purposes only.
  "Ok to use setf with this, for the prior simulated battle.")

(defun main ()
  "Calling this 'main' for lack of a better idea."
  (let ((*battle* (make-test-battle)))
    (setf *last-battle* *battle*)
  ))


(defmethod player1 ((obj battle))
  "First player of 2 in a pokemon battle."
  (first (players obj)))

(defmethod player2 ((obj battle))
  "Second player of 2 in a pokemon battle."
  (second (players obj)))

(defun pprint-detailed-battle-data (battle)
  (format nil "Battle: Turn #~A Weather: ~A
  P1: ~A        P2: ~A"
          (turn battle)
          (weather battle)
          (name (player1 battle))
          (name (player2 battle))))

(defun analyze-attack-damage (battle attacker defender move)
  (simple-damagecalc-min-max (level (first (battle-team attacker)))
                             (power move) (attack (dv (first (battle-team attacker))))
                             (defense (dv (first (battle-team defender))))))

(defun import-csv-as-list (filepath)
  (with-open-file (s filepath :direction :input)
    (read-line s nil);; ignore the header line
    (loop for line = (read-line s nil)
       while line collect
         (let ((sp (split-sequence #\, line)))
           (list (parse-integer (car sp)) (cdr sp))))))

(defparameter *pokedex* (make-hash-table :test #'eq)
  "Global database of pokemon data.

Indexed by the pokemon's ID in the national pokedex.")

(defparameter *typedex* (make-hash-table :test #'eq)
  "Global database mapping typeids to type names.")

(defparameter +pokemon-types+
  '(:normal :fighting :flying :poison :ground :rock :bug :ghost
    :steel :fire :water :grass :electric :psychic :ice :dragon :dark :???)
  "Listing of all the types in pokemon.

Order is important!")

(deftype poketype ()
  "Listing of the 17 valid pokemon types in Generation 5."
  '(member :bug :dark :dragon :electric :fighting :fire :flying :ghost
    :grass :ground :ice :normal :poison :psychic :rock :steel :water :???))


(defmethod print-object ((obj pokemon) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((stats (base-stats obj)))
      (format stream "~A HP: ~A ATK: ~A DEF: ~A SP-ATK ~A SP-DEF ~A SPD ~A"
              (name obj) (hp stats) (attack stats) (defense stats)
              (special-attack stats) (special-defense stats)
              (speed stats)))))

(defvar *po-directory* "/home/eagle/pogeymon-online/"
  "Full path to where Pokemon Online source/installation is.

Make sure your file path ends with a trailing slash!")