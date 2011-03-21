(defpackage #:pokemon
  (:use :cl)
  (:import-from :eos #:is #:test))

(in-package :pokemon)

(eos:def-suite :pokemon)

(defclass ability ()
  ((name :name :name :initarg :name :type 'string)
   (number :name :number :initarg :number :type 'fixnum :readers (number))
   (description :name :description :initarg :description :type 'string)
   (generation :name :generation :initarg :generation :type 'fixnum)
   (single :name :single :initarg :single :type 'fixnum)
   (dual :name :dual :initarg :dual :type 'fixnum)
   (hidden :name :hidden :initarg :hidden :type 'fixnum)))


(defun probability-of-hit (base-accuracy accuracy-of-user evasion)
  "Accuracy calculations.

Returns the percentage chance that a particular hit will occur."
  (* base-accuracy (/ accuracy-of-user evasion)))


(defclass pokemon ()
  ((shinyp :initarg :shinyp :type 'boolean :readers (shinyp))
   (number :initarg :number :type 'fixnum :readers (number))
   (name :initarg :name :type 'string :readers (name))
   (type :initarg :type :readers (type)) ; :type should be a LIST or single type.
   (species :initarg :species :readers (species))
   (abilities :initarg :abilities :readers (abilities))
   (lv100exp :initarg :lv100exp :type 'fixnum)
   (height :initarg :height :type 'fixnum) ; units are tenths of a meter.
   (weight :initarg :weight :type 'fixnum) ; units are tenths of a kilogram.
   (catch-rate :initarg :catch-rate :type 'fixnum :readers (catch-rate))
   (gender-ratio :initarg :gender-ratio)
   (breeding :initarg :breeding)
   (ev-yield :initarg :ev-yield)
   (base-exp :initarg :base-exp)
   (battle-exp :initarg :battle-exp)
   (base-stats :initarg :base-stats :type 'battle-statistics)
   (moves :initarg :moves)
   (type-effectiveness :initarg :type-effectiveness)
   (nature :initarg :nature :reader nature)
   (level :initarg :level :type '(integer 0 100) :reader level)
   (item :initarg :item); what item is it holding?
   (gender :initarg :gender)
   (ability :initarg :ability)
   (dv :initarg :dv :type 'battle-statistics :accessor dv)
   (happiness :initarg :happiness) ; only has effect on moves.
   (steps-walked-with :initarg :steps-walked-with :accessor steps-walked-with
                      :documentation "Total number of steps this pokemon
   has been in the active party of the player."
                      :type 'fixnum)))

(defclass battle-pokemon (pokemon)
  ((hp-remaining :initarg :hp-remaining :type 'fixnum :readers (hp-remaining)))
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

(defun make-poke-stats (hp atk def sp-atk sp-def speed)
  ;; DEPRECIATED in favor of BATTLE-STATS
  (battle-stats hp atk def sp-atk sp-def speed))

(defmethod print-object ((obj battle-statistics) stream)
  "Print details of a BATTLE-STATISTICS object."
  (print-unreadable-object (obj stream :type :t)
    (princ (list (attack obj) (defense obj)
                 (special-attack obj) (special-defense obj)
                 (speed obj) (hp obj)) stream)))

(defun walk (pokemon steps)
  "Walk with POKEMON for some number of STEPS."
  (declare (type (integer 0 #.most-positive-fixnum)))
  (setf (steps-walked-with pokemon)
        (+ (steps-walked-with pokemon) steps))
  pokemon)

(deftype valid-move-number ()
  "Range of valid numbers for Generation V pokemon games."
  '(integer 1 559))

(deftype valid-generation-number ()
  "Range of valid pokemon generations.

Only released generations are I, II, III, IV, and V. So this corresponds to
  a range of 1 to 5."
  '(integer 1 5))

(deftype move-categories ()
  "Possible categories for pokemon move."
  '(member :physical :status :special))


(defclass move ()
  ((name :initarg :name :type 'string :reader name)
   (number :initarg :number :type 'valid-move-number :reader number)
   (type :initarg :type :reader poketype)
   (learning) ; How the move is learned...
   (pp :initarg :pp :reader pp)
   (pp-remaining :initarg :pp-remaining :accessor pp-remaining)
   (power :initarg :power :reader power)
   (accuracy :initarg :accuracy :reader accuracy)
   (effect-accuracy :initarg :effect-accuracy :reader effect-accuracy)
   (category :initarg :category :reader category)
   (description :initarg :description :reader description)
   (generation :initarg :generation :type 'valid-generation-number :reader generation))
  (:documentation "Description of a pokemon move."))



;;; Functions for manipulating an in memory movedex.
(defmacro defmove (name description &rest r)
  "Define and intern a new move.")

(defun find-move-by-name (name)
  "Get information about the move called NAME."
  (declare (type string name)))

(defun find-move-by-number (number)
  (declare (type valid-move-number number)))











(defclass weather () ())

(defclass sunlight (weather) ())
(defclass rain (weather) ())
(defclass sandstorm (weather) ())
(defclass hailstorm (weather) ())
(defclass shadow-sky (weather) ())
(defclass fog (weather) ())
(defclass cloudy-sky (weather) ())
(defclass snow (weather) ())

(defclass poketype () ()
  (:documentation "Base class for pokemon types."))

(defmethod immunep ((attacking-type poketype) (defending-type poketype))
  "True if ATTACKING-TYPE does not affect DEFENDING-TYPE.")

(defun calc-damage (level bp attack opp-defense)
  (+ (floor (/ (* (floor (+ (* level 2/5) 2)) bp attack) (* 50 opp-defense)))
     2))

(let* ((s1 (floor (* level 2/5)))
       (s2 (+ s1 2))))


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
    (let ((stats (dv obj)))
      (format stream "LvL: ~A HP: ~A/~A ATK: ~A DEF: ~A SP-ATK ~A SP-DEF ~A SPD ~A"
              (level obj) (hp-remaining obj)
              (hp stats) (attack stats) (defense stats)
              (special-attack stats) (special-defense stats)
              (speed stats)))))

(defun battle-stats (hp attack defense special-attack special-defense speed)
  "Helper function for creating instances of BATTLE-STATISTICS."
  (make-instance 'battle-statistics
                 :hp hp :attack attack :defense defense :special-attack special-attack
                 :special-defense special-defense :speed speed))


(defun do-battle (poke1 poke2)
  (declare (type battle-pokemon poke1 poke2))
  (list poke1 poke2))

