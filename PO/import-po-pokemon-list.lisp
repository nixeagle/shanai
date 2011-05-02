(in-package :cl-user)

(defun parse-pokemon-info (stream)
  (labels ((parse-name (pnameline)
           (cl-ppcre:register-groups-bind (name gender item)
               ("^(.*?)(?:\\s+\\((M|F)\\))?\\s+@\\s+(.*)" pnameline)
             (list (pokemon.po.client::find-pokemon-by-name name) gender item)))
         (parse-trait (line)
           (subseq line 7))
         (parse-evs (line)
           (let ((evs (subseq line 5)))
             (loop for ev in (split-sequence:split-sequence #\/ evs)
                  append (cl-ppcre:register-groups-bind ((#'parse-integer num) type)
                              ("(\\d+)\\s+(\\w+)" ev)
                           (list (alexandria:make-keyword (string-upcase type)) num)))))
         (parse-nature (line)
           (cl-ppcre:register-groups-bind (nature)
               ("^(.*)\\s+Nature" line)
             nature))
         (parse-move (line)
           (subseq line 2))
         (parse-moves (stream &optional moves)
           (if (eq #\- (peek-char nil stream nil nil))
               (parse-moves stream (append moves (list (pokemon::find-move (parse-move (read-line stream))))))
               moves)))
    (let ((nameline (read-line stream nil nil)))
      (cond ((string= "" nameline) (parse-pokemon-info stream))
            (nil nil)
            (t (values (parse-name nameline)
                       (parse-trait (read-line stream))
                       (parse-evs (read-line stream))
                       (parse-nature (read-line stream))
                       (parse-moves stream)))))))

(defparameter +importable-teams+
  (list
   "Axew (M) @ Potion
Trait: Mold Breaker
EVs: 32 HP / 60 Atk / 196 SAtk / 220 Spd
Naive Nature (+Spd, -SDef)
- Dragon Pulse
- Strength
- Aerial Ace

Totodile (M) @ Potion
Trait: Sheer Force
EVs: 255 Atk / 8 Def / 244 SAtk
Quiet Nature (+SAtk, -Spd)
- Water Pledge
- Aerial Ace

Nincada (M) @ Potion
Trait: Run Away
EVs: 188 HP / 236 Atk / 36 Def / 48 Spd
Adamant Nature (+Atk, -SAtk)
- Aerial Ace
- Bug Bite
- Faint Attack

Magnemite @ Potion
Trait: Sturdy
EVs: 156 HP / 196 Def / 156 SAtk
Modest Nature (+SAtk, -Atk)
- Magnet Bomb
- Electro Ball

Nosepass (M) @ Potion
Trait: Sand Force
EVs: 196 HP / 236 SAtk / 36 SDef / 40 Spd
Modest Nature (+SAtk, -Atk)
- Power Gem
- Strength

Skorupi (M) @ Potion
Trait: Keen Eye
EVs: 36 HP / 196 Atk / 40 Def / 236 Spd
Jolly Nature (+Spd, -SAtk)
- Aerial Ace
- Faint Attack
- Bug Bite"

   "Spearow (M) @ (No Item)
Trait: Keen Eye
EVs: 255 Atk / 196 Spd
Naive Nature (+Spd, -SDef)
- Aerial Ace
- Faint Attack

Axew (F) @ (No Item)
Trait: Mold Breaker
EVs: 255 SAtk / 224 Spd
Modest Nature (+SAtk, -Atk)
- Dragon Pulse
- Strength
- Aerial Ace

Magnemite @ (No Item)
Trait: Sturdy
EVs: 196 Def / 156 SAtk / 156 Spd
Timid Nature (+Spd, -Atk)
- Electro Ball
- Magnet Bomb

Croagunk (M) @ (No Item)
Trait: Anticipation
EVs: 192 Atk / 40 Def / 36 SDef / 120 Spd
Lonely Nature (+Atk, -Def)
- Wake-Up Slap
- Strength
- Faint Attack
- Venoshock

Spoink (M) @ (No Item)
Trait: Own Tempo
EVs: 140 HP / 164 Def / 144 SAtk / 60 Spd
Modest Nature (+SAtk, -Atk)
- Psyshock
- Power Gem

Teddiursa (M) @ (No Item)
Trait: Pickup
EVs: 172 HP / 120 Atk
Brave Nature (+Atk, -Spd)
- Strength
- Faint Attack
- Aerial Ace"

   "Spoink (M) @ Potion
Trait: Own Tempo
EVs: 196 SAtk / 116 SDef / 196 Spd
Timid Nature (+Spd, -Atk)
- Psyshock
- Power Gem

Axew (M) @ Potion
Trait: Unnerve
EVs: 92 Atk / 196 SAtk / 220 Spd
Hasty Nature (+Spd, -Def)
- Dragon Pulse
- Aerial Ace
- Strength

Nincada (M) @ Potion
Trait: Run Away
EVs: 28 HP / 236 Atk / 36 Def / 196 Spd
Adamant Nature (+Atk, -SAtk)
- Aerial Ace
- Bug Bite
- Faint Attack

Cubchoo (M) @ Potion
Trait: Snow Cloak
EVs: 76 HP / 40 Atk / 196 SAtk / 196 SDef
Mild Nature (+SAtk, -Def)
- Frost Breath
- Strength
- Aerial Ace

Teddiursa (M) @ Potion
Trait: Honey Gather
EVs: 196 HP / 196 Atk / 116 Spd
Adamant Nature (+Atk, -SAtk)
- Aerial Ace
- Strength
- Faint Attack

Magnemite @ Potion
Trait: Sturdy
EVs: 36 Def / 236 SAtk / 236 Spd
Timid Nature (+Spd, -Atk)
- Magnet Bomb
- Electro Ball"

"Cubchoo (M) @ (No Item)
Trait: Snow Cloak
EVs: 156 HP / 196 SAtk / 40 SDef / 116 Spd
Modest Nature (+SAtk, -Atk)
- Frost Breath
- Strength
- Aerial Ace

Spoink (M) @ (No Item)
Trait: Own Tempo
EVs: 116 HP / 196 SAtk / 196 Spd
Timid Nature (+Spd, -Atk)
- Power Gem
- Psyshock

Nosepass (M) @ (No Item)
Trait: Sand Force
EVs: 196 HP / 236 SAtk / 40 SDef / 36 Spd
Modest Nature (+SAtk, -Atk)
- Power Gem
- Strength

Golett @ (No Item)
Trait: Klutz
EVs: 204 HP / 244 Atk / 60 Def
Adamant Nature (+Atk, -SAtk)
- Strength
- Shadow Punch

Croagunk (M) @ (No Item)
Trait: Anticipation
EVs: 52 HP / 188 Atk / 36 Def / 36 SDef / 196 Spd
Jolly Nature (+Spd, -SAtk)
- Faint Attack
- Wake-Up Slap
- Venoshock
- Strength

Purrloin (M) @ (No Item)
Trait: Limber
EVs: 28 HP / 196 Atk / 56 SAtk / 228 Spd
Jolly Nature (+Spd, -SAtk)
- Aerial Ace
- Faint Attack"

   "Spoink (M) @ (No Item)
Trait: Own Tempo
EVs: 76 Def / 196 SAtk / 40 SDef / 196 Spd
Timid Nature (+Spd, -Atk)
- Psyshock
- Power Gem

Magnemite @ (No Item)
Trait: Sturdy
EVs: 156 Atk / 116 Def / 236 SAtk
Quiet Nature (+SAtk, -Spd)
- Electro Ball
- Magnet Bomb

Axew (M) @ (No Item)
Trait: Mold Breaker
EVs: 92 Atk / 196 SAtk / 220 Spd
Naive Nature (+Spd, -SDef)
- Aerial Ace
- Strength
- Dragon Pulse

Nosepass (M) @ (No Item)
Trait: Sand Force
EVs: 196 HP / 76 Def / 236 SAtk
Modest Nature (+SAtk, -Atk)
- Power Gem

Skorupi (M) @ (No Item)
Trait: Keen Eye
EVs: 196 Atk / 236 Spd
Jolly Nature (+Spd, -SAtk)
- Bug Bite
- Aerial Ace
- Strength
- Faint Attack

Nincada (M) @ (No Item)
Trait: Compoundeyes
EVs: 76 HP / 236 Atk / 196 Def
Adamant Nature (+Atk, -SAtk)
- Bug Bite
- Aerial Ace
- Faint Attack"))
(defun test-pkminfo ()
  (with-input-from-string (s (first +importable-teams+))
    (list (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s)))))

(defun random-pkminfo ()
  (with-input-from-string (s (alexandria:random-elt +importable-teams+))
    (list (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s)))))

