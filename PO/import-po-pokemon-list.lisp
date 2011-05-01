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

(defun test-pkminfo ()
  (with-input-from-string (s "Axew (M) @ Potion
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
- Bug Bite")
    (list (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s)))))
