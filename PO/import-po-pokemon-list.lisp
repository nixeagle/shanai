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
  (with-input-from-string (s "Axew (M) @ (No Item)
Trait: Rivalry
EVs: 252 HP / 96 SAtk / 160 SDef
Quiet Nature (+SAtk, -Spd)
- Dragon Pulse
- Strength

Golett @ (No Item)
Trait: Iron Fist
EVs: 252 HP / 184 Def / 72 SDef
Bold Nature (+Def, -Atk)
- Shadow Punch
- Strength

Clefairy (M) @ (No Item)
Trait: Magic Guard
EVs: 252 HP / 120 Def / 136 SDef
Hardy Nature
- Strength

Electrike (M) @ (No Item)
Trait: Static
EVs: 252 SAtk / 252 Spd
Mild Nature (+SAtk, -Def)
- Electro Ball
- Strength

Roggenrola (M) @ (No Item)
Trait: Sturdy
EVs: 252 HP / 252 SDef
Sassy Nature (+SDef, -Spd)
- Smack Down
- Strength

Machop (M) @ (No Item)
Trait: Guts
EVs: 252 Atk / 252 Spd
Adamant Nature (+Atk, -SAtk)
- Brick Break
- Strength
- Smack Down")
    (list (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s))
          (multiple-value-list (parse-pokemon-info s)))))
