(in-package :shanai.formulas)

(flet ((level (lvl)
         "Adjustments to the LVL to scale it down a bit."
         (+ (floor (* 2/5 lvl)) 2))
       (f (expr)
         "Shorthand for flooring expr"
         (floor expr)))
  (defun damage-formula (lvl bp atk def stab type1 type2 mod1 mod2 mod3 random crit)
    (f (* (f (* (f (* (f (* (f (* (f (+ (f (* (f (/ (f (* (level lvl) bp atk 1/50))
                                                    def))
                                              mod1))
                                        (f (* 2 crit mod2))))
                                  random 1/100))
                            stab))
                      type1))
                type2))
           mod3))))



(defun damage-formula-smooth (lvl bp atk def stab type1 type2 mod1 mod2 mod3 random crit)
  "Version of the damage formula that does not FLOOR."
  (* (+ (* (/ (* (+ (* 2/5 lvl) 2) bp atk 1/50) def) mod1)
        (* 2 crit mod2))
     1/100 random stab type1 type2 mod3))


(defun damage-possible-range (lvl bp atk def
                              &key (stab 1) (type1 1) (type2 1)
                              (mod1 1) (mod2 1) (mod3 1) (crit 1))
  "List the 16 possible sets of lost hitpoints.

Since the random number generator in pokemon games can only range from 85
to 100 in battles, we generate a list covering each possible amount of HP
lost in a given attack."
  (loop for r from 85 to 100
       collect (damage-formula lvl bp atk def stab type1 type2
                               mod1 mod2 mod3 r crit)))

(defun hp-formula-gen-4 (level base ev &optional (iv 31))
  "Health point formula for pokemon generations 3, 4 and 5."
  (declare (type (integer 0 31) iv)
           (type (integer 0 255) base ev)
           (type (integer 1 100) level)
           (optimize (speed 3) (compilation-speed 0)
                     (debug 0) (safety 0)))
  (+ (floor (* (+ iv (* 2 base) (floor ev 4) 100) level)
            100)
     10))

(defun hp-formula-gen-4-by-evs (level base &optional (iv 31))
  "List all possible HP values the opponent might have.

In total there are 64 possible HP values depending on the pokemon's LEVEL
and EV allocation."
  (loop for ev from 0 to 255 by 4
     collect (hp-formula-gen-4 level base ev iv)))

(defun other-stat-formula-gen-4 (level base ev &optional (iv 31))
  "Formula for non HP stat values in pokemon generations 3, 4 and 5."
  (+ (floor (* (+ iv (* 2 base) (floor ev 4)) level)
            100)
     5))

(defun other-stat-formula-gen-4-by-evs (level base &optional (iv 31))
  "List possible stat values the opponent may have.

In total there are 64 possible values depending on the pokemon's LEVEL and
EV allocation.

Remember that pokemon natures add two more sets of 64 HP values. But the
nature modifier is applied after evs are applied to the BASE stat. So in
total there are 192 different possible values. A large majority of these
will overlap."
  (loop for ev from 0 to 255 by 4
     collect (other-stat-formula-gen-4 level base ev iv)))

(defun compute-possible-move-damage (level base percent)
  (loop for hp in (hp-formula-gen-4-by-evs level base)
     collect (cons hp (* hp (/ percent 100)))))