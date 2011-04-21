(in-package :shanai.pokemon.type)

(defconstant +valid-type-keywords+
  #(:normal :fighting :flying :poison :ground :rock :bug
    :ghost  :steel :fire :water :grass :electric
    :psychic :ice :dragon :dark :???))

(deftype pokemon-type-keyword ()
  `(member ,@(loop for i across +valid-type-keywords+ collect i)))

(defconstant +total-valid-types+ (length +valid-type-keywords+))

(deftype pokemon-type-id ()
  "Total range of pokemon type ids."
  `(mod ,+total-valid-types+))

(defun typeid (type-symbol)
  "Return corresponding enumeration id for TYPE-SYMBOL."
  (declare (type pokemon-type-keyword type-symbol)
           (optimize (speed 3) (debug 0) (safety 0)))
  (the pokemon-type-id (position type-symbol +valid-type-keywords+)))

;;; Type damage array. 0 1 2 4 are the 4 valid values. Result must be
;;; divided by 2 at some later point.
(defconstant +type-table+
  (make-array '(18 18) :element-type '(integer 0 4) :initial-contents
              '((2 2 2 2 2 1 2 0 1 2 2 2 2 2 2 2 2 2)
                (4 2 1 1 2 4 1 0 4 2 2 2 2 1 4 2 4 2)
                (2 4 2 2 2 1 4 2 1 2 2 4 1 2 2 2 2 2)
                (2 2 2 1 1 1 2 1 0 2 2 4 2 2 2 2 2 2)
                (2 2 0 4 2 4 1 2 4 4 2 1 4 2 2 2 2 2)
                (2 1 4 2 1 2 4 2 1 4 2 2 2 2 4 2 2 2)
                (2 1 1 1 2 2 2 1 1 1 2 4 2 4 2 2 4 2)
                (0 2 2 2 2 2 2 4 1 2 2 2 2 4 2 2 1 2)
                (2 2 2 2 2 4 2 2 1 1 1 2 1 2 4 2 2 2)
                (2 2 2 2 2 1 4 2 4 1 1 4 2 2 4 1 2 2)
                (2 2 2 2 4 4 2 2 2 4 1 1 2 2 2 1 2 2)
                (2 2 1 1 4 4 1 2 1 1 4 1 2 2 2 1 2 2)
                (2 2 4 2 0 2 2 2 2 2 4 1 1 2 2 1 2 2)
                (2 4 2 4 2 2 2 2 1 2 2 2 2 1 2 2 0 2)
                (2 2 4 2 4 2 2 2 1 1 1 4 2 2 1 4 2 2)
                (2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 4 2 2)
                (2 1 2 2 2 2 2 4 1 2 2 2 2 4 2 2 1 2)
                (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2))))

(defparameter +pokemon-type-name-dictionary+
  (alist-hash-table
   '(("fire" . :fire)
     ("water" . :water)
     ("poison" . :poison)
     ("???" . :???)
     ("fighting" . :fighting)
     ("fight" . :fighting)
     ("normal" . :normal)
     ("flying" . :flying)
     ("fly" . :flying)
     ("pioson" . :poison)
     ("ground" . :ground)
     ("rock" . :rock)
     ("bug" . :bug)
     ("ghost" . :ghost)
     ("steel" . :steel)
     ("steal" . :steel)
     ("grass" . :grass)
     ("electric" . :electric)
     ("psychic" . :psychic)
     ("psy" . :psychic)
     ("ice" . :ice)
     ("dragon" . :dragon)
     ("dark" . :dark))
   :test #'equalp :lock-free t))

(defun %type-matchup (attacking defending)
  (declare (type pokemon-type-id attacking defending))
  (the (integer 0 4) (aref +type-table+ attacking defending)))

(defun type-matchup (attacking defending &optional (defending2 :???))
  (declare (type pokemon-type-keyword attacking defending defending2))
  (let ((atk-id (position attacking +valid-type-keywords+))
        (def-id (position defending +valid-type-keywords+))
        (def-id2 (position defending2 +valid-type-keywords+)))
    (the (integer 0 16) (* (%type-matchup atk-id def-id) (%type-matchup atk-id def-id2)))))

