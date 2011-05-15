(in-package :shanai.po.bot.zombie)

(defvar *game-state* nil)

(defclass game-state ()
  ((players :initarg :players :accessor game-players
            :initform ())
   (weapons :initarg :weapons :accessor game-weapons
            :initform (list (make-weapon "Crowbar" 10)
                            (make-weapon "Bat" 5)
                            (make-weapon "Wooden&nbsp;Ruler" 1)
                            (make-weapon "Brick" 1)))
   (baddies :initarg :baddies :accessor game-baddies
            :initform (list (make-zombie)))
   (areas :initarg :areas :accessor game-areas
          :initform (list (make-instance 'area :name "Alpha Mall"
                                         :descriptions
                                         (list "A once magnificent mall. Thriving with life and joy. But today, the air has a mysterious feel to it..."))
                          (make-instance 'area :name "Wishing Well Fountain"
                                         :descriptions
                                         (list "A soothing water fountain with sparkling water. A perfect place to start the carnage."
                                               "A once beautiful water fountain, where many children have made their wishes. It is now filled with corpses and a place of sadness."))))))

(defmethod generic:po-htmlize ((game game-state) &key)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:table
            (:tr
             (:td "Players")
             (:td (mapc (lambda (id)
                          (cl-who:htm
                           (:font :color (if (player-afkp id) "grey" "black")
                                  (print (who:escape-string (name id)))))
                          (princ " "))
                        (game-players game))))
            (:tr (:td "Player Count")
                 (:td (princ (length (game-players game)))))
            (:tr (:td "Weapons")
                 (:td :width "70%"(mapc (lambda (weapon)
                              (princ (po-htmlize weapon)))
                            (game-weapons game))))
            (:tr (:td "Zombies")
                 (:td (mapc (lambda (baddy)
                              (princ (po-htmlize baddy)))
                            (game-baddies game))))
            (:tr (:td "Areas")
                 (:td (mapc (lambda (area)
                              (princ (po-htmlize area)))
                            (game-areas game)))))))

(defmethod po-htmlize ((weapon weapon) &key)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:table
     (:tr (:td "Name") (:td (princ (name weapon))))
     (:tr (:td "Total&nbsp;Uses") (:td (princ (weapon-uses weapon)))))))
(defun make-game-state ()
  (make-instance 'game-state))
(defun zombie-game ()
  *game-state*)
(defun (setf zombie-game) (value)
  (setf *game-state* value))

(defclass player ()
  ((name :initarg :name :reader name)
   (id :initarg :id :reader object-id)
   (afkp :initarg :afkp :accessor player-afkp
         :type 'boolean :initform nil)))

(defun make-player (id name)
  (make-instance 'player :name name :id id))

(defclass weapon ()
  ((name :initarg :name :reader name)
   (uses :initarg :uses :accessor weapon-uses
         :documentation "Remaining number of times the weapon can be used
         before destroyed.")))

(defun make-weapon (name uses)
  (make-instance 'weapon :name name :uses uses))

(defclass zombie ()
  ((name :initarg :name :reader name
         :initform "Nameless")))

(defmethod po-htmlize ((zombie zombie) &key)
  (cl-who:with-html-output-to-string (*standard-output* nil :indent t)
    (:table :style "background-color: #c7b4c3;"
     (:tr :width "20em" (:td :width "20em" "Name")
          (:td (princ (name zombie)))))))

(defun make-zombie ()
  (make-instance 'zombie))


(defclass area ()
  ((name :initarg :name :accessor name :type 'string)
   (current-description :initarg :current-description
                        :accessor current-description
                        :type 'string
                        :initform "NONE (TODO!)")
   (descriptions :initarg :descriptions :accessor descriptions
                 :type 'list
                 :initform ())
   (map :initarg :map :accessor area-map
        :initform (make-instance 'map :tiles (make-tile-array 6 6)))))

(defclass map ()
  ((tiles :initarg :tiles :accessor map-tiles)))

(defmethod po-htmlize ((map map) &key)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:table :border 0 :cellpadding 4
            (loop for i from 0 to (1- (array-dimension (map-tiles map) 0))
               do (cl-who:htm
                   (:tr :align "right"
                        (loop for j from 0 to (1- (array-dimension (map-tiles map) 1))
                           do (princ (po-htmlize (aref (map-tiles map) i j))))))))))

(defclass tile ()
  ((coordinate :initarg :coordinate :accessor tile-coordinate
               :documentation "Location of the tile.")
   (items :initarg :items :accessor tile-items
          :documentation "List of items found in the tile."
          :initform () :type 'list)
   (players :initarg :players :accessor tile-players
            :documentation "List of players currently on tile."
            :initform () :type 'list)
   (baddies :initarg :baddies :accessor tile-baddies
            :documentation "List of baddies currently on tile."
            :initform () :type 'list)
   (actions :initarg :actions :accessor tile-actions
            :documentation "List of actions availible on tile."
            :initform () :type 'list)))
(defun format-tile-title-attr (tile)
  (declare (type tile tile))
  (format nil "~A
Players: ~A
Items: ~A
Zombies: ~A
Actions: ~A"
          (tile-coordinate tile)
          (length (tile-players tile))
          (length (tile-items tile))
          (length (tile-baddies tile))
          (length (tile-actions tile))))

(defmethod po-htmlize ((tile tile) &key)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:td :bgcolor "white" :width 30 :title (format-tile-title-attr tile)
         (princ " "))))

(defmethod initialize-instance :after ((area area) &key descriptions)
  (declare (type (cons string) descriptions))
  (when descriptions
    (setf (current-description area) (car descriptions))))
(defmethod po-htmlize ((area area) &key)
  (cl-who:with-html-output-to-string (*standard-output*)
    (:table
     (:tr (:td "Name")
          (:td (princ (name area))))
     (:tr (:td "Current&nbsp;Description")
          (:td (:i (princ (current-description area)))))
     (:tr (:td "map")
          (:td (princ (po-htmlize (area-map area))))))))


(defun zombie-command/afk (game player)
  "Mark PLAYER as afk.")