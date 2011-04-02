;;; In battle weather conditions.
;;; Not 100% positive we will be using these classes.

(in-package :pokemon)

(defclass weather () ())

(defclass sunlight (weather) ())
(defclass rain (weather) ())
(defclass sandstorm (weather) ())
(defclass hailstorm (weather) ())
(defclass shadow-sky (weather) ())
(defclass fog (weather) ())
(defclass cloudy-sky (weather) ())
(defclass snow (weather) ())
