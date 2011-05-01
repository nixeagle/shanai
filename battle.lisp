(in-package #:shanai.battle)

(defclass basic-battle ()
  ((challenger :initarg :challenger
               :accessor battle-challenger)
   (challenged :initarg :challenged
               :accessor battle-challenged)
   (tier :initarg :tier :reader battle-tier
         :type 'string)
   (gen :initarg :gen :reader battle-gen)))


