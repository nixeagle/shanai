;;;; Define all the important generic functions that most everything else
;;;; will want to use


(in-package #:shanai.generic)

(defgeneric name (thing)
  (:documentation "Return the name of THING as a string."))

(defgeneric object-id (thing)
  ;; Probably not useful in common lisp but who knows!
  (:documentation "Integer identifier of THING."))

(defgeneric challenger (thing)
  (:documentation "Player challenging."))

(defgeneric challenged (thing)
  (:documentation "Player challenged."))

(defgeneric gen (thing)
  (:documentation "Current generation of THING.

Usually refers to something in the range of 1 to 5 standing for the various
gamefreak pokemon generations currently known."))

(defgeneric tier (thing)
  (:documentation  "Returns the string name of THING's tier."))
(defgeneric type1 (thing))
(defgeneric type2 (thing))

(defgeneric forme-id (thing)
  (:documentation "ID of THING's forme."))


(defgeneric po-htmlize (thing &key &allow-other-keys))