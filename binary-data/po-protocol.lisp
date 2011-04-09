;;; needs to be in some package...
(defpackage #:shanai.po.protocol
  (:use :cl :shanai.binary-data))

(in-package :shanai.po.protocol)

(define-binary-class color ()
  ((spec u1)
   (alpha u2)
   (red u2)
   (green u2)
   (blue u2)
   (padding u2)))

(define-binary-class pokemon-unique-id ()
  ((pokemon-id u2)
   (pokemon-subid u1)))

(define-binary-class challenge-info ()
  ((dsc u1)
   (opponent u4)
   (clauses u4)
   (mode u1)))

(define-binary-class player-info ()
  ((id s4)
   (name utf16-qt-string)
   (info utf16-qt-string)
   (auth u1)
   (flags u1)
   (rating s2)
   (poke1 pokemon-unique-id)
   (poke2 pokemon-unique-id)
   (poke3 pokemon-unique-id)
   (poke4 pokemon-unique-id)
   (poke5 pokemon-unique-id)
   (poke6 pokemon-unique-id)
   (avatar s2)
   (tier utf16-qt-string)
   (color color)
   (gen u1)))

(define-binary-class version-control ()
  ((version utf16-qt-string)))

(define-binary-class server-name ()
  ((name utf16-qt-string)))

(define-binary-class trainer-team ()
  ((name utf16-qt-string)
   (info utf16-qt-string)
   (lose-msg utf16-qt-string)
   (win-msg utf16-qt-string)
   (avatar utf16-qt-string)
   (default-tier utf16-qt-string)
   (team utf16-qt-string)))

(define-binary-class poke-personal
    ((pokemon-id pokemon-unique-id)))
(define-binary-type pokestats ()
  (:reader (in)
           (pokemon::battle-stats (read-value 'u1 in)
                                  (read-value 'u1 in)
                                  (read-value 'u1 in)
                                  (read-value 'u1 in)
                                  (read-value 'u1 in)
                                  (read-value 'u1 in)))
  (:writer (out value)
           (write-value 'u1 out (pokemon::hp value))
           (write-value 'u1 out (pokemon::attack value))
           (write-value 'u1 out (pokemon::defense value))
           (write-value 'u1 out (pokemon::special-attack value))
           (write-value 'u1 out (pokemon::special-defense value))
           (write-value 'u1 out (pokemon::speed value))))