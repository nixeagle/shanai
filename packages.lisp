(defpackage #:pokemon
  (:use :cl)
  (:import-from :eos #:is #:test)
  (:import-from :alexandria :non-negative-fixnum)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :iterate :iter :for :appending :collecting :generate :generating :next))

(defpackage #:pokemon.po.client
  (:import-from :split-sequence #:split-sequence)
  (:import-from :cl-who :str :esc))


(defpackage #:shanai.po.client
  (:import-from :split-sequence #:split-sequence))

(defpackage #:shanai.po.bot)

(defpackage #:shanai.pokemon
   (:use :cl)
   (:import-from :eos #:is #:test)
   (:import-from :alexandria :non-negative-fixnum)
   (:import-from :split-sequence #:split-sequence)
   (:import-from :iterate :iter :for :appending :collecting :generate :generating :next))