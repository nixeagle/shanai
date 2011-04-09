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

(defpackage #:shanai.pokedex
  (:use :cl))

(defpackage #:shanai.po.db
  (:use :cl)
  (:documentation "Read and write Pokemon Online data files.

These files are stored in bin/db/ and containing subdirectories of the
Pokemon Online git repository source."))


(defpackage #:shanai.pokemon
   (:use :cl)
   (:import-from :eos #:is #:test)
   (:import-from :alexandria :non-negative-fixnum)
   (:import-from :split-sequence #:split-sequence)
   (:import-from :iterate :iter :for :appending :collecting :generate :generating :next))