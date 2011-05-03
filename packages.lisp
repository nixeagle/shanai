(defpackage #:shanai.config
  (:use :cl)
  (:documentation "global project configuration... of some sort.")
  (:nicknames :conf)
  (:export #:po-root-directory))

(defpackage #:shanai.global
  (:use :cl)
  (:documentation "Global variables and functions to manipulate them."))
(defpackage #:shanai.generic
  (:use :cl)
  (:nicknames :generic)
  (:documentation "Supplies generic functions used in the whole project.

These functions all must be implemented by various other packages.")
  (:export #:name
           #:object-id
           #:challenger
           #:challenged
           #:tier
           #:gen
           #:type1
           #:type2
           #:forme-id))

(defpackage #:shanai.pokemon
  (:export :stats
           #:make-stats
           #:statp
           #:stats-hp
           #:stats-atk
           #:stats-def
           #:stats-satk
           #:stats-sdef
           #:stats-spd

           :battle-pokemon
           #:pokemon-id
           #:pokemon-forme
           #:pokemon-name
           #:pokemon-type
           #:pokemon-weight
           #:pokemon-base-stats
           #:pokemon-height
           #:pokemon-gen
           #:pokemon-gender
           #:pokemon-level
           #:pokemon-nickname
           #:pokemon-shiny-p
           #:pokemon-ivs
           #:pokemon-current-hp
           #:pokemon-evs
           #:pokemon-item
           #:pokemon-ability
           #:pokemon-happiness
           #:pokemon-moves)
  (:use :cl)
  #+ ()   (:import-from :eos #:is #:test)
  #+ ()   (:import-from :alexandria :non-negative-fixnum)
  #+ ()   (:import-from :split-sequence #:split-sequence)
  #+ ()   (:import-from :iterate :iter :for :appending :collecting :generate :generating :next))
(defpackage #:shanai.po.client
  (:use :cl
        :binary-data)
  (:import-from :split-sequence #:split-sequence)
  (:nicknames :po-client)
  (:export #:trainer-id
           #:get-channel
           #:get-trainer
           #:channel-id
           #:channel-name))

(defpackage #:shanai.po.bot.user-warn-patterns
  (:use :cl)
  (:export #:whitelisted-username-list
           #:blacklisted-username-pattern-list
           #:whitelist-username
           ))

(defpackage #:shanai.po.bot.vote
  (:use :cl)
  (:export #:getpoll
           #:valid-poll-id-p
           #:basic-poll-title
           #:addpoll
           #:cast-basic-vote
           #:parse-yes-or-no-p
           #:tally-poll)
  (:import-from :shanai.po.client #:trainer-id))

(defpackage #:pokemon
  (:use :cl)
  (:import-from :eos #:is #:test)
  (:import-from :alexandria :non-negative-fixnum)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :iterate :iter :for :appending :collecting :generate :generating :next))

(defpackage #:pokemon.po.client
  (:use :cl :ccl :shanai.pokemon)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :cl-who :str :esc))




(defpackage #:shanai.po.protocol
  (:use :cl :binary-data)
  (:nicknames :po-proto)
  (:export #:write-join-channel
           #:read-qtstring
           #:write-leave-channel
           #:write-channel-message
           #:write-challenge-stuff))
(defpackage #:shanai.po.bot
  (:use :cl :shanai.define-user-command :shanai.generic))

(defpackage #:shanai.pokedex
  (:use :cl))

(defpackage #:shanai.po.db
  (:use :cl)
  (:documentation "Read and write Pokemon Online data files.

These files are stored in bin/db/ and containing subdirectories of the
Pokemon Online git repository source."))





(defpackage #:shanai.pokemon.type
  (:use :cl)
  (:import-from :alexandria #:alist-hash-table)
  (:export #:typeid))


(defpackage #:shanai.www
  (:use :cl))

(defpackage #:shanai.battle
  (:use :cl :shanai.pokemon)
  (:export :basic-battle
           #:battle-challenger
           #:battle-challenged))

(defpackage #:shanai.team
  (:use :cl))

(defpackage #:shanai.po.battle
  (:use :cl :shanai.battle
        :shanai.pokemon)
  (:export :battle
           #:battle-id
           #:battle-spectators
           #:battle-spectating-p))