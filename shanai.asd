;;; ASDF system definition for shania a pokemon AI system.

;;; TODO Actually write the system definition. Will do as enough source is
;;; written to mandate using multiple source files.

(asdf:defsystem :shanai
  :depends-on (:eos :hunchentoot :alexandria :split-sequence :iterate :cl-who
                    :PARSE-DECLARATIONS-1.0 :ironclad
                    :cl-ppcre :drakma :cl-json :parenscript
                    :shanai.define-user-command
                    :shanai.util :postmodern
                    :shanai.binary-data :flexi-streams)
  :components
  ((:file "packages")
   (:file "global" :depends-on ("packages"))
   (:file "config" :depends-on ("packages"))
   (:file "team" :depends-on ("packages"))
   (:file "generic" :depends-on ("packages"))
   (:module "PO"
            :depends-on ("packages" "generic" "Pokemon" "global" "message")
            :components
            ((:module "Client"
                      :depends-on ("connection" "po-battle" "protocol-classes")
                      :components
                      ((:file "client" :depends-on ())))
             (:file "trainer" :depends-on ())
             (:file "po-battle" :depends-on ("trainer"))
             (:file "import-po-pokemon-list")
             (:file "protocol-classes")
             (:file "connection")))
   (:module "Pokemon"
            :depends-on ("packages")
            :components
            ((:file "ability")
             (:file "move")
             (:file "type")
             (:file "pokemon")
             (:file "basic-pokemon")))
   (:module "Rpg"
            :depends-on ("packages" "generic")
            :components
            ((:file "rpg-cmd")
             (:file "rpg-global")
             (:file "rpg-database")))
   (:module "Commands"
            :depends-on ("packages" "Pokemon" "PO" "handle-command")
            :components
            ((:file "cmd-challenge-me")
             (:file "cmd-cmdtest")
             (:file "cmd-movedex")
             (:file "cmd-pokedex")
             (:file "cmd-homepage")
             (:file "cmd-shanai-cup")
             (:file "cmd-username-whitelist")
             (:file "cmd-vote")
             (:file "cmd-who")
             (:file "user-warn-patterns")))
   (:file "battle" :depends-on ("team" "generic"))
   (:file "po-data-import" :depends-on ("Pokemon" "config"))
   (:file "pokedex" :depends-on ("Pokemon" "po-data-import" "config"))
   (:file "po-client" :depends-on ("packages" "generic" "PO" "Pokemon" "message"))
   (:file "scratch" :depends-on ("packages" "generic" "PO" "Pokemon" "po-client"))
   (:file "movedex" :depends-on ("Pokemon" "po-data-import"))
   (:file "handle-command" :depends-on ("po-client" "Pokemon" "movedex" "Rpg"
                                                    "google-translate"))
   (:file "shanai-simple-init" :depends-on ("movedex" "pokedex"))
   (:file "google-translate" :depends-on ("packages" "config"))
   (:file "message" :depends-on ("generic"))
   (:file "user" :depends-on ("po-client"))
   (:file "www" :depends-on ("Pokemon" "movedex" "pokedex" "po-client"))))