;;; ASDF system definition for shania a pokemon AI system.

;;; TODO Actually write the system definition. Will do as enough source is
;;; written to mandate using multiple source files.

(asdf:defsystem :shanai
  :depends-on (:eos :hunchentoot :alexandria :split-sequence :iterate :cl-who
                    :PARSE-DECLARATIONS-1.0 :ironclad
                    :cl-ppcre :drakma :cl-json :parenscript
                    :shanai.define-user-command
                    :shanai.binary-data :flexi-streams)
  :components
  ((:file "packages")
   (:file "config" :depends-on ("packages"))
   (:file "team" :depends-on ("packages"))
   (:file "generic" :depends-on ("packages"))
   (:module "PO"
            :depends-on ("packages" "generic" "Pokemon" "po-client")
            :components
            ((:module "Client"
                      :depends-on ("connection" "po-battle")
                      :components
                      ((:file "client" :depends-on ())))
             (:file "trainer" :depends-on ())
             (:file "po-battle" :depends-on ("trainer"))
             (:file "import-po-pokemon-list")
             (:file "connection")))
   (:module "Pokemon"
            :depends-on ("packages")
            :components
            ((:file "ability")
             (:file "move")
             (:file "type")
             (:file "pokemon")
             (:file "basic-pokemon")))
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
   (:file "po-data-import" :depends-on ("Pokemon"))
   (:file "pokedex" :depends-on ("Pokemon" "po-data-import" "config"))
   (:file "po-client" :depends-on ("packages" "generic" "PO" "Pokemon"))
   (:file "scratch" :depends-on ("packages" "generic" "PO" "Pokemon" "po-client"))
   (:file "movedex" :depends-on ("Pokemon" "po-data-import"))
   (:file "handle-command" :depends-on ("po-client" "Pokemon" "movedex"))
   (:file "shanai-simple-init" :depends-on ("movedex" "pokedex"))
   (:file "www" :depends-on ("Pokemon" "movedex" "pokedex" "po-client"))))