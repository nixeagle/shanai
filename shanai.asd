;;; ASDF system definition for shania a pokemon AI system.

;;; TODO Actually write the system definition. Will do as enough source is
;;; written to mandate using multiple source files.

(asdf:defsystem :shanai
  :depends-on (:eos :hunchentoot :alexandria :split-sequence :iterate :cl-who
                    :shanai.define-user-command :shanai.binary-data :flexi-streams )
  :components
  ((:file "packages")
   (:module "Po"
            :depends-on ("packages")
            :components
            ((:module "Client"
                      :depends-on ("packages" "connection")
                      :components
                      ((:file "trainer" :depends-on ())
                       (:file "client" :depends-on ("generic" "po-battle"))))
             (:file "po-battle" :depends-on ("generic" "trainer"))
             (:file "connection")))
   (:module "Pokemon"
            :depends-on ("packages")
            :components
            ((:file "ability")
             (:file "move")
             (:file "pokemon")))
   (:module "Commands"
            :depends-on ("packages")
            :components
            ((:file "cmd-challenge-me")
             (:file "cmd-cmdtest")
             (:file "cmd-movedex")
             (:file "cmd-pokedex")
             (:file "cmd-username-whitelist")
             (:file "cmd-vote")
             (:file "cmd-who")
             (:file "user-warn-patterns")))
   (:file "team" :depends-on ("packages"))
   (:file "generic" :depends-on ("packages"))
   (:file "battle" :depends-on ("team" "trainer" "generic"))
   
   (:file "po-data-import" :depends-on ("Pokemon"))
   (:file "pokedex" :depends-on ("Pokemon" "po-data-import"))
   (:file "po-client" :depends-on ("packages" "generic" "connection" "client" "Pokemon"))
   (:file "movedex" :depends-on ("Pokemon" "po-data-import"))
   (:file "handle-command" :depends-on ("po-client" "Pokemon"))))