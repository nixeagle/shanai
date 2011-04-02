;;; ASDF system definition for shania a pokemon AI system.

;;; TODO Actually write the system definition. Will do as enough source is
;;; written to mandate using multiple source files.

(asdf:defsystem :shanai
  :depends-on (:eos :hunchentoot :alexandria :split-sequence :iterate :cl-who)
  :components
  ((:file "packages")
   (:module "Pokemon"
            :components
            ((:file "ability")
             (:file "move")
             (:file "pokemon")))
   (:file "po-data-import" :depends-on ("Pokemon"))
   (:file "pokedex" :depends-on ("Pokemon" "po-data-import"))
   (:file "po-client" :depends-on ("packages"))
   (:file "movedex" :depends-on ("Pokemon" "po-data-import"))
   (:file "handle-command" :depends-on ("po-client" "Pokemon"))))