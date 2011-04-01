;;; ASDF system definition for shania a pokemon AI system.

;;; TODO Actually write the system definition. Will do as enough source is
;;; written to mandate using multiple source files.

(asdf:defsystem :shania
  :depends-on (:eos :hunchentoot :alexandria :split-sequence :iterate :cl-who)
  :components ((:file "pokemon")
               (:file "po-data-import" :depends-on ("pokemon"))
               (:file "pokedex" :depends-on ("pokemon" "po-data-import"))
               (:file "po-client")
               (:file "movedex" :depends-on ("pokemon" "po-data-import"))
               (:file "handle-command" :depends-on ("po-client" "pokemon"))))